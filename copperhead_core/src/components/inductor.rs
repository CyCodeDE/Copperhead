/*
 * Copyright (c) 2026-2026 CyCode and the Copperhead contributors
 *
 * This file is part of Copperhead.
 *
 * Copperhead is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Copperhead is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Copperhead. If not, see <https://www.gnu.org/licenses/>.
 */
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use faer::{ColMut, ColRef, MatMut};
use std::collections::HashMap;
use num_traits::cast;
use crate::circuit::Circuit;
use crate::descriptor::Instantiable;
use crate::util::mna::{stamp_conductance, stamp_current_source};

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct InductorDef {
    pub inductance: f64,
    pub series_resistance: f64,
}

impl<T: CircuitScalar> Instantiable<T> for InductorDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let comp = Inductor::new(
            nodes[0],
            nodes[1],
            cast(self.inductance).expect("Failed to cast inductance"),
            cast(self.series_resistance).expect("Failed to cast series resistance"),
            dt,
        );
        circuit.add_component(comp);
    }
}

pub struct Inductor<T> {
    node_a: NodeId,
    node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    inductance: T,

    series_resistance: T,

    /// Discrete equivalent conductance
    conductance: T,

    /// Norton equivalent current source value (history state)
    eq_current: T,

    // BDF2 history states
    /// Inductor current at t[n-1]
    i_l_m1: T,
    /// Inductor current at t[n-2]
    i_l_m2: T
}

impl<T: CircuitScalar> Inductor<T> {
    pub fn new(a: NodeId, b: NodeId, l: T, r: T, dt: T) -> Self {
        let mut inductor = Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            inductance: l,
            series_resistance: r,
            conductance: T::zero(),
            eq_current: T::zero(),
            i_l_m1: T::zero(),
            i_l_m2: T::zero(),
        };

        inductor.update_conductance(dt);
        inductor
    }

    fn update_conductance(&mut self, dt: T) {
        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();

        let r_step = (three * self.inductance) / (two * dt);
        let total_imp = self.series_resistance + r_step;

        self.conductance = if total_imp.abs() < T::from(1e-12).unwrap() {
            T::from(1.0e12).unwrap()
        } else {
            T::one() / total_imp
        };
    }

    /// Helper to read voltage from solution vector.
    fn get_voltage_diff(&self, solution: &ColRef<T>) -> T {
        let v1 = if let Some(idx) = self.cached_idx_a {
            solution[idx]
        } else {
            T::zero()
        };

        let v2 = if let Some(idx) = self.cached_idx_b {
            solution[idx]
        } else {
            T::zero()
        };

        v1 - v2
    }
}

impl<T: CircuitScalar> Component<T> for Inductor<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::LinearDynamic
    }

    fn bake_indices(&mut self, ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = if self.node_a.0 == 0 {
            None
        } else {
            Some(node_map.get(&self.node_a).copied().unwrap())
        };
        self.cached_idx_b = if self.node_b.0 == 0 {
            None
        } else {
            Some(node_map.get(&self.node_b).copied().unwrap())
        };
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn auxiliary_row_count(&self) -> usize {
        0
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        let g = if ctx.is_dc_analysis {
            if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap() // Finite short approximation
            } else {
                T::one() / self.series_resistance
            }
        } else {
            self.conductance
        };

        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            g,
            0
        );
    }

    fn stamp_dynamic(
        &mut self,
        _prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
        if ctx.is_dc_analysis {
            return;
        }

        stamp_current_source(
            rhs,
            self.cached_idx_a,
            self.cached_idx_b,
            self.eq_current,
            0,
        );
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_new = self.get_voltage_diff(current_node_voltages);

        let i_new = if ctx.is_dc_analysis {
            let g_dc = if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap()
            } else {
                T::one() / self.series_resistance
            };
            v_new * g_dc
        } else {
            (v_new * self.conductance) + self.eq_current
        };

        if ctx.is_dc_analysis {
            // Steady-state assumption for initialization
            self.i_l_m1 = i_new;
            self.i_l_m2 = i_new;
        } else {
            // Shift history
            self.i_l_m2 = self.i_l_m1;
            self.i_l_m1 = i_new;
        }

        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        let r_step = (three * self.inductance) / (two * ctx.dt);

        let i_history_pure = ((four * self.i_l_m1) - self.i_l_m2) / three;

        let v_history = r_step * i_history_pure;

        // Convert Thevenin voltage to Norton equivalent current source
        self.eq_current = v_history * self.conductance;
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe {
                name: "Voltage".to_string(),
                unit: "V".to_string(),
            },
            ComponentProbe {
                name: "Current".to_string(),
                unit: "A".to_string(),
            },
            ComponentProbe {
                name: "Power".to_string(),
                unit: "W".to_string(),
            },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        let v_new = self.get_voltage_diff(node_voltages);

        let i_new = if ctx.is_dc_analysis {
            let g_dc = if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap()
            } else {
                T::one() / self.series_resistance
            };
            v_new * g_dc
        } else {
            (v_new * self.conductance) + self.eq_current
        };

        let p_new = v_new * i_new;

        out_observables[0] = v_new;
        out_observables[1] = i_new;
        out_observables[2] = p_new;
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_new = self.get_voltage_diff(node_voltages);

        let i_flow = if ctx.is_dc_analysis {
            let g_dc = if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap()
            } else {
                T::one() / self.series_resistance
            };
            v_new * g_dc
        } else {
            (v_new * self.conductance) + self.eq_current
        };

        out_currents[0] = i_flow;
        out_currents[1] = -i_flow;
    }

    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        match name {
            "inductance" => {
                self.inductance = value;
                self.update_conductance(ctx.dt);
                true
            }
            "resistance" | "esr" => {
                self.series_resistance = value;
                self.update_conductance(ctx.dt);
                true
            }
            _ => false,
        }
    }
}
