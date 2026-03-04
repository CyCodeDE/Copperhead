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
use crate::util::mna::{get_voltage_diff, stamp_conductance, stamp_current_source};
use faer::{ColMut, ColRef, MatMut};
use std::collections::HashMap;
use num_traits::cast;
use crate::circuit::Circuit;
use crate::descriptor::Instantiable;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CapacitorDef {
    pub capacitance: f64,
    pub esr: f64,
}

impl<T: CircuitScalar> Instantiable<T> for CapacitorDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let comp = Capacitor::new(
            nodes[0],
            nodes[1],
            cast(self.capacitance).expect("Failed to cast Capacitance"),
            cast(self.esr).expect("Failed to cast ESR"),
            dt,
        );
        circuit.add_component(comp);
    }
}

pub struct Capacitor<T: CircuitScalar> {
    node_a: NodeId,
    node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    /// The physical capacitance in Farads
    capacitance: T,

    /// Equivalent Series Resistance in Ohms
    esr: T,

    /// The discrete equivalent conductance: G = 2C / dt
    conductance: T,

    /// The equivalent current source value (history state)
    /// Represents I_eq in the Norton equivalent model
    eq_current: T,

    // BDF2 history states
    /// Internal capacitor voltage at t[n-1]
    v_c_m1: T,

    /// Internal capacitor voltage at t[n-2]
    v_c_m2: T,
}

impl<T: CircuitScalar> Capacitor<T> {
    /// Creates a new Capacitor.
    pub fn new(a: NodeId, b: NodeId, capacitance: T, esr: T, dt: T) -> Self {
        let mut cap = Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            capacitance,
            esr,
            conductance: T::zero(),
            eq_current: T::zero(),
            v_c_m1: T::zero(),
            v_c_m2: T::zero(),
        };

        cap.update_conductance(dt);
        cap
    }

    fn update_conductance(&mut self, dt: T) {
        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();

        let r_c = (two * dt) / (three * self.capacitance);

        self.conductance = T::one() / (self.esr + r_c);
    }
}

impl<T: CircuitScalar> Component<T> for Capacitor<T> {
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

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        if ctx.is_dc_analysis {
            return;
        }

        let g = self.conductance;

        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            g,
            0,
        );
    }

    fn stamp_dynamic(
        &mut self,
        _prev: &ColRef<T>,
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
        let v_terminal =
            get_voltage_diff(current_node_voltages, self.cached_idx_a, self.cached_idx_b);

        let i_through = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v_terminal * self.conductance) + self.eq_current
        };

        // Extract the voltage across the pure capacitor by subtracting the ESR voltage drop
        let v_c = if ctx.is_dc_analysis {
            v_terminal
        } else {
            v_terminal - (i_through * self.esr)
        };

        if ctx.is_dc_analysis {
            // Steady-state assumption for initialization
            self.v_c_m1 = v_c;
            self.v_c_m2 = v_c;
        } else {
            // Shift history
            self.v_c_m2 = self.v_c_m1;
            self.v_c_m1 = v_c;
        }

        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        let v_th_c = ((four * self.v_c_m1) - self.v_c_m2) / three;

        // Convert Thevenin voltage to Norton equivalent current source
        self.eq_current = -(v_th_c * self.conductance);
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
        let v = get_voltage_diff(node_voltages, self.cached_idx_a, self.cached_idx_b);

        let i = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v * self.conductance) + self.eq_current
        };

        let p = v * i;

        out_observables[0] = v;
        out_observables[1] = i;
        out_observables[2] = p;
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v = get_voltage_diff(node_voltages, self.cached_idx_a, self.cached_idx_b);

        let i_flow = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v * self.conductance) + self.eq_current
        };

        out_currents[0] = i_flow; // Current flowing INTO Node A
        out_currents[1] = -i_flow; // Current flowing INTO Node B is
    }

    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        match name {
            "capacitance" => {
                self.capacitance = value;
                self.update_conductance(ctx.dt);
                true
            }
            "esr" => {
                self.esr = value;
                self.update_conductance(ctx.dt);
                true
            }
            _ => false,
        }
    }
}
