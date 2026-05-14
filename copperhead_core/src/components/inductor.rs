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
use crate::circuit::Circuit;
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::parameter::{resolve_param_value, ComponentEvalCtx, ParamValue, RebuildKind};
use crate::util::deserialize_number_or_string;
use crate::util::mna::{stamp_conductance, stamp_current_source};
use faer::{ColMut, ColRef, MatMut};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct InductorDef {
    #[serde(deserialize_with = "deserialize_number_or_string")]
    pub inductance: String,
    #[serde(deserialize_with = "deserialize_number_or_string")]
    pub series_resistance: String,
}

impl InductorDef {
    pub fn new(inductance: f64, series_resistance: f64) -> Self {
        Self {
            inductance: inductance.to_string(),
            series_resistance: series_resistance.to_string(),
        }
    }
}

impl<T: CircuitScalar> Instantiable<T> for InductorDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let lpv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(&self.inductance, ps))
            .unwrap_or_else(|| {
                ParamValue::Constant(self.inductance.trim().parse::<f64>().unwrap_or(0.0))
            });
        let rpv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(&self.series_resistance, ps))
            .unwrap_or_else(|| {
                ParamValue::Constant(self.series_resistance.trim().parse::<f64>().unwrap_or(0.0))
            });
        circuit.add_component(Inductor::new(nodes[0], nodes[1], lpv, rpv, dt));
    }
}

pub struct Inductor<T> {
    node_a: NodeId,
    node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    inductance: ParamValue,
    series_resistance: ParamValue,

    /// Cached T-typed value of inductance, updated by refresh methods.
    cached_inductance: T,
    /// Cached T-typed value of series resistance, updated by refresh methods.
    cached_series_resistance: T,

    /// Discrete equivalent conductance derived from inductance, series_resistance, and dt.
    conductance: T,

    /// Norton equivalent current source value (history state)
    eq_current: T,

    // BDF2 history states
    /// Inductor current at t[n-1]
    i_l_m1: T,
    /// Inductor current at t[n-2]
    i_l_m2: T,
}

impl<T: CircuitScalar> Inductor<T> {
    pub fn new(a: NodeId, b: NodeId, inductance: ParamValue, series_resistance: ParamValue, dt: T) -> Self {
        let l0 = inductance.as_constant().unwrap_or(1e-6);
        let r0 = series_resistance.as_constant().unwrap_or(0.0);

        let mut ind = Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            inductance,
            series_resistance,
            cached_inductance: T::from(l0).unwrap(),
            cached_series_resistance: T::from(r0).unwrap(),
            conductance: T::zero(),
            eq_current: T::zero(),
            i_l_m1: T::zero(),
            i_l_m2: T::zero(),
        };
        ind.update_conductance(dt);
        ind
    }

    fn update_conductance(&mut self, dt: T) {
        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();

        let r_step = (three * self.cached_inductance) / (two * dt);
        let total_imp = self.cached_series_resistance + r_step;

        self.conductance = if total_imp.abs() < T::from(1e-12).unwrap() {
            T::from(1.0e12).unwrap()
        } else {
            T::one() / total_imp
        };
    }

    fn get_voltage_diff(&self, solution: &ColRef<T>) -> T {
        let v1 = self.cached_idx_a.map(|i| solution[i]).unwrap_or(T::zero());
        let v2 = self.cached_idx_b.map(|i| solution[i]).unwrap_or(T::zero());
        v1 - v2
    }

    fn dc_conductance(&self) -> T {
        if self.cached_series_resistance.abs() < T::from(1e-12).unwrap() {
            T::from(1.0e12).unwrap()
        } else {
            T::one() / self.cached_series_resistance
        }
    }
}

impl<T: CircuitScalar> Component<T> for Inductor<T> {
    fn linearity(&self) -> ComponentLinearity {
        let any_voltage = self.inductance.depends_on_voltage()
            || self.series_resistance.depends_on_voltage();
        let any_dynamic = self.inductance.is_dynamic() || self.series_resistance.is_dynamic();

        if any_voltage {
            ComponentLinearity::NonLinear
        } else if any_dynamic {
            ComponentLinearity::TimeVariant
        } else {
            ComponentLinearity::LinearDynamic
        }
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
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
            self.dc_conductance()
        } else {
            self.conductance
        };
        stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_b, g, 0);
    }

    fn stamp_time_variant(
        &self,
        matrix: &mut MatMut<T>,
        ctx: &SimulationContext<T>,
        offset: usize,
    ) {
        let g = if ctx.is_dc_analysis {
            self.dc_conductance()
        } else {
            self.conductance
        };
        stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_b, g, offset);
    }

    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        matrix: &mut MatMut<T>,
        _rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
        l_size: usize,
    ) {
        let g = if ctx.is_dc_analysis {
            self.dc_conductance()
        } else {
            self.conductance
        };
        stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_b, g, l_size);
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
        stamp_current_source(rhs, self.cached_idx_a, self.cached_idx_b, self.eq_current, 0);
    }

    fn refresh_per_step(&mut self, eval: &ComponentEvalCtx, sim: &SimulationContext<T>) {
        let any_dynamic = self.inductance.is_dynamic() || self.series_resistance.is_dynamic();
        let any_voltage =
            self.inductance.depends_on_voltage() || self.series_resistance.depends_on_voltage();
        if any_dynamic && !any_voltage {
            self.cached_inductance = T::from(self.inductance.eval(eval)).unwrap();
            self.cached_series_resistance = T::from(self.series_resistance.eval(eval)).unwrap();
            self.update_conductance(sim.dt);
        }
    }

    fn refresh_per_iter(&mut self, eval: &ComponentEvalCtx, sim: &SimulationContext<T>) {
        let any_voltage =
            self.inductance.depends_on_voltage() || self.series_resistance.depends_on_voltage();
        if any_voltage {
            self.cached_inductance = T::from(self.inductance.eval(eval)).unwrap();
            self.cached_series_resistance = T::from(self.series_resistance.eval(eval)).unwrap();
            self.update_conductance(sim.dt);
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_new = self.get_voltage_diff(current_node_voltages);

        let i_new = if ctx.is_dc_analysis {
            v_new * self.dc_conductance()
        } else {
            (v_new * self.conductance) + self.eq_current
        };

        if ctx.is_dc_analysis {
            self.i_l_m1 = i_new;
            self.i_l_m2 = i_new;
        } else {
            self.i_l_m2 = self.i_l_m1;
            self.i_l_m1 = i_new;
        }

        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        let r_step = (three * self.cached_inductance) / (two * ctx.dt);
        let i_history_pure = ((four * self.i_l_m1) - self.i_l_m2) / three;
        let v_history = r_step * i_history_pure;

        self.eq_current = v_history * self.conductance;
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "Voltage".to_string(), unit: "V".to_string() },
            ComponentProbe { name: "Current".to_string(), unit: "A".to_string() },
            ComponentProbe { name: "Power".to_string(), unit: "W".to_string() },
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
            v_new * self.dc_conductance()
        } else {
            (v_new * self.conductance) + self.eq_current
        };

        out_observables[0] = v_new;
        out_observables[1] = i_new;
        out_observables[2] = v_new * i_new;
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_new = self.get_voltage_diff(node_voltages);

        let i_flow = if ctx.is_dc_analysis {
            v_new * self.dc_conductance()
        } else {
            (v_new * self.conductance) + self.eq_current
        };

        out_currents[0] = i_flow;
        out_currents[1] = -i_flow;
    }

    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        match name {
            "inductance" => {
                self.cached_inductance = value;
                self.update_conductance(ctx.dt);
                true
            }
            "resistance" | "esr" | "series_resistance" => {
                self.cached_series_resistance = value;
                self.update_conductance(ctx.dt);
                true
            }
            _ => false,
        }
    }

    fn set_param_value(&mut self, name: &str, pv: ParamValue) -> RebuildKind {
        let (was_dyn, was_volt) = match name {
            "inductance" => (
                self.inductance.is_dynamic(),
                self.inductance.depends_on_voltage(),
            ),
            "series_resistance" | "resistance" | "esr" => (
                self.series_resistance.is_dynamic(),
                self.series_resistance.depends_on_voltage(),
            ),
            _ => return RebuildKind::None,
        };

        match name {
            "inductance" => {
                if let Some(v) = pv.as_constant() {
                    self.cached_inductance = T::from(v).unwrap();
                }
                self.inductance = pv;
            }
            _ => {
                if let Some(v) = pv.as_constant() {
                    self.cached_series_resistance = T::from(v).unwrap();
                }
                self.series_resistance = pv;
            }
        }

        let now_dyn = match name {
            "inductance" => self.inductance.is_dynamic(),
            _ => self.series_resistance.is_dynamic(),
        };
        let now_volt = match name {
            "inductance" => self.inductance.depends_on_voltage(),
            _ => self.series_resistance.depends_on_voltage(),
        };

        if was_dyn != now_dyn || was_volt != now_volt {
            RebuildKind::Repartition
        } else if !now_dyn {
            RebuildKind::Restamp
        } else {
            RebuildKind::None
        }
    }
}
