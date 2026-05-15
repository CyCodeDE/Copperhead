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
use crate::parameter::{ComponentEvalCtx, ParamValue, RebuildKind, resolve_param_value};
use crate::util::deserialize_number_or_string;
use crate::util::mna::stamp_conductance;
use faer::ColRef;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ResistorDef {
    #[serde(deserialize_with = "deserialize_number_or_string")]
    pub resistance: String,
}

impl ResistorDef {
    pub fn new(resistance: f64) -> Self {
        Self {
            resistance: resistance.to_string(),
        }
    }
}

impl<T: CircuitScalar> Instantiable<T> for ResistorDef {
    fn instantiate(&self, nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let pv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(&self.resistance, ps))
            .unwrap_or_else(|| {
                let v = self.resistance.trim().parse::<f64>().unwrap_or(0.0);
                ParamValue::Constant(v)
            });
        circuit.add_component(Resistor::<T>::new(nodes[0], nodes[1], pv));
    }
}

pub struct Resistor<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,

    /// User-facing value. May be a constant or a compiled formula.
    pub resistance: ParamValue,

    /// Derived from `resistance`. Re-evaluated by `refresh_per_step` when
    /// formula-driven; set once at construction and on `set_param_value`
    /// for constant values.
    pub conductance: T,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,
}

impl<T: CircuitScalar> Resistor<T> {
    pub fn new(a: NodeId, b: NodeId, resistance: ParamValue) -> Self {
        // Initial conductance for the constant case. Formula-driven values
        // get overwritten by the first `refresh_per_step` call.
        let initial_r = resistance.as_constant().unwrap_or(1.0);
        let conductance = Self::r_to_g(initial_r);

        Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            conductance,
            resistance,
        }
    }

    fn r_to_g(r: f64) -> T {
        // Guard against divide-by-zero (TODO: treat as a node-merge instead).
        let g = if r.abs() < 1e-12 { 1.0e12 } else { 1.0 / r };
        T::from(g).unwrap()
    }

    fn get_voltage(&self, node: NodeId, solution: &ColRef<T>) -> T {
        if node.0 == 0 {
            T::zero()
        } else {
            solution[node.0 - 1]
        }
    }
}

impl<T: CircuitScalar> Component<T> for Resistor<T> {
    fn linearity(&self) -> ComponentLinearity {
        if self.resistance.depends_on_voltage() {
            ComponentLinearity::NonLinear
        } else if self.resistance.is_dynamic() {
            ComponentLinearity::TimeVariant
        } else {
            ComponentLinearity::LinearStatic
        }
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = node_map.get(&self.node_a).copied();
        self.cached_idx_b = node_map.get(&self.node_b).copied();
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn stamp_static(&self, matrix: &mut faer::MatMut<T>, _ctx: &SimulationContext<T>) {
        // Only called by the orchestrator when linearity != TimeVariant.
        // For NonLinear (formula reads voltage) we still want to skip the
        // static stamp since stamp_nonlinear (treated like time-variant
        // here for the conductance contribution) will handle it. Guard
        // against that here to keep the orchestrator simple.
        if self.resistance.is_dynamic() {
            return;
        }
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            self.conductance,
            0,
        );
    }

    fn stamp_time_variant(
        &self,
        matrix: &mut faer::MatMut<T>,
        _ctx: &SimulationContext<T>,
        offset: usize,
    ) {
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            self.conductance,
            offset,
        );
    }

    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        matrix: &mut faer::MatMut<T>,
        _rhs: &mut faer::ColMut<T>,
        _ctx: &SimulationContext<T>,
        l_size: usize,
    ) {
        // Conductance was refreshed for this iteration via refresh_per_iter
        // (the formula reads a node voltage). Stamp it like time-variant.
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            self.conductance,
            l_size,
        );
    }

    fn refresh_per_step(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if self.resistance.is_dynamic() && !self.resistance.depends_on_voltage() {
            let r = self.resistance.eval(eval);
            self.conductance = Self::r_to_g(r);
        }
    }

    fn refresh_per_iter(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if self.resistance.depends_on_voltage() {
            let r = self.resistance.eval(eval);
            self.conductance = Self::r_to_g(r);
        }
    }

    fn set_param_value(&mut self, name: &str, pv: ParamValue) -> RebuildKind {
        match name {
            "resistance" => {
                let was_dynamic = self.resistance.is_dynamic();
                let now_dynamic = pv.is_dynamic();
                self.resistance = pv;

                // For new constants, update the derived conductance now so
                // the next stamp uses the right value without waiting on
                // refresh.
                if let Some(r) = self.resistance.as_constant() {
                    self.conductance = Self::r_to_g(r);
                }

                if was_dynamic != now_dynamic {
                    RebuildKind::Repartition
                } else if !now_dynamic {
                    RebuildKind::Restamp
                } else {
                    RebuildKind::None
                }
            }
            _ => RebuildKind::None,
        }
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe {
                name: "V_delta".into(),
                unit: "V".into(),
            },
            ComponentProbe {
                name: "Current".into(),
                unit: "A".into(),
            },
            ComponentProbe {
                name: "Power".into(),
                unit: "W".into(),
            },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        let v_a = self.get_voltage(self.node_a, node_voltages);
        let v_b = self.get_voltage(self.node_b, node_voltages);

        let voltage_drop = v_a - v_b;
        let current = voltage_drop * self.conductance;
        let power = voltage_drop * current;

        out_observables[0] = voltage_drop;
        out_observables[1] = current;
        out_observables[2] = power;
    }

    fn terminal_currents(
        &self,
        sol: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_a = self.get_voltage(self.node_a, sol);
        let v_b = self.get_voltage(self.node_b, sol);

        let i_a = (v_a - v_b) * self.conductance;
        let i_b = -i_a;

        out_currents[0] = i_a;
        out_currents[1] = i_b;
    }
}
