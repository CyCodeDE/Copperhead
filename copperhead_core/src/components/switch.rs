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
use crate::util::mna::stamp_conductance;
use faer::{ColRef, MatMut};
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SwitchDef {
    /// Name of the global parameter that drives the switch state (0.0 = open, 1.0 = closed).
    pub param_name: String,
    /// Optional formula override. When set, bypasses `param_name`.
    /// A non-zero result means closed.
    #[serde(default)]
    pub state_formula: Option<String>,
    pub comment: Option<String>,
    /// Runtime-only: last known closed state for icon/label rendering. Not serialized.
    #[serde(skip)]
    pub current_closed: bool,
}

impl SwitchDef {
    pub fn new(param_name: String) -> Self {
        Self {
            param_name,
            state_formula: None,
            comment: None,
            current_closed: false,
        }
    }

    pub fn is_closed(&self) -> bool {
        self.current_closed
    }

    pub fn is_formula_mode(&self) -> bool {
        self.state_formula.is_some()
    }

    fn state_source(&self) -> &str {
        match &self.state_formula {
            Some(f) => f.as_str(),
            None => self.param_name.as_str(),
        }
    }
}

impl<T: CircuitScalar> Instantiable<T> for SwitchDef {
    fn instantiate(&self, nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let src = self.state_source();
        let pv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(src, ps))
            .unwrap_or_else(|| {
                let v = match src.trim().to_lowercase().as_str() {
                    "1" | "true" => 1.0,
                    _ => 0.0,
                };
                ParamValue::Constant(v)
            });
        circuit.add_component(Switch::<T>::new(nodes[0], nodes[1], pv));
    }
}

pub struct Switch<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,

    closed: ParamValue,
    cached_closed: bool,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    /// When true, the switch state is baked into the L-block and cannot
    /// be changed until the simulation is unfrozen.
    frozen: bool,

    phantom: PhantomData<T>,
}

impl<T: CircuitScalar> Switch<T> {
    pub fn new(a: NodeId, b: NodeId, closed: ParamValue) -> Self {
        let cached_closed = closed.as_constant().map(|v| v != 0.0).unwrap_or(false);
        Self {
            node_a: a,
            node_b: b,
            closed,
            cached_closed,
            cached_idx_a: None,
            cached_idx_b: None,
            frozen: false,
            phantom: PhantomData,
        }
    }

    fn get_voltage(&self, node: NodeId, solution: &ColRef<T>) -> T {
        if node.0 == 0 {
            T::zero()
        } else {
            solution[node.0 - 1]
        }
    }

    fn effective_conductance(&self) -> T {
        if self.cached_closed {
            T::from(1.0e6).unwrap()
        } else {
            T::from(1.0e-12).unwrap()
        }
    }
}

impl<T: CircuitScalar> Component<T> for Switch<T> {
    fn linearity(&self) -> ComponentLinearity {
        if self.closed.depends_on_voltage() {
            ComponentLinearity::NonLinear
        } else if self.frozen {
            // Frozen: switch state is baked into the L-block for this session.
            ComponentLinearity::LinearStatic
        } else {
            ComponentLinearity::TimeVariant
        }
    }

    fn is_freeze_eligible(&self) -> bool {
        self.closed.is_freeze_eligible()
    }

    fn set_frozen(&mut self, frozen: bool) {
        self.frozen = frozen;
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = node_map.get(&self.node_a).copied();
        self.cached_idx_b = node_map.get(&self.node_b).copied();
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {
        if self.frozen {
            stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_b, self.effective_conductance(), 0);
        }
    }

    fn stamp_time_variant(
        &self,
        matrix: &mut MatMut<T>,
        _ctx: &SimulationContext<T>,
        offset: usize,
    ) {
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            self.effective_conductance(),
            offset,
        );
    }

    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        matrix: &mut MatMut<T>,
        _rhs: &mut faer::ColMut<T>,
        _ctx: &SimulationContext<T>,
        l_size: usize,
    ) {
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            self.effective_conductance(),
            l_size,
        );
    }

    fn refresh_per_step(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if self.frozen {
            return;
        }
        if self.closed.is_dynamic() && !self.closed.depends_on_voltage() {
            self.cached_closed = self.closed.eval(eval) != 0.0;
        }
    }

    fn refresh_per_iter(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if self.frozen {
            return;
        }
        if self.closed.depends_on_voltage() {
            self.cached_closed = self.closed.eval(eval) != 0.0;
        }
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        if name == "closed" {
            let is_closed = value != T::zero();
            if self.cached_closed != is_closed {
                self.cached_closed = is_closed;
                return true;
            }
        }
        false
    }

    fn set_param_value(&mut self, name: &str, pv: ParamValue) -> RebuildKind {
        if name != "closed" {
            return RebuildKind::None;
        }

        let was_dyn = self.closed.is_dynamic();
        let was_volt = self.closed.depends_on_voltage();

        if let Some(v) = pv.as_constant() {
            self.cached_closed = v != 0.0;
        }
        self.closed = pv;

        let now_dyn = self.closed.is_dynamic();
        let now_volt = self.closed.depends_on_voltage();

        if was_dyn != now_dyn || was_volt != now_volt {
            RebuildKind::Repartition
        } else if !now_dyn {
            RebuildKind::Restamp
        } else {
            RebuildKind::None
        }
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "V_delta".into(), unit: "V".into() },
            ComponentProbe { name: "Current".into(), unit: "A".into() },
            ComponentProbe { name: "Power".into(), unit: "W".into() },
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
        let current = voltage_drop * self.effective_conductance();
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

        let i_a = (v_a - v_b) * self.effective_conductance();
        out_currents[0] = i_a;
        out_currents[1] = -i_a;
    }
}
