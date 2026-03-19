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
use crate::util::mna::stamp_conductance;
use faer::{ColRef, MatMut};
use std::collections::HashMap;
use std::marker::PhantomData;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct SwitchDef {
    pub closed: bool,
    pub comment: Option<String>,
}

impl<T: CircuitScalar> Instantiable<T> for SwitchDef {
    fn instantiate(&self, nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let comp: Switch<T> = Switch::<T>::new(nodes[0], nodes[1], self.closed);
        circuit.add_component(comp);
    }
}

pub struct Switch<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,
    pub closed: bool,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    phantom: PhantomData<T>,
}

impl<T: CircuitScalar> Switch<T> {
    pub fn new(a: NodeId, b: NodeId, closed: bool) -> Self {
        Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            closed,

            phantom: PhantomData,
        }
    }

    fn get_voltage(&self, node: NodeId, solution: &ColRef<T>) -> T {
        if node.0 == 0 {
            T::zero() // Ground
        } else {
            solution[node.0 - 1]
        }
    }

    fn effective_conductance(&self) -> T {
        if self.closed {
            T::from(1.0e6).unwrap() // 1 micro-ohm
        } else {
            T::from(1.0e-12).unwrap() // g_min
        }
    }
}

impl<T: CircuitScalar> Component<T> for Switch<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::TimeVariant
    }

    fn bake_indices(&mut self, ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        let a = node_map.get(&self.node_a).copied();
        let b = node_map.get(&self.node_b).copied();

        if a.is_none() {
            self.cached_idx_a = None;
        } else {
            self.cached_idx_a = a;
        }
        if b.is_none() {
            self.cached_idx_b = None;
        } else {
            self.cached_idx_b = b;
        }
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
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

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        if name == "closed" {
            let is_closed = value == T::from(1.0).unwrap();

            if self.closed != is_closed {
                self.closed = is_closed;
                return true;
            }
        }
        false
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

        // Current flows from A -> B
        let i_a = (v_a - v_b) * self.effective_conductance();
        // Current flowing INTO Node B is negative of that
        let i_b = -i_a;

        out_currents[0] = i_a;
        out_currents[1] = i_b;
    }
}
