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
use std::collections::HashMap;
use crate::model::{
    CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext,
};
use faer::ColRef;
use crate::circuit::NodePartition;

pub struct Resistor<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,
    pub conductance: T,
    pub resistance: T,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,
}

impl<T: CircuitScalar> Resistor<T> {
    pub fn new(a: NodeId, b: NodeId, resistance: T) -> Self {
        // Guard against divide-by-zero
        let conductance = if resistance.abs() < T::from(1e-12).unwrap() {
            // TODO: Treat it as a node-merge instead
            T::from(1.0e12).unwrap()
        } else {
            T::one() / resistance
        };

        Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            conductance,
            resistance,
        }
    }

    fn get_voltage(&self, node: NodeId, solution: &ColRef<T>) -> T {
        if node.0 == 0 {
            T::zero() // Ground
        } else {
            solution[node.0 - 1]
        }
    }
}

impl<T: CircuitScalar> Component<T> for Resistor<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::LinearStatic
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

    fn stamp_static(&self, matrix: &mut faer::MatMut<T>, _ctx: &SimulationContext<T>) {
        let g = self.conductance;

        let idx_a = self.cached_idx_a;
        let idx_b = self.cached_idx_b;

        // Diagonal: Node A
        if let Some(i) = idx_a {
            matrix[(i, i)] = matrix[(i, i)] + g;
        }

        // Diagonal: Node B
        if let Some(j) = idx_b {
            matrix[(j, j)] = matrix[(j, j)] + g;
        }

        // Off-Diagonals: Coupling
        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            matrix[(i, j)] = matrix[(i, j)] - g;
            matrix[(j, i)] = matrix[(j, i)] - g;
        }
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        if name == "resistance" {
            self.resistance = value;

            // update derived conductance
            self.conductance = if value.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap()
            } else {
                T::one() / value
            };
            // Return TRUE: The conductance matrix (G) changed,
            // so the solver MUST rebuild Matrix A before the next step.
            return true;
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
        ctx: &SimulationContext<T>,
        out_observables: &mut [T]
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

    fn terminal_currents(&self, sol: &ColRef<T>, _ctx: &SimulationContext<T>, out_currents: &mut [T]) {
        let v_a = self.get_voltage(self.node_a, sol);
        let v_b = self.get_voltage(self.node_b, sol);

        // Current flows from A -> B
        let i_a = (v_a - v_b) * self.conductance;
        // Current flowing INTO Node B is negative of that
        let i_b = -i_a;
        
        out_currents[0] = i_a;
        out_currents[1] = i_b;
    }
}
