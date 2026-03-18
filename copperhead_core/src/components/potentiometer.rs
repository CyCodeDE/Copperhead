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
use num_traits::cast;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct PotentiometerDef {
    pub resistance: f64,
    /// Wiper position in [0.0, 1.0]. 0.0 means wiper is at node A, 1.0 means wiper is at node B.
    pub position: f64,
    pub comment: Option<String>,

    // Only relevant for the UI
    pub max: f64,
    pub min: f64,
    pub step: f64,
}

impl<T: CircuitScalar> Instantiable<T> for PotentiometerDef {
    fn instantiate(&self, nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let comp = Potentiometer::new(
            nodes[0],
            nodes[1],
            nodes[2],
            cast(self.resistance).expect("Failed to cast resistance"),
            cast(self.position).expect("Failed to cast position"),
        );
        circuit.add_component(comp);
    }
}

pub struct Potentiometer<T: CircuitScalar> {
    /// One end of the resistive track
    pub node_a: NodeId,
    /// Other end of the resistive track
    pub node_b: NodeId,
    /// Wiper (sliding contact)
    pub node_w: NodeId,

    pub total_resistance: T,
    /// Wiper position in [0.0, 1.0]
    pub position: T,

    /// Conductance between node A and wiper: G_aw = 1 / (pos * R_total)
    pub conductance_aw: T,
    /// Conductance between node B and wiper: G_bw = 1 / ((1 - pos) * R_total)
    pub conductance_bw: T,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,
    cached_idx_w: Option<usize>,
}

impl<T: CircuitScalar> Potentiometer<T> {
    pub fn new(a: NodeId, b: NodeId, w: NodeId, total_resistance: T, position: T) -> Self {
        let (g_aw, g_bw) = Self::compute_conductances(total_resistance, position);

        Self {
            node_a: a,
            node_b: b,
            node_w: w,
            total_resistance,
            position,
            conductance_aw: g_aw,
            conductance_bw: g_bw,
            cached_idx_a: None,
            cached_idx_b: None,
            cached_idx_w: None,
        }
    }

    fn compute_conductances(total_resistance: T, position: T) -> (T, T) {
        let min_r = T::from(1e-12).unwrap();
        let max_g = T::from(1.0e12).unwrap();

        let r_aw = position * total_resistance;
        let r_bw = (T::one() - position) * total_resistance;

        let g_aw = if r_aw.abs() < min_r {
            max_g
        } else {
            T::one() / r_aw
        };

        let g_bw = if r_bw.abs() < min_r {
            max_g
        } else {
            T::one() / r_bw
        };

        (g_aw, g_bw)
    }

    fn update_conductances(&mut self) {
        let (g_aw, g_bw) = Self::compute_conductances(self.total_resistance, self.position);
        self.conductance_aw = g_aw;
        self.conductance_bw = g_bw;
    }

    fn get_voltage(&self, node: NodeId, solution: &ColRef<T>) -> T {
        if node.0 == 0 {
            T::zero() // Ground
        } else {
            solution[node.0 - 1]
        }
    }
}

impl<T: CircuitScalar> Component<T> for Potentiometer<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::TimeVariant
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = node_map.get(&self.node_a).copied();
        self.cached_idx_b = node_map.get(&self.node_b).copied();
        self.cached_idx_w = node_map.get(&self.node_w).copied();
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b, self.node_w]
    }

    fn stamp_time_variant(
        &self,
        matrix: &mut MatMut<T>,
        _ctx: &SimulationContext<T>,
        offset: usize,
    ) {
        // Stamp R_aw between node A and wiper
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_w,
            self.conductance_aw,
            offset,
        );
        // Stamp R_bw between node B and wiper
        stamp_conductance(
            matrix,
            self.cached_idx_b,
            self.cached_idx_w,
            self.conductance_bw,
            offset,
        );
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        match name {
            "resistance" => {
                self.total_resistance = value;
                self.update_conductances();
                true
            }
            "position" => {
                self.position = value;
                self.update_conductances();
                true
            }
            _ => false,
        }
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe {
                name: "V_aw".into(),
                unit: "V".into(),
            },
            ComponentProbe {
                name: "V_bw".into(),
                unit: "V".into(),
            },
            ComponentProbe {
                name: "I_aw".into(),
                unit: "A".into(),
            },
            ComponentProbe {
                name: "I_bw".into(),
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
        let v_w = self.get_voltage(self.node_w, node_voltages);

        let v_aw = v_a - v_w;
        let v_bw = v_b - v_w;

        let i_aw = v_aw * self.conductance_aw;
        let i_bw = v_bw * self.conductance_bw;

        let power = v_aw * i_aw + v_bw * i_bw;

        out_observables[0] = v_aw;
        out_observables[1] = v_bw;
        out_observables[2] = i_aw;
        out_observables[3] = i_bw;
        out_observables[4] = power;
    }

    fn terminal_currents(
        &self,
        sol: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_a = self.get_voltage(self.node_a, sol);
        let v_b = self.get_voltage(self.node_b, sol);
        let v_w = self.get_voltage(self.node_w, sol);

        // Current from A into wiper
        let i_a = (v_a - v_w) * self.conductance_aw;
        // Current from B into wiper
        let i_b = (v_b - v_w) * self.conductance_bw;
        // Current leaving wiper (KCL: i_w = -(i_a + i_b))
        let i_w = -(i_a + i_b);

        out_currents[0] = i_a;
        out_currents[1] = i_b;
        out_currents[2] = i_w;
    }
}
