/*
 * Copyright (c) 2026 CyCode and the Copperhead contributors
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
use crate::components::transformer::accurate_transformer::AccurateTransformerDef;
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId};
use serde::{Deserialize, Serialize};

pub mod accurate_transformer;
pub mod performance_transformer;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum TransformerMode {
    Performance,
    Accurate,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Winding {
    pub node_a: NodeId,
    pub node_b: NodeId,
    pub inductance: f64,
    pub series_resistance: f64,
    pub location: WindingLocation,
    pub phase_inverted: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum WindingLocation {
    Left,
    Right,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Coupling {
    pub winding_1: usize,
    pub winding_2: usize,
    pub k: f64, // Coupling coefficient
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TransformerDef {
    pub mode: TransformerMode,
    pub windings: Vec<Winding>,
    pub couplings: Vec<Coupling>,

    /// Saturation flux in Webers (Wb). Determines the maximum magnetic flux.
    pub phi_sat: f64,
    /// Magnetizing current in Amperes (A) at which the core begins to saturate.
    pub i_sat: f64,
}

impl<T: CircuitScalar> Instantiable<T> for TransformerDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, max_steps: usize) {
        match self.mode {
            TransformerMode::Performance => {}
            TransformerMode::Accurate => {
                let mut def = AccurateTransformerDef {
                    windings: self.windings.clone(),
                    couplings: self.couplings.clone(),
                    i_sat: self.i_sat,
                    phi_sat: self.phi_sat,
                };

                let mut node_idx = 0;
                for w in &mut def.windings {
                    if node_idx + 1 < nodes.len() {
                        w.node_a = nodes[node_idx];
                        w.node_b = nodes[node_idx + 1];
                    }
                    node_idx += 2;
                }

                def.instantiate(nodes, dt, circuit, max_steps);
            }
        }
    }
}
