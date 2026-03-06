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
use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;
use crate::circuit::Circuit;
use crate::components::tube::triode::physical_triode::{PhysicalTriode, PhysicalTriodeDef, PhysicalTriodeModel};
use crate::components::tube::triode::generic_triode::{GenericTriode, GenericTriodeDef, GenericTriodeModel};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId};

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum TriodeFidelity {
    Generic,   // Maps to Koren
    Precision, // Maps to Ayumi
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize, EnumIter)]
pub enum TriodeType {
    _12AX7,
    _12AU7,
}

impl TriodeType {
    /// Allows to query which fidelities are available for a given tube
    /// to dynamically populate dropdowns or disable unsupported toggles.
    pub fn available_fidelities(&self) -> Vec<TriodeFidelity> {
        match self {
            TriodeType::_12AX7 => vec![TriodeFidelity::Generic, TriodeFidelity::Precision],
            TriodeType::_12AU7 => vec![TriodeFidelity::Precision],
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            TriodeType::_12AX7 => "12AX7",
            TriodeType::_12AU7 => "12AU7",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TriodeDef {
    pub triode_type: TriodeType,
    pub fidelity: TriodeFidelity,
}

impl<T: CircuitScalar> Instantiable<T> for TriodeDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        match (self.triode_type, self.fidelity) {
            (TriodeType::_12AX7, TriodeFidelity::Generic) => {
                let def = GenericTriodeDef {
                    model: GenericTriodeModel::_12AX7
                };
                def.instantiate(nodes, dt, circuit, _max_steps);
            }
            (TriodeType::_12AX7, TriodeFidelity::Precision) => {
                let def = PhysicalTriodeDef {
                    model: PhysicalTriodeModel::_12AX7
                };
                def.instantiate(nodes, dt, circuit, _max_steps);
            }
            (TriodeType::_12AU7, TriodeFidelity::Precision) => {
                let def = PhysicalTriodeDef {
                    model: PhysicalTriodeModel::_12AU7
                };
                def.instantiate(nodes, dt, circuit, _max_steps);
            }
            _ => panic!("Unsupported tube type and fidelity combination: {:?} with {:?}", self.triode_type, self.fidelity),
        }
    }
}