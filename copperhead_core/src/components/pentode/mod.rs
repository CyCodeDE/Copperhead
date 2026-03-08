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
use crate::components::pentode::generic_pentode::{GenericPentodeDef, GenericPentodeModel};
use crate::components::pentode::physical_pentode::{PhysicalPentodeDef, PhysicalPentodeModel};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId};
use serde::{Deserialize, Serialize};
use strum_macros::EnumIter;

pub mod generic_pentode;
pub mod physical_pentode;

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum PentodeFidelity {
    Generic,   // Maps to Koren
    Precision, // Maps to Ayumi
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize, EnumIter)]
pub enum PentodeType {
    EL34,
    _6L6,
    _6L6GC,
}

impl PentodeType {
    /// Allows to query which fidelities are available for a given pentode
    /// to dynamically populate dropdowns or disable unsupported toggles.
    pub fn available_fidelities(&self) -> Vec<PentodeFidelity> {
        match self {
            PentodeType::EL34 => vec![PentodeFidelity::Generic, PentodeFidelity::Precision],
            PentodeType::_6L6 => vec![PentodeFidelity::Precision],
            PentodeType::_6L6GC => vec![PentodeFidelity::Generic],
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            PentodeType::EL34 => "EL34",
            PentodeType::_6L6 => "6L6",
            PentodeType::_6L6GC => "6L6GC",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PentodeDef {
    pub pentode_type: PentodeType,
    pub fidelity: PentodeFidelity,
}

impl<T: CircuitScalar> Instantiable<T> for PentodeDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        match (self.pentode_type, self.fidelity) {
            (PentodeType::EL34, PentodeFidelity::Generic) => {
                let def = GenericPentodeDef {
                    model: GenericPentodeModel::EL34,
                };
                def.instantiate(nodes, dt, circuit, _max_steps);
            }
            (PentodeType::EL34, PentodeFidelity::Precision) => {
                let def = PhysicalPentodeDef {
                    model: PhysicalPentodeModel::EL34,
                };
                def.instantiate(nodes, dt, circuit, _max_steps);
            }
            (PentodeType::_6L6, PentodeFidelity::Precision) => {
                let def = PhysicalPentodeDef {
                    model: PhysicalPentodeModel::_6L6,
                };
                def.instantiate(nodes, dt, circuit, _max_steps);
            }
            (PentodeType::_6L6GC, PentodeFidelity::Generic) => {
                let def = GenericPentodeDef {
                    model: GenericPentodeModel::_6L6GC,
                };
                def.instantiate(nodes, dt, circuit, _max_steps);
            }
            _ => panic!(
                "Unsupported Pentode type and fidelity combination: {:?} with {:?}",
                self.pentode_type, self.fidelity
            ),
        }
    }
}
