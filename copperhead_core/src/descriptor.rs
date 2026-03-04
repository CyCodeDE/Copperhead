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
use crate::components::audio_probe::AudioProbeDef;
use crate::components::capacitor::CapacitorDef;
use crate::components::diode::DiodeDef;
use crate::components::inductor::InductorDef;
use crate::components::pentode::PentodeDef;
use crate::components::resistor::ResistorDef;
use crate::components::transistor::bjt::BjtDef;
use crate::components::triode::TriodeDef;
use crate::components::voltage_source::VoltageSourceDef;
use crate::model::{CircuitScalar, NodeId};

/// Trait for defining how a data payload instantiates a simulation component.
pub trait Instantiable<T: CircuitScalar> {
    /// `nodes` is an ordered list of solver nodes mapped by the netlist compiler.
    /// The order MUST match the UI local pins and the simulation ports.
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, max_steps: usize);
}

macro_rules! define_components {
    (
        $(
            // Matches: VariantName(StructName)
            $variant:ident($def_type:ty)
        ),* $(,)?
    ) => {
        /// The unified enum holding all component definitions
        #[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
        pub enum ComponentDef {
            $( $variant($def_type) ),*
        }

        impl ComponentDef {
            /// Delegates the instantiation to the specific component definition
            pub fn instantiate<T: CircuitScalar>(
                &self,
                nodes: &[NodeId],
                dt: T,
                circuit: &mut Circuit<T>,
                max_steps: usize
            ) {
                match self {
                    $( Self::$variant(def) => def.instantiate(nodes, dt, circuit, max_steps) ),*
                }
            }
        }
    };
}

define_components! {
    Resistor(ResistorDef),
    Capacitor(CapacitorDef),
    Inductor(InductorDef),
    Diode(DiodeDef),
    AudioProbe(AudioProbeDef),
    VoltageSource(VoltageSourceDef),
    Bjt(BjtDef),
    Triode(TriodeDef),
    Pentode(PentodeDef),
    // ... add all other components here
}