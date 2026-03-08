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
use crate::ui::components::definitions::SchematicElement;
use crate::ui::components::definitions::ground::GroundDef;
use crate::ui::components::definitions::label::LabelDef;
use copperhead_core::components::audio_probe::AudioProbeDef;
use copperhead_core::components::capacitor::CapacitorDef;
use copperhead_core::components::diode::{DiodeDef, DiodeModel};
use copperhead_core::components::inductor::InductorDef;
use copperhead_core::components::pentode::{PentodeDef, PentodeModel};
use copperhead_core::components::resistor::ResistorDef;
use copperhead_core::components::transistor::bjt::{BjtDef, BjtModel};
use copperhead_core::components::triode::{TriodeDef, TriodeFidelity, TriodeType};
use copperhead_core::components::voltage_source::{VoltageSourceDef, VoltageSourceType};
use copperhead_core::descriptor::ComponentDef;
use egui::Key;
use std::path::PathBuf;

pub struct PaletteItem {
    pub label: &'static str,
    pub shortcut_key: Option<Key>,
    pub shortcut_name: &'static str,
    pub element: SchematicElement,
}

impl PaletteItem {
    pub fn get_standard_palette() -> Vec<PaletteItem> {
        vec![
            PaletteItem {
                label: "Resistor",
                shortcut_key: Some(Key::R),
                shortcut_name: "R",
                element: SchematicElement::Core(ComponentDef::Resistor(ResistorDef {
                    resistance: 1000.,
                })),
            },
            PaletteItem {
                label: "Capacitor",
                shortcut_key: Some(Key::C),
                shortcut_name: "C",
                element: SchematicElement::Core(ComponentDef::Capacitor(CapacitorDef {
                    capacitance: 1e-6,
                    esr: 0.,
                })),
            },
            PaletteItem {
                label: "Inductor",
                shortcut_key: Some(Key::L),
                shortcut_name: "L",
                element: SchematicElement::Core(ComponentDef::Inductor(InductorDef {
                    inductance: 1e-3,
                    series_resistance: 0.,
                })),
            },
            PaletteItem {
                label: "Diode",
                shortcut_key: Some(Key::D),
                shortcut_name: "D",
                element: SchematicElement::Core(ComponentDef::Diode(DiodeDef {
                    model: DiodeModel::_1N4148,
                })),
            },
            PaletteItem {
                label: "Bjt",
                shortcut_key: None,
                shortcut_name: "",
                element: SchematicElement::Core(ComponentDef::Bjt(BjtDef {
                    model: BjtModel::GenericNPN,
                })),
            },
            PaletteItem {
                label: "Triode",
                shortcut_key: None,
                shortcut_name: "",
                element: SchematicElement::Core(ComponentDef::Triode(TriodeDef {
                    fidelity: TriodeFidelity::Precision,
                    triode_type: TriodeType::_12AX7,
                })),
            },
            PaletteItem {
                label: "Pentode",
                shortcut_key: None,
                shortcut_name: "",
                element: SchematicElement::Core(ComponentDef::Pentode(PentodeDef {
                    model: PentodeModel::_6L6GC,
                })),
            },
            PaletteItem {
                label: "DC Source",
                shortcut_key: Some(Key::Y),
                shortcut_name: "Y",
                element: SchematicElement::Core(ComponentDef::VoltageSource(VoltageSourceDef {
                    source_type: VoltageSourceType::DC { voltage: 5. },
                })),
            },
            PaletteItem {
                label: "AC Source",
                shortcut_key: Some(Key::A),
                shortcut_name: "A",
                element: SchematicElement::Core(ComponentDef::VoltageSource(VoltageSourceDef {
                    source_type: VoltageSourceType::AC {
                        amplitude: 5.,
                        frequency: 60.,
                        phase: 0.,
                    },
                })),
            },
            PaletteItem {
                label: "Audio Source",
                shortcut_key: None,
                shortcut_name: "",
                element: SchematicElement::Core(ComponentDef::VoltageSource(VoltageSourceDef {
                    source_type: VoltageSourceType::AudioBuffer {
                        file_path: PathBuf::new(),
                    },
                })),
            },
            PaletteItem {
                label: "Audio Probe",
                shortcut_key: None,
                shortcut_name: "",
                element: SchematicElement::Core(ComponentDef::AudioProbe(AudioProbeDef {
                    file_path: PathBuf::new(),
                })),
            },
            PaletteItem {
                label: "Ground",
                shortcut_key: Some(Key::G),
                shortcut_name: "G",
                element: SchematicElement::Ground(GroundDef),
            },
            PaletteItem {
                label: "Label",
                shortcut_key: Some(Key::N),
                shortcut_name: "N",
                element: SchematicElement::Label(LabelDef),
            },
        ]
    }
}
