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
use crate::ui::app::{CircuitApp, Tool};
use crate::ui::{ComponentBuildData, SimCommand};
use copperhead_core::components::diode::DiodeModel;
use copperhead_core::components::transistor::bjt::BjtModel;
use copperhead_core::components::triode::TriodeModel;
use egui::{Button, Checkbox, Frame, Id, Key, Label};
use std::path::PathBuf;

pub fn show(app: &mut CircuitApp, ctx: &egui::Context) {
    let running = app.sim_state.running;

    egui::SidePanel::left("tools")
        .frame(
            Frame::new()
                .inner_margin(8.)
                .fill(app.theme.panel_color),
        )
        .resizable(false)
        .show(ctx, |ui| {
            if ui
                .add_enabled(
                    !running,
                    Checkbox::new(&mut app.realtime_mode, "Real-Time Simulation"),
                )
                .clicked()
            {
                // TODO: handle this better
                // Command should be queued and processed by the sim thread when it is safe to do so instead of completely dropping it
                app.tx_command
                    .send(SimCommand::SetRealtime(app.realtime_mode))
                    .unwrap();
            };

            ui.heading("Tools");
            ui.add(Label::new("Controls:\nMB to Pan\nScroll to Zoom\n'CTRL+R' to Rotate\n'Esc and RMB' to Cancel").selectable(false));
            ui.separator();

            if ui.button("Select (S)").clicked() || ui.input(|i| i.key_pressed(Key::S) && !app.keybinds_locked) {
                app.selected_tool = Tool::Select;
            }
            if ui.button("Erase (E/DEL)").clicked()
                || ui.input(|i| i.key_pressed(Key::Delete) || i.key_pressed(Key::E) && !app.keybinds_locked)
            {
                app.selected_tool = Tool::Erase;
            }
            ui.separator();

            // TODO: make a grid with icons for each component type instead of label buttons

            // Component Buttons
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceWire(_)), Button::new("Draw Wire (W)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::W)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool = Tool::PlaceWire(None);
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Resistor { resistance: _ })), Button::new("Resistor (R)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::R)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::Resistor { resistance: 1000.0 }); // 1k Ohm
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Capacitor { capacitance: _, esr: _ })), Button::new("Capacitor (C)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::C)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::Capacitor { capacitance: 1e-6, esr: 0f64 }); // 1 uF
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Inductor { inductance: _, series_resistance: _ })), Button::new("Inductor (L)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::L)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::Inductor { inductance: 1e-3, series_resistance: 0f64 }); // 1 mH
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Diode { model: _ })), Button::new("Diode (D)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::D)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool = Tool::PlaceComponent(ComponentBuildData::Diode {
                    model: DiodeModel::D1N4148,
                });
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::DCSource { voltage: _ })), Button::new("DC Source (Y)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::Y)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::DCSource { voltage: 5.0 }); // 5V default
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::ASource { amplitude: _, frequency: _ })), Button::new("AC Source (A)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::A)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool = Tool::PlaceComponent(ComponentBuildData::ASource {
                    amplitude: 5.0,
                    frequency: 60.0,
                }); // 5V, 60Hz default
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::AudioSource { path: _ })), Button::new("Audio Source")).clicked()
            {
                app.selected_tool = Tool::PlaceComponent(ComponentBuildData::AudioSource { path: PathBuf::new() });
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Ground)), Button::new("Ground (G)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::G)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                app.selected_tool = Tool::PlaceComponent(ComponentBuildData::Ground);
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Label)), Button::new("Label (N)")).clicked()
                || (ui.input(|i| i.key_pressed(Key::N)) && ui.input(|i| i.modifiers.is_none()) && !app.keybinds_locked)
            {
                // set a variable to determine that a modal should be opened until closed
                ui.memory_mut(|mem| mem.data.insert_temp(Id::new("label_tool_open"), true));
                app.keybinds_locked = true;
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Bjt { model: _ })), Button::new("Bjt")).clicked()
            {
                app.selected_tool = Tool::PlaceComponent(ComponentBuildData::Bjt { model: BjtModel::GenericNPN });
            }
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::Triode { model: _ })), Button::new("Triode")).clicked()
            {
                app.selected_tool = Tool::PlaceComponent(ComponentBuildData::Triode { model: TriodeModel::T12AX7 });
            }
            ui.separator();
            if ui.add_enabled(!matches!(app.selected_tool, Tool::PlaceComponent(ComponentBuildData::AudioProbe { path: _ })), Button::new("Audio Probe")).clicked()
            {
                app.selected_tool = Tool::PlaceComponent(ComponentBuildData::AudioProbe { path: PathBuf::new() });
            }

            ui.separator();
            ui.label(format!("State: {:?}", app.selected_tool.get_name()));
        });
}
