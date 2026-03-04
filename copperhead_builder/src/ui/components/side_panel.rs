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
use crate::ui::SimCommand;
use crate::ui::app::{CircuitApp, Tool};
use crate::ui::components::definitions::SchematicElement;
use crate::ui::components::palette::PaletteItem;
use egui::{Button, Checkbox, Frame, Id, Key, Label};

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

            // TODO: make a grid with icons for each component type instead of label buttons


            // Component Buttons
            let palette = PaletteItem::get_standard_palette();

            if !app.keybinds_locked && ui.input(|i| i.modifiers.is_none()) {
                if ui.input(|i| i.key_pressed(Key::W)) {
                    app.selected_tool = Tool::PlaceWire(None);
                } else if ui.input(|i| i.key_pressed(Key::E) || i.key_pressed(Key::Delete)) {
                    app.selected_tool = Tool::Erase;
                } else if ui.input(|i| i.key_pressed(Key::S)) {
                    app.selected_tool = Tool::Select;
                }

                for item in &palette {
                    if let Some(key) = item.shortcut_key {
                        if ui.input(|i| i.key_pressed(key)) {
                            if matches!(item.element, SchematicElement::Label(_)) {
                                ui.memory_mut(|mem| mem.data.insert_temp(Id::new("label_tool_open"), true));
                                app.keybinds_locked = true;
                            }
                            app.selected_tool = Tool::PlaceComponent(item.element.clone());
                        }
                    }
                }
            }

            let size = egui::vec2(ui.available_width(), 28.0);
            let select_button = Button::new("Select (S)").shortcut_text("S");
            let erase_button = Button::new("Erase (E/DEL)").shortcut_text("E / DEL");
            let wire_button = Button::new("Place Wire (W)").shortcut_text("W");

            let is_select_active = matches!(app.selected_tool, Tool::Select);
            if ui.add_enabled_ui(!is_select_active, |ui| ui.add_sized(size, select_button)).inner.clicked() {
                app.selected_tool = Tool::Select;
            };
            let is_erase_active = matches!(app.selected_tool, Tool::Erase);
            if ui.add_enabled_ui(!is_erase_active, |ui| ui.add_sized(size, erase_button)).inner.clicked() {
                app.selected_tool = Tool::Erase;
            };
            let is_wire_active = matches!(app.selected_tool, Tool::PlaceWire(_));

            ui.separator();

            if ui.add_enabled_ui(!is_wire_active, |ui| ui.add_sized(size, wire_button)).inner.clicked() {
                app.selected_tool = Tool::PlaceWire(None);
            };

            ui.separator();

            ui.columns(2, |cols| {
                for (index, item) in palette.iter().enumerate() {
                    let col_ui = &mut cols[index % 2];

                    let is_active = match &app.selected_tool {
                        Tool::PlaceComponent(current_element) => {
                            // Assumes you implemented the `is_same_category` method from earlier
                            current_element.is_same_category(&item.element)
                        },
                        _ => false,
                    };

                    let button_size = egui::vec2(col_ui.available_width(), 28.0);
                    let button = Button::new(item.label.to_string()).shortcut_text(item.shortcut_name);

                    // Draw the button and handle clicks
                    if col_ui.add_enabled_ui(!is_active, |ui| ui.add_sized(button_size, button)).inner.clicked() {
                        if matches!(item.element, SchematicElement::Label(_)) {
                            // Handle modal state
                            col_ui.memory_mut(|mem| mem.data.insert_temp(Id::new("label_tool_open"), true));
                            app.keybinds_locked = true;
                        }
                        app.selected_tool = Tool::PlaceComponent(item.element.clone());
                    }
                }
            });

            ui.separator();
            ui.label(format!("State: {:?}", app.selected_tool.get_name()));
        });
}
