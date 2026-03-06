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
use crate::ui::app::CircuitApp;
use crate::ui::components::definitions::{ComponentUIExt, SchematicElement};
use egui::{Align, Id, TextEdit, Vec2};

pub fn handle_properties(app: &mut CircuitApp, ctx: &egui::Context) {
    if let Some(id) = app.editing_component_id {
        let mut open = true;
        let mut rename_request: Option<(usize, String)> = None;

        if let Some(idx) = app
            .state
            .schematic
            .components
            .iter()
            .position(|c| c.id == id)
        {
            let mut comp = app.state.schematic.components.remove(idx);

            // break if the component has no properties. For example ground
            if !matches!(comp.element, SchematicElement::Ground(_)) {
                app.keybinds_locked = true;

                egui::Modal::new(Id::new("component_edit_modal"))
                    .backdrop_color(app.theme.modal_backdrop)
                    .show(ctx, |ui| {
                        app.keybinds_locked = true;
                        ui.spacing_mut().item_spacing = Vec2::new(10.0, 10.0);

                        let prefix = comp.element.prefix().to_string();

                        ui.heading(format!("{} Properties", comp.element.ui_name()));

                        ui.horizontal(|ui| {
                            ui.label("Name:");
                            let mut text = comp.name.clone();
                            let text_edit = TextEdit::singleline(&mut text)
                                .cursor_at_end(true)
                                .clip_text(false)
                                .desired_width(35.0)
                                .horizontal_align(Align::Center);

                            if ui.add(text_edit).changed() {
                                if text.trim().is_empty() {
                                    rename_request = Some((id, prefix));
                                } else {
                                    comp.name = text.trim().to_string();
                                }
                            }
                        });

                        comp.element.draw_modal(app, ui);

                        ui.separator();

                        if ui.button("Close").clicked()
                            || ui.input(|i| i.key_pressed(egui::Key::Escape))
                        {
                            open = false;
                            app.keybinds_locked = false;
                        }
                    });
            } else {
                // Component has no properties, close the modal
                open = false;
                app.keybinds_locked = false;
            }

            app.state.schematic.components.insert(idx, comp);
        } else {
            // ID not found (maybe component was deleted?), close the modal
            open = false;
        }

        if let Some((target_id, prefix)) = rename_request {
            let new_name = app.state.schematic.generate_next_name(&prefix);
            if let Some(comp) = app
                .state
                .schematic
                .components
                .iter_mut()
                .find(|c| c.id == target_id)
            {
                comp.name = new_name;
            }
        }

        if !open {
            app.editing_component_id = None;
        }
    }
}
