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
use crate::ui::ParameterDecl;
use crate::ui::app::CircuitApp;
use egui::Ui;

/// Panel for declaring global simulation parameters.
/// Each parameter has a name (used in formulas) and a default numeric value.
/// While the simulation is running, dragging a value calls `set_param` on the
/// shared `ParamTable` immediately (wait-free, no rebuild needed).
/// Adding/removing/renaming parameters requires a circuit rebuild.
pub fn show(app: &mut CircuitApp, ui: &mut Ui) {
    ui.collapsing("Parameters", |ui| {
        let mut to_remove: Option<usize> = None;

        for i in 0..app.state.parameters.len() {
            // Clone the name so we can hold it across the split borrow below.
            let name = app.state.parameters[i].name.clone();

            ui.horizontal(|ui| {
                ui.add(
                    egui::TextEdit::singleline(&mut app.state.parameters[i].name)
                        .desired_width(80.0)
                        .hint_text("name"),
                );

                let drag = ui.add(
                    egui::DragValue::new(&mut app.state.parameters[i].default)
                        .speed(0.01),
                );

                // Propagate the new value to the live simulation immediately.
                if drag.changed() {
                    let new_val = app.state.parameters[i].default;
                    if let Some(ps) = &app.sim_state.param_system {
                        ps.set_param(&name, new_val);
                    }
                }

                if ui.small_button("×").clicked() {
                    to_remove = Some(i);
                }
            });
        }

        if let Some(i) = to_remove {
            app.state.parameters.remove(i);
        }

        if ui.small_button("+ Add").clicked() {
            let n = app.state.parameters.len() + 1;
            app.state.parameters.push(ParameterDecl {
                name: format!("param{}", n),
                default: 1.0,
            });
        }
    });
}
