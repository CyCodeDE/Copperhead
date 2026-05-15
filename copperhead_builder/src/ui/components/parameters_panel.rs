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
use crate::ui::components::definitions::SchematicElement;
use crate::ui::{ComponentDef, ParameterKind};
use egui::{ComboBox, Ui};

/// Rename-sync: when a parameter's name changes, update any pot/switch that referenced the old name.
fn sync_param_rename(app: &mut CircuitApp, old_name: &str, new_name: &str) {
    for comp in &mut app.state.schematic.components {
        if let SchematicElement::Core(def) = &mut comp.element {
            match def {
                ComponentDef::Potentiometer(pot) => {
                    if pot.param_name == old_name {
                        pot.param_name = new_name.to_string();
                    }
                }
                ComponentDef::Switch(sw) => {
                    if sw.param_name == old_name {
                        sw.param_name = new_name.to_string();
                    }
                }
                _ => {}
            }
        }
    }
}

/// Update the runtime display value on the matching component def so icons stay current.
fn sync_param_value(app: &mut CircuitApp, name: &str, value: f64) {
    for comp in &mut app.state.schematic.components {
        if let SchematicElement::Core(def) = &mut comp.element {
            match def {
                ComponentDef::Potentiometer(pot) if pot.param_name == name => {
                    pot.current_position = value;
                }
                ComponentDef::Switch(sw) if sw.param_name == name => {
                    sw.current_closed = value != 0.0;
                }
                _ => {}
            }
        }
    }
}

pub fn show(app: &mut CircuitApp, ui: &mut Ui) {
    ui.collapsing("Parameters", |ui| {
        let mut to_remove: Option<usize> = None;
        let mut rename: Option<(usize, String, String)> = None; // (idx, old, new)

        let frozen = app.sim_state.frozen;
        if frozen {
            ui.label(
                egui::RichText::new("Frozen — parameters locked")
                    .color(egui::Color32::YELLOW)
                    .small(),
            );
        }

        for i in 0..app.state.parameters.len() {
            let name = app.state.parameters[i].name.clone();
            let is_auto = app.state.parameters[i].auto;

            ui.horizontal(|ui| {
                // Name edit
                let edit_id = egui::Id::new("param_name_orig").with(i);
                let name_resp = ui.add(
                    egui::TextEdit::singleline(&mut app.state.parameters[i].name)
                        .desired_width(80.0)
                        .hint_text("name"),
                );
                if name_resp.gained_focus() {
                    ui.ctx()
                        .data_mut(|d| d.insert_temp(edit_id, app.state.parameters[i].name.clone()));
                }
                if name_resp.lost_focus() {
                    if let Some(original) = ui.ctx().data(|d| d.get_temp::<String>(edit_id)) {
                        if original != app.state.parameters[i].name {
                            rename = Some((i, original, app.state.parameters[i].name.clone()));
                        }
                        ui.ctx().data_mut(|d| d.remove_temp::<String>(edit_id));
                    }
                }

                // Value widget — kind-dependent. Disabled during freeze so the
                // user cannot change parameters that are baked into the L-block.
                let new_val = ui
                    .add_enabled_ui(!frozen, |ui| match &app.state.parameters[i].kind.clone() {
                        ParameterKind::Number => {
                            let mut v = app.state.parameters[i].default;
                            let changed =
                                ui.add(egui::DragValue::new(&mut v).speed(0.01)).changed();
                            if changed { Some(v) } else { None }
                        }
                        ParameterKind::Boolean => {
                            let mut checked = app.state.parameters[i].default != 0.0;
                            let changed = ui.checkbox(&mut checked, "").changed();
                            if changed {
                                Some(if checked { 1.0 } else { 0.0 })
                            } else {
                                None
                            }
                        }
                        ParameterKind::Slider { min, max } => {
                            let (min, max) = (*min, *max);
                            let mut v = app.state.parameters[i].default;
                            let changed = ui.add(egui::Slider::new(&mut v, min..=max)).changed();
                            if changed { Some(v) } else { None }
                        }
                    })
                    .inner;

                if let Some(v) = new_val {
                    app.state.parameters[i].default = v;
                    if let Some(ps) = &app.sim_state.param_system {
                        ps.set_param(&name, v);
                    }
                    sync_param_value(app, &name, v);
                }

                // Type picker — only for user-created params
                if !is_auto {
                    let kind_label = match &app.state.parameters[i].kind {
                        ParameterKind::Number => "№",
                        ParameterKind::Boolean => "☑",
                        ParameterKind::Slider { .. } => "⇔",
                    };
                    ComboBox::from_id_salt(egui::Id::new("param_kind").with(i))
                        .selected_text(kind_label)
                        .width(36.0)
                        .show_ui(ui, |ui| {
                            let cur = app.state.parameters[i].kind.clone();
                            if ui
                                .selectable_label(
                                    matches!(cur, ParameterKind::Number),
                                    "Number (№)",
                                )
                                .clicked()
                            {
                                app.state.parameters[i].kind = ParameterKind::Number;
                            }
                            if ui
                                .selectable_label(
                                    matches!(cur, ParameterKind::Boolean),
                                    "Boolean (☑)",
                                )
                                .clicked()
                            {
                                app.state.parameters[i].kind = ParameterKind::Boolean;
                                app.state.parameters[i].default =
                                    if app.state.parameters[i].default != 0.0 {
                                        1.0
                                    } else {
                                        0.0
                                    };
                            }
                            if ui
                                .selectable_label(
                                    matches!(cur, ParameterKind::Slider { .. }),
                                    "Slider (⇔)",
                                )
                                .clicked()
                            {
                                app.state.parameters[i].kind =
                                    ParameterKind::Slider { min: 0.0, max: 1.0 };
                            }
                        });

                    // Slider min/max editors
                    if let ParameterKind::Slider { min, max } = &mut app.state.parameters[i].kind {
                        ui.add(
                            egui::DragValue::new(min)
                                .prefix("min:")
                                .speed(0.01)
                                .max_decimals(3),
                        );
                        ui.add(
                            egui::DragValue::new(max)
                                .prefix("max:")
                                .speed(0.01)
                                .max_decimals(3),
                        );
                    }

                    if ui.small_button("×").clicked() {
                        to_remove = Some(i);
                    }
                }
            });
        }

        // Apply rename sync
        if let Some((_, old, new)) = rename {
            sync_param_rename(app, &old, &new);
        }

        if let Some(i) = to_remove {
            app.state.parameters.remove(i);
        }

        if ui.small_button("+ Add").clicked() {
            let n = app.state.parameters.iter().filter(|p| !p.auto).count() + 1;
            app.state.parameters.push(crate::ui::ParameterDecl {
                name: format!("param{n}"),
                default: 0.0,
                kind: ParameterKind::Number,
                auto: false,
            });
        }
    });
}
