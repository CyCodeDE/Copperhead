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
use crate::ui::ComponentBuildData;
use crate::ui::app::{CircuitApp, FileDialogState};
use crate::ui::util::{format_si_single, parse_si};
use copperhead_core::components::diode::DiodeModel;
use copperhead_core::components::transistor::bjt::BjtModel;
use copperhead_core::components::triode::TriodeModel;
use egui::{Align, ComboBox, Id, Sense, StrokeKind, TextEdit, Vec2};

pub fn handle_properties(app: &mut CircuitApp, ctx: &egui::Context) {
    if let Some(id) = app.editing_component_id {
        let mut open = true;

        let mut rename_request: Option<(usize, String)> = None;

        if let Some(comp) = app
            .state
            .schematic
            .components
            .iter_mut()
            .find(|c| c.id == id)
        {
            // break if the component has no properties. For example ground
            if comp.component != ComponentBuildData::Ground {
                app.keybinds_locked = true;

                egui::Modal::new(Id::new("component_edit_modal"))
                    .backdrop_color(app.theme.modal_backdrop)
                    .show(ctx, |ui| {
                        app.keybinds_locked = true;
                        ui.spacing_mut().item_spacing = Vec2::new(10.0, 10.0);

                        let prefix = comp.component.prefix().to_string();

                        ui.heading(format!(
                            "{} Properties",
                            match comp.component {
                                ComponentBuildData::Resistor { .. } => "Resistor",
                                ComponentBuildData::Capacitor { .. } => "Capacitor",
                                ComponentBuildData::DCSource { .. } => "DC Source",
                                ComponentBuildData::ASource { .. } => "AC Source",
                                ComponentBuildData::AudioSource { .. } => "Audio Source",
                                ComponentBuildData::Inductor { .. } => "Inductor",
                                ComponentBuildData::Diode { .. } => "Diode",
                                ComponentBuildData::Bjt { .. } => "Bipolar Junction Transistor",
                                ComponentBuildData::Label => "Label",
                                ComponentBuildData::AudioProbe { .. } => "Audio Probe",
                                ComponentBuildData::Triode { .. } => "Triode",
                                _ => "Component",
                            }
                        ));

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

                        match &mut comp.component {
                            ComponentBuildData::Resistor { resistance } => {
                                ui.horizontal(|ui| {
                                    ui.label("Resistance:");
                                    ui.add(
                                        egui::DragValue::new(resistance)
                                            .speed(10.0)
                                            .range(0.0..=f64::INFINITY)
                                            .suffix("Ω")
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });
                            }
                            ComponentBuildData::Capacitor { capacitance, esr } => {
                                ui.horizontal(|ui| {
                                    ui.label("Capacitance:");
                                    // Use scientific notation for small values
                                    ui.add(
                                        egui::DragValue::new(capacitance)
                                            .suffix("F")
                                            .speed(1e-7)
                                            .range(0.0..=f64::INFINITY)
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });
                                ui.horizontal(|ui| {
                                    ui.label("ESR:");
                                    ui.add(
                                        egui::DragValue::new(esr)
                                            .suffix("Ω")
                                            .speed(1e-4)
                                            .range(0.0..=f64::INFINITY)
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });
                            }
                            ComponentBuildData::DCSource { voltage } => {
                                ui.horizontal(|ui| {
                                    ui.label("Voltage:");
                                    ui.add(
                                        egui::DragValue::new(voltage)
                                            .speed(0.1)
                                            .range(-f64::INFINITY..=f64::INFINITY)
                                            .suffix("V")
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });
                            }
                            ComponentBuildData::ASource {
                                amplitude,
                                frequency,
                            } => {
                                ui.horizontal(|ui| {
                                    ui.label("Voltage:");
                                    ui.add(
                                        egui::DragValue::new(amplitude)
                                            .speed(0.1)
                                            .range(-f64::INFINITY..=f64::INFINITY)
                                            .suffix("V")
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });
                                ui.horizontal(|ui| {
                                    ui.label("Frequency:");
                                    ui.add(
                                        egui::DragValue::new(frequency)
                                            .speed(1.0)
                                            .range(0.0..=f64::INFINITY)
                                            .suffix("Hz")
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });
                            }
                            ComponentBuildData::AudioSource { path } => {
                                if let Some(new_path) = app.temp_audio_path.take() {
                                    *path = new_path;
                                }

                                ui.label("Select input file:");

                                let (rect, response) = ui.allocate_exact_size(
                                    Vec2::new(ui.available_width() / 2., 20.0f32),
                                    Sense::click(),
                                );
                                let is_hovered = response.hovered();
                                let has_dragged_files =
                                    !ui.ctx().input(|i| i.raw.hovered_files.clone()).is_empty();
                                let is_dragged_over = is_hovered && has_dragged_files;

                                let visuals = ui.style().interact(&response);
                                let fill_color = if is_dragged_over {
                                    ui.visuals().selection.bg_fill
                                } else {
                                    visuals.bg_fill
                                };

                                ui.painter().rect(
                                    rect,
                                    6.0,
                                    fill_color,
                                    visuals.bg_stroke,
                                    StrokeKind::Inside,
                                );
                                let display_text = if !path.is_empty() {
                                    format!(
                                        "🎵 {}",
                                        path.file_name().unwrap_or_default().to_string_lossy()
                                    )
                                } else {
                                    "Select File".to_string()
                                };

                                ui.painter().text(
                                    rect.center(),
                                    egui::Align2::CENTER_CENTER,
                                    display_text,
                                    egui::FontId::proportional(14.0),
                                    visuals.text_color(),
                                );

                                if response.clicked()
                                    && app.file_dialog_state == FileDialogState::Closed
                                {
                                    app.file_dialog_state = FileDialogState::LoadAudio;
                                    let tx = app.file_sender.clone();
                                    let ctx_clone = ui.ctx().clone();

                                    // Spawn RFD thread
                                    std::thread::spawn(move || {
                                        let file = rfd::FileDialog::new()
                                            .add_filter("Audio", &["wav", "mp3", "flac"])
                                            .pick_file();
                                        let _ = tx.send(file);
                                        ctx_clone.request_repaint();
                                    });
                                }

                                ui.ctx().input(|i| {
                                    if is_hovered && !i.raw.dropped_files.is_empty() {
                                        if let Some(dropped_file) = i.raw.dropped_files.first() {
                                            if let Some(dropped_path) = &dropped_file.path {
                                                *path = dropped_path.clone();
                                            }
                                        }
                                    }
                                });
                            }
                            ComponentBuildData::AudioProbe { path } => {
                                if let Some(new_path) = app.temp_audio_path.take() {
                                    *path = new_path;
                                }

                                ui.label("Select output path:");

                                let (rect, response) = ui.allocate_exact_size(
                                    Vec2::new(ui.available_width() / 2., 20.0f32),
                                    Sense::click(),
                                );
                                let is_hovered = response.hovered();
                                let has_dragged_files =
                                    !ui.ctx().input(|i| i.raw.hovered_files.clone()).is_empty();
                                let is_dragged_over = is_hovered && has_dragged_files;

                                let visuals = ui.style().interact(&response);
                                let fill_color = if is_dragged_over {
                                    ui.visuals().selection.bg_fill // Highlight color when dragging a file over
                                } else {
                                    visuals.bg_fill // Normal button color
                                };

                                ui.painter().rect(
                                    rect,
                                    6.0,
                                    fill_color,
                                    visuals.bg_stroke,
                                    StrokeKind::Inside,
                                );
                                let display_text = if !path.is_empty() {
                                    format!(
                                        "🎵 {}",
                                        path.file_name().unwrap_or_default().to_string_lossy()
                                    )
                                } else {
                                    "Select File".to_string()
                                };

                                ui.painter().text(
                                    rect.center(),
                                    egui::Align2::CENTER_CENTER,
                                    display_text,
                                    egui::FontId::proportional(14.0),
                                    visuals.text_color(),
                                );

                                if response.clicked()
                                    && app.file_dialog_state == FileDialogState::Closed
                                {
                                    app.file_dialog_state = FileDialogState::SaveAudio;
                                    let tx = app.file_sender.clone();
                                    let ctx_clone = ui.ctx().clone();

                                    // Spawn RFD thread
                                    std::thread::spawn(move || {
                                        let file = rfd::FileDialog::new()
                                            .add_filter("Audio", &["wav"])
                                            .save_file();
                                        let _ = tx.send(file);
                                        ctx_clone.request_repaint();
                                    });
                                }

                                ui.ctx().input(|i| {
                                    if is_hovered && !i.raw.dropped_files.is_empty() {
                                        if let Some(dropped_file) = i.raw.dropped_files.first() {
                                            if let Some(dropped_path) = &dropped_file.path {
                                                // Update the source of truth directly!
                                                *path = dropped_path.clone();
                                            }
                                        }
                                    }
                                });
                            }
                            ComponentBuildData::Ground => {
                                open = false;
                            }
                            ComponentBuildData::Inductor {
                                inductance,
                                series_resistance,
                            } => {
                                ui.horizontal(|ui| {
                                    ui.label("Inductance:");
                                    ui.add(
                                        egui::DragValue::new(inductance)
                                            .suffix("H")
                                            .speed(1e-4)
                                            .range(0.0..=f64::INFINITY)
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });

                                ui.horizontal(|ui| {
                                    ui.label("Series Resistance:");
                                    ui.add(
                                        egui::DragValue::new(series_resistance)
                                            .suffix("Ω")
                                            .speed(1e-4)
                                            .range(0.0..=f64::INFINITY)
                                            .custom_formatter(|val, _range| {
                                                format_si_single(val, 3)
                                            })
                                            .custom_parser(|text| parse_si(text)),
                                    );
                                });
                            }
                            ComponentBuildData::Label => {}
                            ComponentBuildData::Diode { model } => {
                                ui.horizontal(|ui| {
                                    ui.label("Model:");
                                    ComboBox::from_id_salt("diode_combo")
                                        .selected_text(model.format_name())
                                        .show_ui(ui, |ui| {
                                            ui.selectable_value(
                                                &mut *model,
                                                DiodeModel::D1N4148,
                                                DiodeModel::D1N4148.format_name(),
                                            );
                                        });
                                });
                            }
                            ComponentBuildData::Bjt { model } => {
                                ui.horizontal(|ui| {
                                    ui.label("Model:");
                                    ComboBox::from_id_salt("bjt_combo")
                                        .selected_text(model.format_name())
                                        .show_ui(ui, |ui| {
                                            ui.selectable_value(
                                                &mut *model,
                                                BjtModel::GenericNPN,
                                                BjtModel::GenericNPN.format_name(),
                                            );
                                            ui.selectable_value(
                                                &mut *model,
                                                BjtModel::GenericPNP,
                                                BjtModel::GenericPNP.format_name(),
                                            );
                                        });
                                });
                            }
                            ComponentBuildData::Triode { model } => {
                                ui.horizontal(|ui| {
                                    ui.label("Model:");
                                    ComboBox::from_id_salt("triode_combo")
                                        .selected_text(model.format_name())
                                        .show_ui(ui, |ui| {
                                            ui.selectable_value(
                                                &mut *model,
                                                TriodeModel::T12AX7,
                                                TriodeModel::T12AX7.format_name(),
                                            );
                                        });
                                });
                            }
                            _ => {
                                ui.label("No editable properties for this component.");
                            }
                        }

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
