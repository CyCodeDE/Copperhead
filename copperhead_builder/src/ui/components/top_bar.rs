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
use crate::ui::app::{CircuitApp, FileDialogState};
use crate::ui::netlist::compile_netlist;
use crate::ui::util::get_default_path;
use egui::{Align, Button, Frame, Label, Layout, Vec2, ViewportCommand};
use std::thread;

pub fn show(app: &mut CircuitApp, ctx: &egui::Context) {
    let running = app.sim_state.running;
    // menu bar
    egui::TopBottomPanel::top("menu_bar")
        .frame(Frame::new().inner_margin(8.).fill(app.theme.panel_color))
        .show(ctx, |ui| {
            ui.columns(3, |columns| {
                columns[0].horizontal(|ui| {
                    ui.menu_button("File", |ui| {
                        if ui.button("New Schematic").clicked() {
                            app.state.schematic = Default::default();
                            app.active_netlist = None;
                            app.undo_stack.clear();
                            ui.close_menu();
                            ctx.send_viewport_cmd(ViewportCommand::Title(
                                "Copperhead - Untitled".to_string(),
                            ));
                            app.current_file = None;
                        }

                        if ui
                            .add_enabled(
                                app.file_dialog_state == FileDialogState::Closed,
                                Button::new("Open Schematic"),
                            )
                            .clicked()
                        {
                            let default_path = get_default_path();

                            app.file_dialog_state = FileDialogState::LoadSchem;
                            let tx = app.file_sender.clone();
                            let ctx_clone = ctx.clone();

                            thread::spawn(move || {
                                let file = rfd::FileDialog::new()
                                    .set_directory(default_path)
                                    .pick_file();
                                let _ = tx.send(file);
                                ctx_clone.request_repaint();
                            });
                        }

                        if ui
                            .add_enabled(
                                app.file_dialog_state == FileDialogState::Closed,
                                Button::new("Save"),
                            )
                            .clicked()
                        {
                            let default_path = get_default_path();

                            app.file_dialog_state = FileDialogState::SaveSchem;
                            let tx = app.file_sender.clone();
                            let ctx_clone = ctx.clone();

                            thread::spawn(move || {
                                let file = rfd::FileDialog::new()
                                    .set_directory(default_path)
                                    .save_file();
                                let _ = tx.send(file);
                                ctx_clone.request_repaint();
                            });
                        }

                        if ui.button("Quit").clicked() {
                            ui.ctx().send_viewport_cmd(ViewportCommand::Close);
                        }
                    });

                    ui.menu_button("Edit", |ui| {
                        if ui.add_enabled(!running, Button::new("Undo")).clicked() {
                            if let Some(prev) = app.undo_stack.undo(app.state.clone()) {
                                app.state = prev;
                            }
                        }
                        if ui.add_enabled(!running, Button::new("Redo")).clicked() {
                            if let Some(next) = app.undo_stack.redo(app.state.clone()) {
                                app.state = next;
                            }
                        }
                    });

                    ui.menu_button("View", |ui| {
                        if ui.button("Reset View").clicked() {
                            app.pan = Vec2::ZERO;
                            app.zoom = 50.0;
                            ui.close_menu();
                        }
                    });

                    ui.menu_button("Help", |ui| {
                        if ui.button("About").clicked() {
                            ui.close_menu();
                        }
                        if ui.button("GitHub").clicked() {
                            ui.close_menu();
                            if let Err(err) = open::that("https://github.com/CyCodeDE/Copperhead") {
                                eprintln!("Failed to open GitHub page: {}", err);
                            }
                        }
                    });
                });

                let ui = &mut columns[1];

                // Determine Button Text and Logic based on 'running' state
                let btn_text = if running { "⏹ Stop" } else { "▶ Start" };
                let label_text = "Sim Time:";

                // Measure widths to perform manual centering
                let item_spacing = ui.spacing().item_spacing.x;
                let button_padding = ui.spacing().button_padding.x * 2.0;

                // Measure Button
                let btn_font_id = egui::TextStyle::Button.resolve(ui.style());
                let btn_text_width = ui.fonts_mut(|f| {
                    f.layout_no_wrap(btn_text.to_string(), btn_font_id, egui::Color32::WHITE)
                        .rect
                        .width()
                });
                let btn_total_width = btn_text_width + button_padding;

                // Measure Label
                let label_font_id = egui::TextStyle::Body.resolve(ui.style());
                let label_width = ui.fonts_mut(|f| {
                    f.layout_no_wrap(label_text.to_string(), label_font_id, egui::Color32::WHITE)
                        .rect
                        .width()
                });

                // Set a fixed width for the input box so our math is reliable
                let input_width = 70.0;

                // Calculate offset
                // Total = Label + Spacing + Input + Spacing + Button
                let total_content_width =
                    label_width + item_spacing + input_width + item_spacing + btn_total_width;

                let available_width = ui.available_width();
                let offset = (available_width - total_content_width) / 2.0;

                ui.horizontal(|ui| {
                    // Apply manual centering offset
                    if offset > 0.0 {
                        ui.add_space(offset);
                    }

                    // Simulation Time Input
                    ui.add_enabled(
                        !app.realtime_mode && !running,
                        Label::new(label_text).selectable(false),
                    );

                    let response = ui.add_enabled(
                        !app.realtime_mode && !running,
                        egui::DragValue::new(&mut app.state.simulation_time)
                            .speed(1)
                            .clamp_range(0.001..=1000.0)
                            .max_decimals(3)
                            .suffix("s"),
                    );

                    // Snapshot logic for Undo/Redo
                    if response.drag_started() || response.gained_focus() {
                        app.temp_state_snapshot = Some(app.state.clone());
                    }

                    if response.drag_stopped() || response.lost_focus() {
                        if let Some(prev) = app.temp_state_snapshot.take() {
                            if prev.simulation_time != app.state.simulation_time {
                                app.undo_stack.push(prev);
                            }
                        }
                    }

                    // Unified Start/Stop Button
                    if ui.button(btn_text).clicked() {
                        if running {
                            // Stop Logic
                            app.tx_command.send(SimCommand::Pause).unwrap();
                        } else {
                            // Start Logic
                            let netlist = compile_netlist(&app);
                            app.active_netlist = Some(netlist.clone());

                            app.tx_command
                                .send(SimCommand::SetRunTime(app.state.simulation_time))
                                .unwrap();
                            app.tx_command
                                .send(SimCommand::LoadCircuit(netlist))
                                .unwrap();
                            app.tx_command.send(SimCommand::Resume).unwrap();
                        }
                    }
                });

                columns[2].with_layout(Layout::right_to_left(Align::Center), |ui| {
                    ui.label(format!("Copperhead v{}", env!("CARGO_PKG_VERSION")));
                });
            });
        });
}
