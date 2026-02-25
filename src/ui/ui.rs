/*
 * Copyright (c) 2026-2026 CyCode and the Copperhead contributors
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
use crate::ui::app::{CircuitApp, Tool};
use crate::ui::components::oscilloscope::draw_oscilloscope;
use egui::{CornerRadius, Frame, Id, Key, Margin};

impl Tool {
    pub(crate) fn get_name(&self) -> &str {
        match self {
            Tool::Select => "Select",
            Tool::PlaceComponent(_) => "Place Component",
            Tool::PlaceWire(_) => "Wire",
            Tool::Erase => "Erase",
        }
    }
}

impl eframe::App for CircuitApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        self.process_messages(ctx);

        self.handle_global_shortcuts(ctx);

        crate::ui::components::top_bar::show(self, ctx);

        crate::ui::components::side_panel::show(self, ctx);

        let show_voltage = self.plotting_node_voltage.is_some();
        let show_current = self.plotting_component_current.is_some();

        if show_voltage || show_current {
            egui::TopBottomPanel::top("plot_panel")
                .default_height(300.0)
                .min_height(150.0)
                .max_height(600.)
                .resizable(true)
                .frame(Frame::new().fill(self.theme.panel_color))
                .show(ctx, |ui| {
                    let island_frame = Frame::default()
                        .fill(self.theme.background)
                        .corner_radius(CornerRadius {
                            nw: 0,
                            ne: 0,
                            sw: 20,
                            se: 0,
                        })
                        .stroke(egui::Stroke::new(1.0, self.theme.panel_border)) // Subtle border
                        .inner_margin(Margin {
                            top: 0,
                            right: 0,
                            bottom: 0,
                            left: 0,
                        })
                        .outer_margin(Margin {
                            top: 10,
                            left: 10,
                            right: 0,
                            bottom: 0
                        });

                    island_frame.show(ui, |ui| {
                        let header_frame = Frame::default().inner_margin(Margin { top: 4, left: 8, right: 8, bottom: 4 }).fill(self.theme.panel_color).stroke(egui::Stroke::NONE).corner_radius(0);
                        header_frame.show(ui, |ui| {
                            ui.horizontal(|ui| {
                                ui.heading("Oscilloscope");
                                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                                    if ui.button("❌").clicked() || ui.input(|i| i.key_pressed(Key::Escape)) {
                                        self.plotting_node_voltage = None;
                                        self.plotting_component_current = None;
                                    }
                                    ui.separator();
                                    ui.label(egui::RichText::new("Drag: Pan | Auto-Size: Double Click | Scroll: Zoom | Drag Left click: Box Zoom | Alt+Scroll: Y-Zoom | Shift+Scroll: X-Pan").italics().weak().size(10.0));
                                });
                            });
                        });

                        ui.add_space(4.0);

                        if self.sim_state.history.is_empty() {
                            ui.label("No data available");
                            return;
                        }

                        draw_oscilloscope(ui, &mut self.scope_state, &self.sim_state, self.plotting_node_voltage.map(|id| id.0), self.plotting_component_current);

                        ctx.request_repaint();
                    });
                });
        }

        let running = self.sim_state.running;

        if ctx
            .data(|d| d.get_temp::<bool>(Id::new("label_tool_open")))
            .unwrap_or(false)
        {
            egui::Modal::new(Id::new("label_tool_window")).show(ctx, |ui| {
                ui.label("Enter label text:");
                // create a text edit that uses the data to store the label text
                let mut label_text = ctx.data(|d| {
                    d.get_temp::<String>(Id::new("label_tool_text"))
                        .unwrap_or_default()
                });
                let text_edit = ui.text_edit_singleline(&mut label_text);
                text_edit.request_focus();
                ctx.data_mut(|d| d.insert_temp(Id::new("label_tool_text"), label_text.clone()));

                ui.horizontal(|ui| {
                    if ui.button("Add Label").clicked() {
                        let label_text = ctx
                            .data(|d| d.get_temp::<String>(Id::new("label_tool_text")))
                            .unwrap_or_default();
                        if !label_text.is_empty() {
                            self.selected_tool = Tool::PlaceComponent(ComponentBuildData::Label);
                            // clear the stored text
                            // update the stored text to the final value
                            ctx.data_mut(|d| {
                                d.insert_temp::<String>(Id::new("label_tool_text"), label_text)
                            });
                            // close the modal
                            ctx.data_mut(|d| d.remove::<bool>(Id::new("label_tool_open")));
                            self.keybinds_locked = false;
                        }
                    }

                    if ui.button("Close").clicked() || ui.input(|i| i.key_pressed(Key::Escape)) {
                        ctx.data_mut(|d| d.remove::<bool>(Id::new("label_tool_open")));
                        self.keybinds_locked = false;
                    }
                });
            });
        }

        crate::ui::components::canvas::show(self, ctx);

        // check for repaint, full redraw next frame if sim is running and 30fps if not
        if running {
            ctx.request_repaint();
        } else {
            ctx.request_repaint_after(std::time::Duration::from_millis(33));
        }
    }
}
