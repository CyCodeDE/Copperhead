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

use crate::components::diode::DiodeModel;
use crate::components::transistor::bjt::BjtModel;
use crate::model::GridPos;
use crate::ui::app::{CircuitApp, FileDialogState, StateUpdate, Tool};
use crate::ui::components::oscilloscope::draw_oscilloscope;
use crate::ui::drawing::{
    check_line_rect_intersection, draw_component, draw_component_labels, draw_grid,
};
use crate::ui::{
    CircuitSelection, ComponentBuildData, SimCommand, SimStepData, VisualComponent, VisualWire,
    handle_circuit_loaded, lerp_color,
};
use crate::util::{format_si_single, get_default_path, parse_si};
use egui::text::LayoutJob;
use egui::{
    Align, Align2, Button, CentralPanel, Checkbox, Color32, ComboBox, CornerRadius, CursorIcon,
    Direction, FontSelection, Frame, Id, Key, Label, Layout, Margin, Modal, Modifiers, Painter,
    Pos2, Rect, RichText, Sense, Separator, Shape, Stroke, StrokeKind, Style, TextEdit, TextFormat,
    TopBottomPanel, UiBuilder, Vec2, Vec2b, ViewportCommand, WidgetText, vec2,
};
use egui_plot::{Line, Plot, PlotPoints};
use faer::prelude::default;
use log::{debug, info};
use std::ops::Add;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;

impl Tool {
    fn get_name(&self) -> &str {
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
        while let Ok(msg) = self.state_receiver.try_recv() {
            match msg {
                StateUpdate::UpdateRunning(is_running) => {
                    self.sim_state.running = is_running;
                }
                StateUpdate::SendHistory(mut batch, current_sample) => {
                    let nodes = batch.nodes_per_step;
                    let terms = batch.terminals_per_step;
                    let obs = batch.observables_per_step;

                    // Reconstruct SimStepData
                    for i in 0..batch.times.len() {
                        let step = SimStepData {
                            time: batch.times[i],
                            voltages: batch.voltages[i * nodes..(i + 1) * nodes].to_vec(),
                            currents: batch.currents[i * terms..(i + 1) * terms].to_vec(),
                            observables: batch.observables[i * obs..(i + 1) * obs].to_vec(),
                        };
                        self.sim_state.history.push(step);
                    }

                    self.sim_state.current_sample = current_sample;

                    batch.clear_for_reuse();
                    let _ = self.recycle_tx.try_send(batch);
                }
                StateUpdate::ClearHistory => {
                    self.sim_state.history.clear();
                }
                StateUpdate::CircuitLoaded(metadata) => {
                    self.sim_state.lookup_map = handle_circuit_loaded(&metadata);
                    self.sim_state.metadata = Some(metadata);
                }
            }
        }

        let running = self.sim_state.running;

        if let Ok(result) = self.file_receiver.try_recv() {
            if let Some(path) = result {
                match self.file_dialog_state {
                    FileDialogState::LoadSchem => {
                        self.load_from_path(path);
                        self.active_netlist = None;
                        self.undo_stack.clear();
                        ctx.send_viewport_cmd(ViewportCommand::Title(match self.current_file {
                            Some(ref p) => format!(
                                "Copperhead - {}",
                                // get file name only (no path, no extension)
                                p.file_stem().and_then(|s| s.to_str()).unwrap_or("Untitled")
                            ),
                            None => "Copperhead - Untitled".to_string(),
                        }));
                    }
                    FileDialogState::SaveSchem => {
                        self.save_to_path(path);
                    }
                    FileDialogState::LoadAudio => {
                        self.temp_audio_path = Some(path);
                    }
                    FileDialogState::SaveAudio => {
                        self.temp_audio_path = Some(path);
                    }
                    _ => {}
                }
            } else {
                // User cancelled the dialog
                debug!("File dialog was cancelled by the user.");
            }

            self.file_dialog_state = FileDialogState::Closed;
        }

        if !running {
            // Handle undo and redo only if not running
            if ctx.input_mut(|i| i.consume_key(Modifiers::COMMAND, Key::Z)) && !self.keybinds_locked
            {
                if let Some(prev) = self.undo_stack.undo(self.state.clone()) {
                    self.state = prev;
                    // TODO: maybe show a notification that undo was performed
                }
            }
            if ctx.input_mut(|i| i.consume_key(Modifiers::COMMAND, Key::Y)) && !self.keybinds_locked
            {
                if let Some(next) = self.undo_stack.redo(self.state.clone()) {
                    self.state = next;
                }
            }
        }

        // Handle Escape to cancel tool
        if ctx.input(|i| i.key_pressed(Key::Escape)) {
            self.selected_tool = Tool::Select;
        }

        // Handle Rotate (CTRL + R)
        if ctx.input(|i| i.modifiers.command && i.key_pressed(Key::R)) {
            self.current_rotation = (self.current_rotation + 1) % 4;
        }

        // menu bar
        egui::TopBottomPanel::top("menu_bar")
            .frame(Frame::new().inner_margin(8.).fill(self.theme.panel_color))
            .show(ctx, |ui| {
                ui.columns(3, |columns| {
                    columns[0].horizontal(|ui| {
                        ui.menu_button("File", |ui| {
                            if ui.button("New Schematic").clicked() {
                                self.state.schematic = Default::default();
                                self.active_netlist = None;
                                self.undo_stack.clear();
                                ui.close_menu();
                                ctx.send_viewport_cmd(ViewportCommand::Title(
                                    "Copperhead - Untitled".to_string(),
                                ));
                                self.current_file = None;
                            }

                            if ui
                                .add_enabled(
                                    self.file_dialog_state == FileDialogState::Closed,
                                    Button::new("Open Schematic"),
                                )
                                .clicked()
                            {
                                let default_path = get_default_path();

                                self.file_dialog_state = FileDialogState::LoadSchem;
                                let tx = self.file_sender.clone();
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
                                    self.file_dialog_state == FileDialogState::Closed,
                                    Button::new("Save"),
                                )
                                .clicked()
                            {
                                let default_path = get_default_path();

                                self.file_dialog_state = FileDialogState::SaveSchem;
                                let tx = self.file_sender.clone();
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
                                if let Some(prev) = self.undo_stack.undo(self.state.clone()) {
                                    self.state = prev;
                                }
                            }
                            if ui.add_enabled(!running, Button::new("Redo")).clicked() {
                                if let Some(next) = self.undo_stack.redo(self.state.clone()) {
                                    self.state = next;
                                }
                            }
                        });

                        ui.menu_button("View", |ui| {
                            if ui.button("Reset View").clicked() {
                                self.pan = Vec2::ZERO;
                                self.zoom = 50.0;
                                ui.close_menu();
                            }
                        });

                        ui.menu_button("Help", |ui| {
                            if ui.button("About").clicked() {
                                ui.close_menu();
                            }
                            if ui.button("GitHub").clicked() {
                                ui.close_menu();
                                if let Err(err) =
                                    open::that("https://github.com/CyCodeDE/Copperhead")
                                {
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
                        f.layout_no_wrap(
                            label_text.to_string(),
                            label_font_id,
                            egui::Color32::WHITE,
                        )
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
                            !self.realtime_mode && !running,
                            Label::new(label_text).selectable(false),
                        );

                        let response = ui.add_enabled(
                            !self.realtime_mode && !running,
                            egui::DragValue::new(&mut self.state.simulation_time)
                                .speed(1)
                                .clamp_range(0.001..=1000.0)
                                .max_decimals(3)
                                .suffix("s"),
                        );

                        // Snapshot logic for Undo/Redo
                        if response.drag_started() || response.gained_focus() {
                            self.temp_state_snapshot = Some(self.state.clone());
                        }

                        if response.drag_stopped() || response.lost_focus() {
                            if let Some(prev) = self.temp_state_snapshot.take() {
                                if prev.simulation_time != self.state.simulation_time {
                                    self.undo_stack.push(prev);
                                }
                            }
                        }

                        // Unified Start/Stop Button
                        if ui.button(btn_text).clicked() {
                            if running {
                                // Stop Logic
                                self.tx_command.send(SimCommand::Pause).unwrap();
                            } else {
                                // Start Logic
                                let netlist = self.compile_netlist();
                                self.active_netlist = Some(netlist.clone());
                                debug!("Compiled Netlist: {:?}", netlist);

                                self.tx_command
                                    .send(SimCommand::SetRunTime(self.state.simulation_time))
                                    .unwrap();
                                self.tx_command
                                    .send(SimCommand::LoadCircuit(netlist))
                                    .unwrap();
                                self.tx_command.send(SimCommand::Resume).unwrap();
                            }
                        }
                    });

                    columns[2].with_layout(Layout::right_to_left(Align::Center), |ui| {
                        ui.label(format!("Copperhead v{}", env!("CARGO_PKG_VERSION")));
                    });
                });
            });

        egui::SidePanel::left("tools")
            .frame(
                Frame::new()
                    .inner_margin(8.)
                    .fill(self.theme.panel_color),
            )
            .resizable(false)
            .show(ctx, |ui| {
                if ui
                    .add_enabled(
                        !running,
                        Checkbox::new(&mut self.realtime_mode, "Real-Time Simulation"),
                    )
                    .clicked()
                {
                        // TODO: handle this better
                        // Command should be queued and processed by the sim thread when it is safe to do so instead of completely dropping it
                        self.tx_command
                            .send(SimCommand::SetRealtime(self.realtime_mode))
                            .unwrap();
                };

                ui.heading("Tools");
                ui.add(Label::new("Controls:\nMB to Pan\nScroll to Zoom\n'CTRL+R' to Rotate\n'Esc and RMB' to Cancel").selectable(false));
                ui.separator();

                if ui.button("Select (S)").clicked() || ui.input(|i| i.key_pressed(Key::S) && !self.keybinds_locked) {
                    self.selected_tool = Tool::Select;
                }
                if ui.button("Erase (E/DEL)").clicked()
                    || ui.input(|i| i.key_pressed(Key::Delete) || i.key_pressed(Key::E) && !self.keybinds_locked)
                {
                    self.selected_tool = Tool::Erase;
                }
                ui.separator();

                // TODO: make a grid with icons for each component type instead of label buttons

                // Component Buttons
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceWire(_)), Button::new("Draw Wire (W)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::W)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool = Tool::PlaceWire(None);
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::Resistor { resistance: _ })), Button::new("Resistor (R)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::R)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool =
                        Tool::PlaceComponent(ComponentBuildData::Resistor { resistance: 1000.0 }); // 1k Ohm
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::Capacitor { capacitance: _, esr: _ })), Button::new("Capacitor (C)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::C)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool =
                        Tool::PlaceComponent(ComponentBuildData::Capacitor { capacitance: 1e-6, esr: 0f64 }); // 1 uF
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::Inductor { inductance: _, series_resistance: _ })), Button::new("Inductor (L)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::L)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool =
                        Tool::PlaceComponent(ComponentBuildData::Inductor { inductance: 1e-3, series_resistance: 0f64 }); // 1 mH
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::Diode { model: _ })), Button::new("Diode (D)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::D)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool = Tool::PlaceComponent(ComponentBuildData::Diode {
                        model: DiodeModel::D1N4148,
                    });
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::DCSource { voltage: _ })), Button::new("DC Source (Y)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::Y)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool =
                        Tool::PlaceComponent(ComponentBuildData::DCSource { voltage: 5.0 }); // 5V default
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::ASource { amplitude: _, frequency: _ })), Button::new("AC Source (A)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::A)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool = Tool::PlaceComponent(ComponentBuildData::ASource {
                        amplitude: 5.0,
                        frequency: 60.0,
                    }); // 5V, 60Hz default
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::AudioSource { path: _ })), Button::new("Audio Source")).clicked()
                {
                    self.selected_tool = Tool::PlaceComponent(ComponentBuildData::AudioSource { path: PathBuf::new() });
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::Ground)), Button::new("Ground (G)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::G)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    self.selected_tool = Tool::PlaceComponent(ComponentBuildData::Ground);
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::Label)), Button::new("Label (N)")).clicked()
                    || (ui.input(|i| i.key_pressed(Key::N)) && ui.input(|i| i.modifiers.is_none()) && !self.keybinds_locked)
                {
                    // set a variable to determine that a modal should be opened until closed
                    ui.memory_mut(|mem| mem.data.insert_temp(Id::new("label_tool_open"), true));
                    self.keybinds_locked = true;
                }
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::Bjt { model: _ })), Button::new("Bjt")).clicked()
                {
                    self.selected_tool = Tool::PlaceComponent(ComponentBuildData::Bjt { model: BjtModel::GenericNPN });
                }
                ui.separator();
                if ui.add_enabled(!matches!(self.selected_tool, Tool::PlaceComponent(ComponentBuildData::AudioProbe { path: _ })), Button::new("Audio Probe")).clicked()
                {
                    self.selected_tool = Tool::PlaceComponent(ComponentBuildData::AudioProbe { path: PathBuf::new() });
                }

                ui.separator();
                ui.label(format!("State: {:?}", self.selected_tool.get_name()));
            });

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

        // Central Canvas
        egui::CentralPanel::default().frame(Frame::new().fill(self.theme.panel_color)).show(ctx, |ui| {
            let island_frame = egui::Frame::default()
                .fill(self.theme.background)
                .corner_radius(CornerRadius {
                    nw: 20,
                    ne: 0,
                    sw: 0,
                    se: 0,
                })
                .stroke(egui::Stroke::new(1.0, self.theme.panel_border)) // Subtle border
                .inner_margin(0.)
                .outer_margin(Margin {
                    top: 10,
                    left: 10,
                    right: 0,
                    bottom: 0
                });

            island_frame.show(ui, |ui| {
                if !self.is_initialized {
                    let rect = ui.available_rect_before_wrap();
                    let center = rect.center();
                    self.pan = center.to_vec2();
                    self.is_initialized = true;
                }

                let (response, painter) = ui.allocate_painter(
                    ui.available_size_before_wrap(),
                    Sense::click_and_drag()
                );

                let rect = response.rect;

                // Pan & Zoom Logic
                if response.dragged_by(egui::PointerButton::Middle) || response.dragged_by(egui::PointerButton::Primary) && matches!(self.selected_tool, Tool::Select) {
                    self.pan += response.drag_delta();
                }

                let scroll = ui.input(|i| i.smooth_scroll_delta);
                if scroll.y != 0.0 {
                    let zoom_factor = if scroll.y > 0.0 { 1.05 } else { 0.95 };
                    let new_zoom = (self.zoom * zoom_factor).clamp(5.0, 200.0);
                    if let Some(mouse_pos) = response.hover_pos() {
                        let mouse_world = (mouse_pos - self.pan) / self.zoom;
                        self.zoom = new_zoom;
                        self.pan = mouse_pos - (mouse_world * self.zoom);
                    }
                }

                // Draw Grid
                draw_grid(&painter, rect, self.zoom, self.pan, self.theme.dot_color);

                // Draw Existing Components
                for comp in &self.state.schematic.components {
                    draw_component(
                        &painter,
                        comp,
                        |p| self.to_screen(p),
                        self.zoom,
                        self.theme.component_body.blend(self.theme.disabled_text_color),
                        self.theme.component_body
                    );

                    draw_component_labels(
                        &comp,
                        &painter,
                        |p| self.to_screen(p),
                        self.zoom,
                    );
                }

                const voltage_sensitivity: f64 = 5.0;

                // Draw Wires (really naive rn)
                if running {
                    if let Some(netlist) = &self.active_netlist {
                        for (coord, &node_id) in &netlist.node_map {
                            // calculate RMS
                            let (sum_sq, count) = self.sim_state.history.iter().rev().take(8000)
                                .filter_map(|step| step.voltages.get(node_id.0))
                                .fold((0.0, 0), |(acc_sq, acc_cnt), &v| {
                                    (acc_sq + (v * v), acc_cnt + 1)
                                });

                            let color = if count > 0 {
                                let rms = (sum_sq / count as f64).sqrt();
                                let t = (rms / 5.0) as f32; // 5.0 is sensitivity
                                lerp_color(self.theme.wire_off, self.theme.wire_on, t)
                            } else {
                                self.theme.wire_off
                            };

                            self.wire_color_cache.insert(node_id, color);
                        }
                    }
                        // get latest sim state
                        for wire in &self.state.schematic.wires {
                            let start = self.to_screen(wire.start);
                            let end = self.to_screen(wire.end);

                            let mut color = self.theme.wire_off;
                            let mut stroke_width = 2.0;

                            if let Some(netlist) = &self.active_netlist {
                                if let Some(&node_id) = netlist.node_map.get(&wire.start) {
                                    if let Some(cached_color) = self.wire_color_cache.get(&node_id) {
                                        color = *cached_color;

                                        if color != self.theme.wire_off {
                                            if color.r() > self.theme.wire_off.r() + 10 {
                                                stroke_width = 2.5;
                                            }
                                        }
                                    }
                                }
                            }

                            painter.line_segment([start, end], Stroke::new(stroke_width, color));
                        }
                } else {
                    for wire in &self.state.schematic.wires {
                        let start = self.to_screen(wire.start);
                        let end = self.to_screen(wire.end);
                        painter.line_segment([start, end], Stroke::new(2.0, self.theme.wire_off));
                    }
                }

                // Tool Interaction & Ghost Drawing
                if let Some(mouse_pos) = response.hover_pos() {
                    let grid_pos = self.to_grid(mouse_pos);
                    let snap_pos = self.to_screen(grid_pos);

                    // Highlight grid point under mouse
                    painter.circle_filled(snap_pos, 3.0, Color32::from_white_alpha(50));

                    match &self.selected_tool {
                        Tool::PlaceComponent(comp_data) => {
                            let name = match comp_data {
                                ComponentBuildData::Label => {
                                    let name = ctx.data(|d| d.get_temp::<String>(Id::new("label_tool_text")).unwrap_or_default()).clone();
                                    name
                                }
                                _ => self.state.schematic.generate_next_name(&comp_data.prefix()),
                            };

                            // calculate size in grid units depending on the type of component
                            let size = match comp_data {
                                ComponentBuildData::Resistor { .. } |
                                ComponentBuildData::Capacitor { .. } |
                                ComponentBuildData::Inductor { .. } |
                                ComponentBuildData::Diode { .. } |
                                ComponentBuildData::DCSource { .. } |
                                ComponentBuildData::ASource { .. } |
                                ComponentBuildData::AudioSource { .. } => GridPos { x: 2, y: 1 },

                                ComponentBuildData::Ground |
                                ComponentBuildData::Label |
                                ComponentBuildData::AudioProbe { .. } => GridPos { x: 1, y: 1 },

                                ComponentBuildData::Bjt { .. } => GridPos { x: 2, y: 2}, // a bjt has pins at these positions: vec![(1, 1), (-1, 0), (1, -1)]
                            };

                            let rotated_size = match self.current_rotation % 4 {
                                0 | 2 => size,
                                1 | 3 => GridPos { x: size.y, y: size.x },
                                _ => unreachable!(),
                            };

                            let ghost_comp = VisualComponent {
                                name: name.clone(),
                                id: 0,
                                component: comp_data.clone(),
                                pos: grid_pos,
                                size: rotated_size,
                                rotation: self.current_rotation,
                            };

                            // Draw Ghost
                            draw_component(
                                &painter,
                                &ghost_comp,
                                |p| self.to_screen(p),
                                self.zoom,
                                Color32::from_white_alpha(30),
                                Color32::from_white_alpha(150)
                            );

                            // Handle Click to Place
                            if response.clicked_by(egui::PointerButton::Primary) {
                                self.undo_stack.push(self.state.clone());
                                self.state.schematic.add_component_with_name(
                                    comp_data.clone(),
                                    grid_pos,
                                    self.current_rotation,
                                    name,
                                    rotated_size,
                                );

                                self.current_rotation = 0;

                                if ui.input(|i| i.modifiers.shift) {
                                    // Keep placing the same component
                                } else {
                                    if matches!(comp_data, ComponentBuildData::Label) {
                                        ctx.data_mut(|d| d.remove_temp::<String>(Id::new("label_tool_text")));
                                    }
                                    self.selected_tool = Tool::Select;
                                }
                            }

                            if response.clicked_by(egui::PointerButton::Secondary) {
                                self.selected_tool = Tool::Select
                            }
                        },
                        Tool::PlaceWire(start_node) => {
                            painter.circle_stroke(snap_pos, 4.0, Stroke::new(2.0, Color32::GREEN));

                            if let Some(start) = start_node {
                                // Start point is set, dragging to End point

                                // Calculate L-Shape Routing (Manhattan)
                                let knee = GridPos { x: grid_pos.x, y: start.y };

                                let mut ghost_segments = Vec::new();

                                if start.x != knee.x {
                                    ghost_segments.push(VisualWire { start: *start, end: knee });
                                }

                                if knee.y != grid_pos.y {
                                    ghost_segments.push(VisualWire { start: knee, end: grid_pos });
                                }

                                // Draw Ghost Wires
                                for wire in &ghost_segments {
                                    let w_start = self.to_screen(wire.start);
                                    let w_end = self.to_screen(wire.end);
                                    // Draw dashed or semi-transparent line
                                    painter.line_segment(
                                        [w_start, w_end],
                                        Stroke::new(2.0, Color32::from_rgba_unmultiplied(0, 255, 0, 128))
                                    );
                                }

                                // Handle Second Click (Commit)
                                if response.clicked_by(egui::PointerButton::Primary) {
                                    self.undo_stack.push(self.state.clone());
                                    // Add generated segments to the schematic
                                    self.state.schematic.wires.extend(ghost_segments);

                                    // Reset to wait for a new wire
                                    self.selected_tool = Tool::PlaceWire(None);
                                    // Alternative option: set new wire at end point of current wire
                                    // self.selected_tool = Tool::PlaceWire(Some(grid_pos));
                                }

                            } else {
                                // Waiting for first click to set Start point
                                if response.clicked_by(egui::PointerButton::Primary) {
                                    self.selected_tool = Tool::PlaceWire(Some(grid_pos));
                                }
                            }

                            // Cancel on Right Click
                            if response.clicked_by(egui::PointerButton::Secondary) {
                                self.selected_tool = Tool::Select;
                            }
                        },

                        Tool::Select => {
                            if self.editing_component_id.is_none() && response.clicked_by(egui::PointerButton::Secondary) {
                                let mut found_id = None;

                                for comp in &self.state.schematic.components {
                                    let comp_screen_pos = self.to_screen(comp.pos);
                                    // add a small tolerance to the size for easier selection
                                    let size = (comp.size.to_vec2() + Vec2::splat(0.5)) * self.zoom;
                                    let rect = Rect::from_center_size(comp_screen_pos, size);

                                    if rect.contains(mouse_pos) {
                                        found_id = Some(comp.id);
                                        break;
                                    }
                                }

                                if let Some(id) = found_id {
                                    self.editing_component_id = Some(id);
                                }
                            }

                            if let Some(mouse_pos) = response.hover_pos() {
                                if let Some(netlist) = &self.active_netlist {
                                    let mut hit_pin = None;
                                    let mut hit_body = None;

                                    for comp in &self.state.schematic.components {
                                        let comp_screen_pos = self.to_screen(comp.pos);

                                        let pins = comp.get_pin_locations();
                                        for (terminal_index, pin_pos) in pins.iter().enumerate() {
                                            let pin_screen_pos = self.to_screen(*pin_pos);
                                            if pin_screen_pos.distance(mouse_pos) < 10.0 {
                                                hit_pin = Some((comp, terminal_index));
                                                break;
                                            }
                                        }

                                        if hit_pin.is_some() {
                                            break;
                                        }

                                        if hit_body.is_none() {
                                            let size = Vec2::new(2.0 * self.zoom, 1.0 * self.zoom);
                                            let rect = Rect::from_center_size(comp_screen_pos, size);

                                            if rect.contains(mouse_pos) {
                                                hit_body = Some(comp);
                                            }
                                        }
                                    }

                                    // Order: Pin -> Wire -> Body

                                    if let Some((comp, terminal_index)) = hit_pin {
                                        // CASE 1: PIN CLICKED
                                        if let Some(&sim_idx) = netlist.component_map.get(&comp.id) {
                                            painter.text(
                                                mouse_pos + Vec2::new(15.0, -15.0),
                                                egui::Align2::LEFT_BOTTOM,
                                                format!("Click to plot current at pin {} of {}", terminal_index, comp.name),
                                                egui::FontId::monospace(14.0),
                                                Color32::YELLOW,
                                            );

                                            if response.clicked_by(egui::PointerButton::Primary) {
                                                self.plotting_component_current = Some((sim_idx.clone(), terminal_index));
                                            }
                                        }
                                    } else if let Some(node_id) = self.wire_at_screen_pos(mouse_pos, 5.0) {
                                        // CASE 2: WIRE CLICKED (Only if no Pin)
                                        painter.text(
                                            mouse_pos + Vec2::new(15.0, -15.0),
                                            egui::Align2::LEFT_BOTTOM,
                                            format!("Click to plot voltage at node {}", node_id.0),
                                            egui::FontId::monospace(14.0),
                                            Color32::YELLOW,
                                        );

                                        if response.clicked_by(egui::PointerButton::Primary) {
                                            self.plotting_node_voltage = Some(node_id);
                                        }
                                    } else if let Some(comp) = hit_body {
                                        // CASE 3: BODY CLICKED (Only if no Pin AND no Wire)
                                        if let Some(&sim_idx) = netlist.component_map.get(&comp.id) {
                                            painter.text(
                                                mouse_pos + Vec2::new(15.0, -15.0),
                                                egui::Align2::LEFT_BOTTOM,
                                                format!("Click to plot an observable of {}", comp.name),
                                                egui::FontId::monospace(14.0),
                                                Color32::YELLOW,
                                            );

                                            if response.clicked_by(egui::PointerButton::Primary) {
                                                // TODO: Context menu logic
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        Tool::Erase => {
                            let erase_id = response.id.with("erase_tool_start");

                            ctx.set_cursor_icon(CursorIcon::Cell);

                            // Handle Cancellation (Escape or Right Click)
                            if ctx.input(|i| i.key_pressed(egui::Key::Escape) || i.pointer.secondary_clicked()) {
                                ctx.data_mut(|d| d.remove::<Pos2>(erase_id));
                                return;
                            }

                            // Handle Drag Start
                            if response.drag_started_by(egui::PointerButton::Primary) {
                                ctx.data_mut(|d| d.insert_temp(erase_id, mouse_pos));
                            }

                            // check if instead of drag, it was just a click to allow single-click erasing of components/wires
                            if response.clicked_by(egui::PointerButton::Primary) {
                                self.temp_state_snapshot = Some(self.state.clone());

                                let click_tolerance = 5.0;
                                let selection_rect = Rect::from_center_size(
                                    mouse_pos,
                                    Vec2::splat(click_tolerance)
                                );

                                let mut comps_to_remove = Vec::new();
                                let mut wires_to_remove = Vec::new();

                                // Check Components
                                for comp in &self.state.schematic.components {
                                    let comp_screen_pos = self.to_screen(comp.pos);
                                    let size = Vec2::new(2.0 * self.zoom, 1.0 * self.zoom);
                                    let comp_rect = Rect::from_center_size(comp_screen_pos, size);

                                    if selection_rect.intersects(comp_rect) {
                                        comps_to_remove.push(comp.id);
                                    }
                                }

                                // Check Wires
                                for (i, wire) in self.state.schematic.wires.iter().enumerate() {
                                    let p1 = self.to_screen(wire.start);
                                    let p2 = self.to_screen(wire.end);
                                    let wire_rect = Rect::from_two_pos(p1, p2).expand(2.0); // slight expansion

                                    if selection_rect.intersects(wire_rect) {
                                        wires_to_remove.push(i);
                                    }
                                }

                                // Apply removals (Components)
                                for id in comps_to_remove {
                                    self.state.schematic.remove_component(id);
                                }

                                // Apply removals (Wires)
                                wires_to_remove.sort_by(|a, b| b.cmp(a));
                                for idx in wires_to_remove {
                                    self.state.schematic.wires.remove(idx);
                                }

                                if let Some(prev) = self.temp_state_snapshot.take() {
                                    if prev.schematic != self.state.schematic {
                                        self.undo_stack.push(prev);
                                    }
                                }

                                self.selected_tool = Tool::Select;
                            }

                            // Handle Active Drag & Release
                            // We check if we have a start position stored
                            if let Some(start_pos) = ctx.data(|d| d.get_temp::<Pos2>(erase_id)) {
                                let selection_rect = Rect::from_two_pos(start_pos, mouse_pos);

                                // Draw the selection box
                                let painter = ctx.layer_painter(response.layer_id);
                                painter.rect(
                                    selection_rect,
                                    5.0,
                                    Color32::from_rgba_unmultiplied(135, 5, 1, 40),
                                    Stroke::new(1.0, Color32::WHITE),
                                    StrokeKind::Inside
                                );

                                // Handle Release
                                if response.drag_stopped_by(egui::PointerButton::Primary) {
                                    self.temp_state_snapshot = Some(self.state.clone());
                                    ctx.data_mut(|d| d.remove::<Pos2>(erase_id));

                                    // Collect IDs to remove
                                    let mut comps_to_remove = Vec::new();
                                    let mut wires_to_remove = Vec::new();

                                    // Check Components
                                    for comp in &self.state.schematic.components {
                                        let comp_screen_pos = self.to_screen(comp.pos);
                                        let size = Vec2::new(2.0 * self.zoom, 1.0 * self.zoom);
                                        let comp_rect = Rect::from_center_size(comp_screen_pos, size);

                                        if selection_rect.intersects(comp_rect) {
                                            comps_to_remove.push(comp.id);
                                        }
                                    }

                                    // Check Wires
                                    // We convert the wire endpoints to screen space to compare with selection_rect
                                    for (i, wire) in self.state.schematic.wires.iter().enumerate() {
                                        let p1 = self.to_screen(wire.start);
                                        let p2 = self.to_screen(wire.end);

                                        let wire_rect = Rect::from_two_pos(p1, p2);

                                        let tolerance = if selection_rect.width() < 5.0 { 5.0 } else { 0.0 };
                                        let hit_rect = wire_rect.expand(tolerance);

                                        if selection_rect.intersects(hit_rect) {
                                            if check_line_rect_intersection(p1, p2, selection_rect) {
                                                wires_to_remove.push(i);
                                            }
                                        }
                                    }

                                    // Apply removals (Components)
                                    for id in comps_to_remove {
                                        self.state.schematic.remove_component(id);
                                    }

                                    // Apply removals (Wires)
                                    wires_to_remove.sort_by(|a, b| b.cmp(a));
                                    for idx in wires_to_remove {
                                        self.state.schematic.wires.remove(idx);
                                    }

                                    if let Some(prev) = self.temp_state_snapshot.take() {
                                        if prev.schematic != self.state.schematic {
                                            self.undo_stack.push(prev);
                                        }
                                    }

                                    self.selected_tool = Tool::Select;
                                }
                            }
                        }
                        _ => {}
                    }
                }

                let show_voltage = self.plotting_node_voltage.is_some();
                let show_current = self.plotting_component_current.is_some();

                if show_voltage || show_current {
                    egui::TopBottomPanel::top("plot_panel")
                        .default_height(300.0)
                        .min_height(150.0)
                        .max_height(600.)
                        .resizable(true)
                        .show(ctx, |ui| {
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

                            ui.add_space(4.0);

                                if self.sim_state.history.is_empty() {
                                    ui.label("No data available");
                                    return;
                                }

                                draw_oscilloscope(ui, &mut self.scope_state, &self.sim_state, self.plotting_node_voltage.map(|id| id.0), self.plotting_component_current);

                                ctx.request_repaint();
                        });
                }

                if let Some(id) = self.editing_component_id {
                    let mut open = true;

                    let mut rename_request: Option<(usize, String)> = None;

                    if let Some(comp) = self.state.schematic.components.iter_mut().find(|c| c.id == id) {
                        // break if the component has no properties. For example ground
                        if comp.component != ComponentBuildData::Ground {
                            self.keybinds_locked = true;

                            egui::Modal::new(Id::new("component_edit_modal"))
                                .backdrop_color(self.theme.modal_backdrop)
                                .show(ctx, |ui| {
                                    self.keybinds_locked = true;
                                    ui.spacing_mut().item_spacing = Vec2::new(10.0, 10.0);

                                    let prefix = comp.component.prefix().to_string();

                                    ui.heading(format!("{} Properties", match comp.component {
                                        ComponentBuildData::Resistor { .. } => "Resistor",
                                        ComponentBuildData::Capacitor { .. } => "Capacitor",
                                        ComponentBuildData::DCSource { .. } => "DC Source",
                                        ComponentBuildData::ASource { .. } => "AC Source",
                                        ComponentBuildData::AudioSource { .. } => "Audio Source",
                                        ComponentBuildData::Inductor { .. } => "Inductor",
                                        ComponentBuildData::Diode { .. } => "Diode",
                                        ComponentBuildData::Bjt { .. } => "Bipolar Junction Transistor",
                                        ComponentBuildData::Label => "Label",
                                        _ => "Component",
                                    }));

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
                                                ui.add(egui::DragValue::new(resistance)
                                                    .speed(10.0)
                                                    .range(0.0..=f64::INFINITY)
                                                    .suffix("Ω")
                                                    .custom_formatter(|val, _range| {
                                                        format_si_single(val, 3)
                                                    })
                                                    .custom_parser(|text| {
                                                        parse_si(text)
                                                    }));
                                            });
                                        }
                                        ComponentBuildData::Capacitor { capacitance, esr } => {
                                            ui.horizontal(|ui| {
                                                ui.label("Capacitance:");
                                                // Use scientific notation for small values
                                                ui.add(egui::DragValue::new(capacitance).suffix("F")
                                                    .speed(1e-7)
                                                    .range(0.0..=f64::INFINITY)
                                                    .custom_formatter(|val, _range| {
                                                        format_si_single(val, 3)
                                                    })
                                                    .custom_parser(|text| {
                                                        parse_si(text)
                                                    }));
                                            });
                                            ui.horizontal(|ui| {
                                                ui.label("ESR:");
                                                ui.add(egui::DragValue::new(esr).suffix("Ω")
                                                    .speed(1e-4)
                                                    .range(0.0..=f64::INFINITY)
                                                    .custom_formatter(|val, _range| {
                                                        format_si_single(val, 3)
                                                    })
                                                    .custom_parser(|text| {
                                                        parse_si(text)
                                                    }));
                                            });
                                        }
                                        ComponentBuildData::DCSource { voltage } => {
                                            ui.horizontal(|ui| {
                                                ui.label("Voltage:");
                                                ui.add(egui::DragValue::new(voltage).speed(0.1).range(-f64::INFINITY..=f64::INFINITY).suffix("V")
                                                    .custom_formatter(|val, _range| {
                                                        format_si_single(val, 3)
                                                    })
                                                    .custom_parser(|text| {
                                                        parse_si(text)
                                                    }));
                                            });
                                        }
                                        ComponentBuildData::ASource { amplitude, frequency } => {
                                            ui.horizontal(|ui| {
                                                ui.label("Voltage:");
                                                ui.add(egui::DragValue::new(amplitude).speed(0.1).range(-f64::INFINITY..=f64::INFINITY).suffix("V")
                                                    .custom_formatter(|val, _range| {
                                                        format_si_single(val, 3)
                                                    })
                                                    .custom_parser(|text| {
                                                        parse_si(text)
                                                    }));
                                            });
                                            ui.horizontal(|ui| {
                                                ui.label("Frequency:");

                                            });
                                        }
                                        ComponentBuildData::AudioSource { path } => {
                                            if let Some(new_path) = self.temp_audio_path.take() {
                                                *path = new_path;
                                            }

                                            ui.label("Select input file:");

                                            let (rect, response) = ui.allocate_exact_size(
                                                Vec2::new(ui.available_width() / 2., 20.0f32),
                                                Sense::click()
                                            );
                                            let is_hovered = response.hovered();
                                            let has_dragged_files = !ui.ctx().input(|i| i.raw.hovered_files.clone()).is_empty();
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
                                                StrokeKind::Inside
                                            );
                                            let display_text = if !path.is_empty() {
                                                format!("🎵 {}", path.file_name().unwrap_or_default().to_string_lossy())
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

                                            if response.clicked() && self.file_dialog_state == FileDialogState::Closed {
                                                self.file_dialog_state = FileDialogState::LoadAudio;
                                                let tx = self.file_sender.clone();
                                                let ctx_clone = ui.ctx().clone();

                                                // Spawn RFD thread
                                                std::thread::spawn(move || {
                                                    let file = rfd::FileDialog::new().add_filter("Audio", &["wav", "mp3", "flac"]).pick_file();
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
                                            if let Some(new_path) = self.temp_audio_path.take() {
                                                *path = new_path;
                                            }

                                            ui.label("Select output path:");

                                            let (rect, response) = ui.allocate_exact_size(
                                                Vec2::new(ui.available_width() / 2., 20.0f32),
                                                Sense::click()
                                            );
                                            let is_hovered = response.hovered();
                                            let has_dragged_files = !ui.ctx().input(|i| i.raw.hovered_files.clone()).is_empty();
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
                                                StrokeKind::Inside
                                            );
                                            let display_text = if !path.is_empty() {
                                                format!("🎵 {}", path.file_name().unwrap_or_default().to_string_lossy())
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

                                            if response.clicked() && self.file_dialog_state == FileDialogState::Closed {
                                                self.file_dialog_state = FileDialogState::SaveAudio;
                                                let tx = self.file_sender.clone();
                                                let ctx_clone = ui.ctx().clone();

                                                // Spawn RFD thread
                                                std::thread::spawn(move || {
                                                    let file = rfd::FileDialog::new().add_filter("Audio", &["wav"]).save_file();
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
                                        ComponentBuildData::Inductor { inductance, series_resistance } => {
                                            ui.horizontal(|ui| {
                                                ui.label("Inductance:");
                                                ui.add(egui::DragValue::new(inductance).suffix("H")
                                                    .speed(1e-4)
                                                    .range(0.0..=f64::INFINITY)
                                                    .custom_formatter(|val, _range| {
                                                        format_si_single(val, 3)
                                                    })
                                                    .custom_parser(|text| {
                                                        parse_si(text)
                                                    }));
                                            });

                                            ui.horizontal(|ui| {
                                                ui.label("Series Resistance:");
                                                ui.add(egui::DragValue::new(series_resistance).suffix("Ω")
                                                    .speed(1e-4)
                                                    .range(0.0..=f64::INFINITY)
                                                    .custom_formatter(|val, _range| {
                                                        format_si_single(val, 3)
                                                    })
                                                    .custom_parser(|text| {
                                                        parse_si(text)
                                                    }));
                                            });
                                        }
                                        ComponentBuildData::Label => {
                                        }
                                        ComponentBuildData::Diode { model } => {
                                            ui.horizontal(|ui| {
                                                ui.label("Model:");
                                                ComboBox::from_id_salt("diode_combo").selected_text(model.format_name()).show_ui(ui, |ui| {
                                                    ui.selectable_value(&mut *model, DiodeModel::D1N4148, DiodeModel::D1N4148.format_name());
                                                });
                                            });
                                        }
                                        ComponentBuildData::Bjt { model } => {
                                            ui.horizontal(|ui| {
                                                ui.label("Model:");
                                                ComboBox::from_id_salt("bjt_combo").selected_text(model.format_name()).show_ui(ui, |ui| {
                                                    ui.selectable_value(&mut *model, BjtModel::GenericNPN, BjtModel::GenericNPN.format_name());
                                                    ui.selectable_value(&mut *model, BjtModel::GenericPNP, BjtModel::GenericPNP.format_name());
                                                });
                                            });
                                        }
                                        _ => {
                                            ui.label("No editable properties for this component.");
                                        }
                                    }

                                    ui.separator();

                                    if ui.button("Close").clicked() || ui.input(|i| i.key_pressed(egui::Key::Escape)) {
                                        open = false;
                                        self.keybinds_locked = false;
                                    }
                                });
                        } else {
                            // Component has no properties, close the modal
                            open = false;
                            self.keybinds_locked = false;
                        }
                    } else {
                        // ID not found (maybe component was deleted?), close the modal
                        open = false;
                    }

                    if let Some((target_id, prefix)) = rename_request {
                        let new_name = self.state.schematic.generate_next_name(&prefix);
                        if let Some(comp) = self.state.schematic.components.iter_mut().find(|c| c.id == target_id) {
                            comp.name = new_name;
                        }
                    }

                    if !open {
                        self.editing_component_id = None;
                    }
                }
            });
        });

        // check for repaint, full redraw next frame if sim is running and 30fps if not
        if running {
            ctx.request_repaint();
        } else {
            ctx.request_repaint_after(std::time::Duration::from_millis(33));
        }
    }
}
