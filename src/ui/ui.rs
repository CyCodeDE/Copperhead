use crate::model::GridPos;
use crate::ui::app::{CircuitApp, Tool};
use crate::ui::components::oscilloscope::draw_oscilloscope;
use crate::ui::drawing::{
    check_line_rect_intersection, draw_component, draw_component_labels, draw_grid,
};
use crate::ui::{ComponentBuildData, SimCommand, SimStepData, VisualComponent, VisualWire};
use egui::text::LayoutJob;
use egui::{
    Align, Align2, CentralPanel, Checkbox, Color32, CursorIcon, FontSelection, Id, Key, Painter,
    Pos2, Rect, RichText, Sense, Shape, Stroke, StrokeKind, Style, TextFormat, TopBottomPanel,
    Vec2, Vec2b, ViewportCommand, WidgetText,
};
use egui_plot::{Line, Plot, PlotPoints};
use faer::prelude::default;
use log::{debug, info};
use std::time::Duration;
use crate::components::diode::DiodeModel;

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
        // Handle Escape to cancel tool
        if ctx.input(|i| i.key_pressed(Key::Escape)) {
            self.selected_tool = Tool::Select;
        }

        // Handle Rotate (CTRL + R)
        if ctx.input(|i| i.modifiers.command && i.key_pressed(Key::R)) {
            self.current_rotation = (self.current_rotation + 1) % 4;
        }

        // menu bar
        egui::TopBottomPanel::top("menu_bar").show(ctx, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("New").clicked() {
                        self.schematic = Default::default();
                        self.active_netlist = None;
                        ui.close_menu();
                    }

                    if ui.button("Open").clicked() {}

                    if ui.button("Save").clicked() {}

                    if ui.button("Export Netlist").clicked() {
                        let netlist = self.compile_netlist();
                        info!("Exported Netlist: {:?}", netlist);
                        ui.close_menu();
                    }
                    if ui.button("Quit").clicked() {
                        ui.ctx().send_viewport_cmd(ViewportCommand::Close);
                    }
                });

                ui.menu_button("Edit", |ui| {
                    if ui.button("Undo").clicked() {}
                    if ui.button("Redo").clicked() {}
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
                });

                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    ui.label(format!("Copperhead v{}", env!("CARGO_PKG_VERSION")));
                });
            });
        });

        egui::SidePanel::left("tools").show(ctx, |ui| {
            ui.heading("Copperhead");
            if ui.button("Start").clicked() {
                let netlist = self.compile_netlist();
                self.active_netlist = Some(netlist.clone());
                debug!("Compiled Netlist: {:?}", netlist);

                self.tx_command
                    .send(SimCommand::LoadCircuit(netlist))
                    .unwrap();
                self.tx_command
                    .send(SimCommand::SetRunTime(self.simulation_time))
                    .unwrap();
                self.tx_command.send(SimCommand::Resume).unwrap();
            }
            if ui.button("Stop").clicked() {
                self.tx_command.send(SimCommand::Pause).unwrap();
            }

            let enabled = if let Some(state) = self.shared_state.try_read() {
                !state.running
            } else {
                false
            };

            if ui
                .add_enabled(
                    enabled,
                    Checkbox::new(&mut self.realtime_mode, "Real-Time Simulation"),
                )
                .clicked()
            {
                if let Some(state) = self.shared_state.try_read() {
                    // TODO: handle this better
                    // Command should be queued and processed by the sim thread when it is safe to do so instead of completely dropping it
                    self.tx_command
                        .send(SimCommand::SetRealtime(self.realtime_mode))
                        .unwrap();
                }
            };
            ui.horizontal(|ui| {
                ui.label("Sim Time (s):");
                ui.add_enabled(
                    !self.realtime_mode,
                    egui::DragValue::new(&mut self.simulation_time)
                        .speed(1)
                        .clamp_range(0.001..=1000.0)
                        .max_decimals(3),
                );
            });

            ui.separator();

            ui.heading("Tools");
            ui.label("Controls: MB to Pan, Scroll to Zoom, 'R' to Rotate, 'Esc' to Cancel");
            ui.separator();

            if ui.button("Select (Esc)").clicked() {
                self.selected_tool = Tool::Select;
            }
            if ui.button("Erase (E/DEL)").clicked()
                || ui.input(|i| i.key_pressed(Key::Delete) || i.key_pressed(Key::E))
            {
                self.selected_tool = Tool::Erase;
            }
            ui.separator();

            // TODO: make a grid with icons for each component type instead of label buttons

            // Component Buttons
            if ui.button("Draw Wire (W)").clicked()
                || (ui.input(|i| i.key_pressed(Key::W)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool = Tool::PlaceWire(None);
            }
            if ui.button("Resistor (R)").clicked()
                || (ui.input(|i| i.key_pressed(Key::R)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::Resistor { resistance: 1000.0 }); // 1k Ohm
            }
            if ui.button("Capacitor (C)").clicked()
                || (ui.input(|i| i.key_pressed(Key::C)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::Capacitor { capacitance: 1e-6 }); // 1 uF
            }
            if ui.button("Inductor (L)").clicked()
                || (ui.input(|i| i.key_pressed(Key::L)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::Inductor { inductance: 1e-3 }); // 1 mH
            }
            if ui.button("Diode (D)").clicked()
                || (ui.input(|i| i.key_pressed(Key::D)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool = Tool::PlaceComponent(ComponentBuildData::Diode { model: DiodeModel::D1N4148 });
            }
            if ui.button("DC Source (Y)").clicked()
                || (ui.input(|i| i.key_pressed(Key::Y)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool =
                    Tool::PlaceComponent(ComponentBuildData::DCSource { voltage: 5.0 }); // 5V default
            }
            if ui.button("AC Source (A)").clicked()
                || (ui.input(|i| i.key_pressed(Key::A)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool = Tool::PlaceComponent(ComponentBuildData::ASource {
                    amplitude: 5.0,
                    frequency: 60.0,
                }); // 5V, 60Hz default
            }
            if ui.button("Ground (G)").clicked()
                || (ui.input(|i| i.key_pressed(Key::G)) && ui.input(|i| i.modifiers.is_none()))
            {
                self.selected_tool = Tool::PlaceComponent(ComponentBuildData::Ground);
            }

            ui.separator();
            ui.label(format!("State: {:?}", self.selected_tool.get_name()));
        });

        // Central Canvas
        egui::CentralPanel::default().show(ctx, |ui| {
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
            draw_grid(&painter, rect, self.zoom, self.pan);

            // Draw Existing Components
            for comp in &self.schematic.components {
                draw_component(
                    &painter,
                    comp,
                    |p| self.to_screen(p),
                    self.zoom,
                    Color32::from_gray(50),
                    Color32::WHITE
                );

                draw_component_labels(
                    &comp,
                    &painter,
                    |p| self.to_screen(p),
                    self.zoom,
                );
            }

            // Draw Wires (really naive rn)
            for wire in &self.schematic.wires {
                let start = self.to_screen(wire.start);
                let end = self.to_screen(wire.end);
                painter.line_segment([start, end], Stroke::new(2.0, Color32::GREEN));
            }

            // Tool Interaction & Ghost Drawing
            if let Some(mouse_pos) = response.hover_pos() {
                let grid_pos = self.to_grid(mouse_pos);
                let snap_pos = self.to_screen(grid_pos);

                // Highlight grid point under mouse
                painter.circle_filled(snap_pos, 3.0, Color32::from_white_alpha(50));

                match &self.selected_tool {
                    Tool::PlaceComponent(comp_data) => {
                        let ghost_comp = VisualComponent {
                            name: "".to_string(),
                            id: 0,
                            component: comp_data.clone(),
                            pos: grid_pos,
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
                            self.schematic.add_component(
                                comp_data.clone(),
                                grid_pos,
                                self.current_rotation
                            );

                            self.current_rotation = 0;
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
                                // Add generated segments to the schematic
                                self.schematic.wires.extend(ghost_segments);

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

                            for comp in &self.schematic.components {
                                let comp_screen_pos = self.to_screen(comp.pos);
                                let size = Vec2::new(2.0 * self.zoom, 1.0 * self.zoom);
                                let rect = Rect::from_center_size(comp_screen_pos, size);

                                if rect.contains(mouse_pos) {
                                    println!("Selected component ID: {}", comp.id);
                                    found_id = Some(comp.id);
                                    break;
                                }
                            }

                            if let Some(id) = found_id {
                                self.editing_component_id = Some(id);
                            }
                        }

                        if let Some(mouse_pos) = response.hover_pos() {
                            let grid_pos = self.to_grid(mouse_pos);

                            if let Some(netlist) = &self.active_netlist {
                                if let Some(&node_id) = netlist.node_map.get(&grid_pos) {

                                    if response.clicked_by(egui::PointerButton::Primary) {
                                        self.plotting_node_voltage = Some(node_id);
                                    }

                                    // get latest data
                                    if let Some(state) = self.shared_state.try_read() {
                                        if state.running {
                                            if let Some(volts) = state.get_latest_voltage(node_id) {
                                                painter.text(
                                                    mouse_pos + Vec2::new(15.0, -15.0),
                                                    egui::Align2::LEFT_BOTTOM,
                                                    format!("Click to plot voltage at node {}", node_id.0),
                                                    egui::FontId::monospace(14.0),
                                                    Color32::YELLOW,
                                                );
                                            }
                                        }
                                    }
                                }

                                // Check for Component Current (Hovering body)
                                for comp in &self.schematic.components {
                                    let comp_screen_pos = self.to_screen(comp.pos);
                                    let size = Vec2::new(2.0 * self.zoom, 1.0 * self.zoom);
                                    let rect = Rect::from_center_size(comp_screen_pos, size);

                                    if rect.contains(mouse_pos) {
                                        if let Some(&sim_idx) = netlist.component_map.get(&comp.id) {
                                            if response.clicked_by(egui::PointerButton::Primary) {
                                                self.plotting_component_current = Some(sim_idx);
                                            }

                                            if let Some(state) = self.shared_state.try_read() {
                                                // it is not safe to get a component or node id if the sim is not running, as the data may be stale
                                                // TODO: improve this
                                                if state.running {
                                                    if state.get_latest_current(sim_idx).is_some() {
                                                        painter.text(
                                                            mouse_pos + Vec2::new(15.0, 0.0),
                                                            egui::Align2::LEFT_TOP,
                                                            format!("Click To Plot Current at {}", comp.name),
                                                            egui::FontId::monospace(14.0),
                                                            Color32::LIGHT_BLUE,
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                        break;
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
                            let click_tolerance = 5.0;
                            let selection_rect = Rect::from_center_size(
                                mouse_pos,
                                Vec2::splat(click_tolerance)
                            );

                            let mut comps_to_remove = Vec::new();
                            let mut wires_to_remove = Vec::new();

                            // Check Components
                            for comp in &self.schematic.components {
                                let comp_screen_pos = self.to_screen(comp.pos);
                                let size = Vec2::new(2.0 * self.zoom, 1.0 * self.zoom);
                                let comp_rect = Rect::from_center_size(comp_screen_pos, size);

                                if selection_rect.intersects(comp_rect) {
                                    comps_to_remove.push(comp.id);
                                }
                            }

                            // Check Wires
                            for (i, wire) in self.schematic.wires.iter().enumerate() {
                                let p1 = self.to_screen(wire.start);
                                let p2 = self.to_screen(wire.end);
                                let wire_rect = Rect::from_two_pos(p1, p2).expand(2.0); // slight expansion

                                if selection_rect.intersects(wire_rect) {
                                    wires_to_remove.push(i);
                                }
                            }

                            // Apply removals (Components)
                            for id in comps_to_remove {
                                self.schematic.remove_component(id);
                            }

                            // Apply removals (Wires)
                            wires_to_remove.sort_by(|a, b| b.cmp(a));
                            for idx in wires_to_remove {
                                self.schematic.wires.remove(idx);
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
                                Color32::from_rgba_unmultiplied(155, 20, 20, 5),
                                Stroke::new(1.0, Color32::WHITE),
                                StrokeKind::Inside
                            );

                            // Handle Release
                            if response.drag_stopped_by(egui::PointerButton::Primary) {
                                ctx.data_mut(|d| d.remove::<Pos2>(erase_id));

                                // Collect IDs to remove
                                let mut comps_to_remove = Vec::new();
                                let mut wires_to_remove = Vec::new();

                                // Check Components
                                for comp in &self.schematic.components {
                                    let comp_screen_pos = self.to_screen(comp.pos);
                                    let size = Vec2::new(2.0 * self.zoom, 1.0 * self.zoom);
                                    let comp_rect = Rect::from_center_size(comp_screen_pos, size);

                                    if selection_rect.intersects(comp_rect) {
                                        comps_to_remove.push(comp.id);
                                    }
                                }

                                // Check Wires
                                // We convert the wire endpoints to screen space to compare with selection_rect
                                for (i, wire) in self.schematic.wires.iter().enumerate() {
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
                                    self.schematic.remove_component(id);
                                }

                                // Apply removals (Wires)
                                wires_to_remove.sort_by(|a, b| b.cmp(a));
                                for idx in wires_to_remove {
                                    self.schematic.wires.remove(idx);
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

                        if let Some(state) = self.shared_state.try_read() {
                            let history = &state.history;
                            if history.is_empty() {
                                ui.label("No data available");
                                return;
                            }

                            draw_oscilloscope(ui, &mut self.scope_state, history, self.plotting_node_voltage.map(|id| id.0), self.plotting_component_current);

                            ctx.request_repaint();
                        } else {
                            ui.centered_and_justified(|ui| ui.spinner());
                        }
                    });
            }

            if let Some(id) = self.editing_component_id {
                let mut open_via_x = true;
                let mut close_via_button = false;

                if let Some(comp) = self.schematic.components.iter_mut().find(|c| c.id == id) {
                    egui::Window::new("Edit Component")
                        .id(Id::new("component_edit_window"))
                        .open(&mut open_via_x)
                        .collapsible(false)
                        .resizable(false)
                        .anchor(egui::Align2::CENTER_CENTER, Vec2::ZERO)
                        .show(ctx, |ui| {
                            ui.spacing_mut().item_spacing = Vec2::new(10.0, 10.0);

                            // Reusable Match for different components
                            match &mut comp.component {
                                ComponentBuildData::Resistor { resistance } => {
                                    ui.heading("Resistor Properties");
                                    ui.horizontal(|ui| {
                                        ui.label("Resistance (Ω):");
                                        ui.add(egui::DragValue::new(resistance)
                                            .speed(10.0)
                                            .range(0.0..=f64::INFINITY));
                                    });
                                }
                                ComponentBuildData::Capacitor { capacitance } => {
                                    ui.heading("Capacitor Properties");
                                    ui.horizontal(|ui| {
                                        ui.label("Capacitance (F):");
                                        // Use scientific notation for small values
                                        ui.add(egui::DragValue::new(capacitance)
                                            .speed(1e-7)
                                            .range(0.0..=f64::INFINITY));
                                    });
                                }
                                ComponentBuildData::DCSource { voltage } => {
                                    ui.heading("DC Source Properties");
                                    ui.horizontal(|ui| {
                                        ui.label("Voltage (V):");
                                        ui.add(egui::DragValue::new(voltage).speed(0.1));
                                    });
                                }
                                ComponentBuildData::ASource { amplitude, frequency } => {
                                    ui.heading("DC Source Properties");
                                    ui.horizontal(|ui| {
                                        ui.label("Voltage (V):");
                                        ui.add(egui::DragValue::new(amplitude).speed(0.1));
                                    });
                                    ui.horizontal(|ui| {
                                        ui.label("Frequency (Hz):");
                                        ui.add(egui::DragValue::new(frequency).speed(1.0));
                                    });
                                }
                                ComponentBuildData::Ground => {
                                    ui.label("Ground component has no properties.");
                                }
                                ComponentBuildData::Inductor { inductance } => {
                                    ui.heading("Inductor Properties");
                                    ui.horizontal(|ui| {
                                        ui.label("Inductance (H):");
                                        // Use scientific notation for small values
                                        ui.add(egui::DragValue::new(inductance)
                                            .speed(1e-4)
                                            .range(0.0..=f64::INFINITY));
                                    });
                                }
                                _ => {
                                    ui.label("No editable properties for this component.");
                                }
                            }

                            ui.separator();

                            if ui.button("Close").clicked() {
                                close_via_button = true;
                            }
                        });
                } else {
                    // ID not found (maybe component was deleted?), close the modal
                    close_via_button = false;
                }

                if !open_via_x || close_via_button {
                    self.editing_component_id = None;
                }
            }
        });

        // check for repaint, full redraw next frame if sim is running and 30fps if not
        if let Some(state) = self.shared_state.try_read() {
            if state.running {
                ctx.request_repaint();
            } else {
                ctx.request_repaint_after(std::time::Duration::from_millis(33));
            }
        }
    }
}