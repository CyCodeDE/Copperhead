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
use crate::ui::components::modals::handle_properties;
use crate::ui::drawing::{draw_component, draw_component_labels};
use crate::ui::lerp_color;
use egui::Context;
use egui::{Color32, CornerRadius, Frame, Margin, Sense, Stroke};

pub fn show(app: &mut CircuitApp, ctx: &Context) {
    let running = app.sim_state.running;

    // Central Canvas
    egui::CentralPanel::default()
        .frame(Frame::new().fill(app.theme.panel_color))
        .show(ctx, |ui| {
            let island_frame = Frame::default()
                .fill(app.theme.background)
                .corner_radius(CornerRadius {
                    nw: 20,
                    ne: 0,
                    sw: 0,
                    se: 0,
                })
                .stroke(Stroke::new(1.0, app.theme.panel_border)) // Subtle border
                .inner_margin(0.)
                .outer_margin(Margin {
                    top: 10,
                    left: 10,
                    right: 0,
                    bottom: 0,
                });

            island_frame.show(ui, |ui| {
                if !app.is_initialized {
                    let rect = ui.available_rect_before_wrap();
                    let center = rect.center();
                    app.pan = center.to_vec2();
                    app.is_initialized = true;
                }

                let (response, painter) =
                    ui.allocate_painter(ui.available_size_before_wrap(), Sense::click_and_drag());

                let rect = response.rect;

                // Pan & Zoom Logic
                if response.dragged_by(egui::PointerButton::Middle)
                    || response.dragged_by(egui::PointerButton::Primary)
                        && matches!(app.selected_tool, Tool::Select)
                {
                    app.pan += response.drag_delta();
                }

                let scroll = ui.input(|i| i.smooth_scroll_delta);
                if scroll.y != 0.0 {
                    let zoom_factor = if scroll.y > 0.0 { 1.05 } else { 0.95 };
                    let new_zoom = (app.zoom * zoom_factor).clamp(5.0, 200.0);
                    if let Some(mouse_pos) = response.hover_pos() {
                        let mouse_world = (mouse_pos - app.pan) / app.zoom;
                        app.zoom = new_zoom;
                        app.pan = mouse_pos - (mouse_world * app.zoom);
                    }
                }

                // Draw Grid
                crate::ui::drawing::draw_grid(
                    &painter,
                    rect,
                    app.zoom,
                    app.pan,
                    app.theme.dot_color,
                );

                // Draw Existing Components
                for comp in &app.state.schematic.components {
                    draw_component(
                        &painter,
                        comp,
                        |p| app.to_screen(p),
                        app.zoom,
                        app.theme
                            .component_body
                            .blend(app.theme.disabled_text_color),
                        app.theme.component_body,
                    );

                    draw_component_labels(&comp, &painter, |p| app.to_screen(p), app.zoom);
                }

                // Draw Wires (really naive rn)
                if running {
                    if let Some(netlist) = &app.active_netlist {
                        for (_coord, &node_id) in &netlist.node_map {
                            // calculate RMS
                            let (sum_sq, count) = app
                                .sim_state
                                .history
                                .iter()
                                .rev()
                                .take(8000)
                                .filter_map(|step| step.voltages.get(node_id.0))
                                .fold((0.0, 0), |(acc_sq, acc_cnt), &v| {
                                    (acc_sq + (v * v), acc_cnt + 1)
                                });

                            let color = if count > 0 {
                                let rms = (sum_sq / count as f64).sqrt();
                                let t = (rms / 5.0) as f32; // 5.0 is sensitivity
                                lerp_color(app.theme.wire_off, app.theme.wire_on, t)
                            } else {
                                app.theme.wire_off
                            };

                            app.wire_color_cache.insert(node_id, color);
                        }
                    }
                    // get latest sim state
                    for wire in &app.state.schematic.wires {
                        let start = app.to_screen(wire.start);
                        let end = app.to_screen(wire.end);

                        let mut color = app.theme.wire_off;
                        let mut stroke_width = 2.0;

                        if let Some(netlist) = &app.active_netlist {
                            if let Some(&node_id) = netlist.node_map.get(&wire.start) {
                                if let Some(cached_color) = app.wire_color_cache.get(&node_id) {
                                    color = *cached_color;

                                    if color != app.theme.wire_off {
                                        if color.r() > app.theme.wire_off.r() + 10 {
                                            stroke_width = 2.5;
                                        }
                                    }
                                }
                            }
                        }

                        painter.line_segment([start, end], Stroke::new(stroke_width, color));
                    }
                } else {
                    for wire in &app.state.schematic.wires {
                        let start = app.to_screen(wire.start);
                        let end = app.to_screen(wire.end);
                        painter.line_segment([start, end], Stroke::new(2.0, app.theme.wire_off));
                    }
                }

                // Tool Interaction & Ghost Drawing
                if let Some(mouse_pos) = response.hover_pos() {
                    let grid_pos = app.to_grid(mouse_pos);
                    let snap_pos = app.to_screen(grid_pos);

                    // Highlight grid point under mouse
                    painter.circle_filled(snap_pos, 3.0, Color32::from_white_alpha(50));

                    match &app.selected_tool.clone() {
                        Tool::PlaceComponent(comp_data) => {
                            crate::ui::tools::placement::handle(
                                app, ui, ctx, &response, &painter, comp_data, grid_pos, snap_pos,
                            );
                        }
                        Tool::PlaceWire(start_node) => {
                            crate::ui::tools::wiring::handle(
                                app, ui, ctx, &response, &painter, start_node, grid_pos, snap_pos,
                            );
                        }

                        Tool::Select => {
                            crate::ui::tools::selection::handle(
                                app, ui, ctx, &response, &painter, mouse_pos,
                            );
                        }

                        Tool::Erase => {
                            crate::ui::tools::erasing::handle(
                                app, ui, ctx, &response, &painter, mouse_pos, grid_pos, snap_pos,
                            );
                        }
                        _ => {}
                    }
                }

                handle_properties(app, ctx);
            });
        });
}
