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
use crate::ui::components::definitions::ComponentUIExt;
use crate::ui::components::modals::handle_properties;
use crate::ui::drawing::{
    draw_component, draw_hop_arc, draw_vertical_wire_with_hops, find_wire_crossings,
    find_wire_junctions,
};
use crate::ui::lerp_color;
use egui::Context;
use egui::{Color32, CornerRadius, Frame, Margin, Sense, Stroke};
use std::collections::HashMap;

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
                .stroke(Stroke::new(1.0, app.theme.panel_border))
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

                if running {
                    if let Some(netlist) = &app.active_netlist {
                        for (_, &node_id) in &netlist.node_map {
                            let (sum_sq, count) = app
                                .sim_state
                                .history
                                .iter()
                                .rev()
                                .take(8000)
                                .filter_map(|step| step.voltages.get(node_id.0))
                                .fold((0.0f64, 0usize), |(sq, cnt), &v| (sq + v * v, cnt + 1));

                            let color = if count > 0 {
                                let rms = (sum_sq / count as f64).sqrt();
                                lerp_color(app.theme.wire_off, app.theme.wire_on, (rms / 5.0) as f32)
                            } else {
                                app.theme.wire_off
                            };

                            app.wire_color_cache.insert(node_id, color);
                        }
                    }
                }

                let wire_count = app.state.schematic.wires.len();
                let mut wire_colors = vec![(app.theme.wire_off, 2.0f32); wire_count];

                if running {
                    for (i, wire) in app.state.schematic.wires.iter().enumerate() {
                        if let Some(netlist) = &app.active_netlist {
                            if let Some(&node_id) = netlist.node_map.get(&wire.start) {
                                if let Some(&cached) = app.wire_color_cache.get(&node_id) {
                                    let sw = if cached.r() > app.theme.wire_off.r() + 10 {
                                        2.5
                                    } else {
                                        2.0
                                    };
                                    wire_colors[i] = (cached, sw);
                                }
                            }
                        }
                    }
                }

                let crossings = find_wire_crossings(&app.state.schematic.wires);
                let junctions = find_wire_junctions(&app.state.schematic.wires);

                // Build a map: vertical-wire index → crossing points on that wire
                let mut hop_map: HashMap<usize, Vec<_>> = HashMap::new();
                for c in &crossings {
                    hop_map
                        .entry(c.vertical_wire_idx)
                        .or_default()
                        .push(c.crossing);
                }

                for (i, wire) in app.state.schematic.wires.iter().enumerate() {
                    let (color, sw) = wire_colors[i];
                    if let Some(hops) = hop_map.get(&i) {
                        draw_vertical_wire_with_hops(
                            &painter,
                            wire,
                            hops,
                            color,
                            sw,
                            app.zoom,
                            |p| app.to_screen(p),
                        );
                    } else {
                        painter.line_segment(
                            [app.to_screen(wire.start), app.to_screen(wire.end)],
                            Stroke::new(sw, color),
                        );
                    }
                }

                for c in &crossings {
                    let (color, sw) = wire_colors[c.vertical_wire_idx];
                    draw_hop_arc(&painter, app.to_screen(c.crossing), app.zoom, color, sw);
                }

                // Phase 6: overlay junction dots
                let junction_radius = app.zoom * 0.15;
                for &junction in &junctions {
                    let color = if running {
                        app.active_netlist
                            .as_ref()
                            .and_then(|nl| nl.node_map.get(&junction))
                            .and_then(|&nid| app.wire_color_cache.get(&nid))
                            .copied()
                            .unwrap_or(app.theme.wire_off)
                    } else {
                        app.theme.wire_off
                    };
                    painter.circle_filled(app.to_screen(junction), junction_radius, color);
                }

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

                    let center = app.to_screen(comp.pos);

                    comp.element.draw_labels(
                        &painter,
                        center,
                        comp.rotation % 4,
                        app.zoom,
                        &comp.name,
                    );
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

                        Tool::Move => crate::ui::tools::movement::handle(
                            app, ui, ctx, &response, &painter, mouse_pos,
                        ),
                        _ => {}
                    }
                }

                handle_properties(app, ctx);
            });
        });
}
