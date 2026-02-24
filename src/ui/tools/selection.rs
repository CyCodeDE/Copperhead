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
use egui::{Color32, Pos2, Rect, Vec2};

pub fn handle(
    app: &mut CircuitApp,
    ui: &egui::Ui,
    ctx: &egui::Context,
    response: &egui::Response,
    painter: &egui::Painter,
    mouse_pos: Pos2,
) {
    if app.editing_component_id.is_none() && response.clicked_by(egui::PointerButton::Secondary) {
        let mut found_id = None;

        for comp in &app.state.schematic.components {
            let comp_screen_pos = app.to_screen(comp.pos);
            // add a small tolerance to the size for easier selection
            let size = (comp.size.to_vec2() + Vec2::splat(0.5)) * app.zoom;
            let rect = Rect::from_center_size(comp_screen_pos, size).translate(Vec2::new(
                comp.offset.0 * app.zoom,
                comp.offset.1 * app.zoom,
            ));

            if rect.contains(mouse_pos) {
                found_id = Some(comp.id);
                break;
            }
        }

        if let Some(id) = found_id {
            app.editing_component_id = Some(id);
        }
    }

    if let Some(mouse_pos) = response.hover_pos() {
        if let Some(netlist) = &app.active_netlist {
            let mut hit_pin = None;
            let mut hit_body = None;

            for comp in &app.state.schematic.components {
                let comp_screen_pos = app.to_screen(comp.pos);

                let pins = comp.get_pin_locations();
                for (terminal_index, pin_pos) in pins.iter().enumerate() {
                    let pin_screen_pos = app.to_screen(*pin_pos);
                    if pin_screen_pos.distance(mouse_pos) < 10.0 {
                        hit_pin = Some((comp, terminal_index));
                        break;
                    }
                }

                if hit_pin.is_some() {
                    break;
                }

                if hit_body.is_none() {
                    let size =
                        Vec2::new(comp.size.x as f32 * app.zoom, comp.size.y as f32 * app.zoom);
                    let rect = Rect::from_center_size(comp_screen_pos, size).translate(Vec2::new(
                        comp.offset.0 * app.zoom,
                        comp.offset.1 * app.zoom,
                    ));

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
                        format!(
                            "Click to plot current at pin {} of {}",
                            terminal_index, comp.name
                        ),
                        egui::FontId::monospace(14.0),
                        Color32::YELLOW,
                    );

                    if response.clicked_by(egui::PointerButton::Primary) {
                        app.plotting_component_current = Some((sim_idx.clone(), terminal_index));
                    }
                }
            } else if let Some(node_id) = app.wire_at_screen_pos(mouse_pos, 5.0) {
                // CASE 2: WIRE CLICKED (Only if no Pin)
                painter.text(
                    mouse_pos + Vec2::new(15.0, -15.0),
                    egui::Align2::LEFT_BOTTOM,
                    format!("Click to plot voltage at node {}", node_id.0),
                    egui::FontId::monospace(14.0),
                    Color32::YELLOW,
                );

                if response.clicked_by(egui::PointerButton::Primary) {
                    app.plotting_node_voltage = Some(node_id);
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
