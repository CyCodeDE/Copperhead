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
use crate::ui::drawing::check_line_rect_intersection;
use crate::ui::GridPos;
use egui::{Color32, CursorIcon, Pos2, Rect, Stroke, StrokeKind, Vec2};

pub fn handle(
    app: &mut CircuitApp,
    ui: &egui::Ui,
    ctx: &egui::Context,
    response: &egui::Response,
    painter: &egui::Painter,
    mouse_pos: Pos2,
    grid_pos: GridPos,
    snap_pos: Pos2,
) {
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
        app.temp_state_snapshot = Some(app.state.clone());

        let click_tolerance = 5.0;
        let selection_rect = Rect::from_center_size(mouse_pos, Vec2::splat(click_tolerance));

        let mut comps_to_remove = Vec::new();
        let mut wires_to_remove = Vec::new();

        // Check Components
        for comp in &app.state.schematic.components {
            let comp_screen_pos = app.to_screen(comp.pos);
            let size = Vec2::new(2.0 * app.zoom, 1.0 * app.zoom);
            let comp_rect = Rect::from_center_size(comp_screen_pos, size).translate(Vec2::new(
                comp.offset.0 * app.zoom,
                comp.offset.1 * app.zoom,
            ));

            if selection_rect.intersects(comp_rect) {
                comps_to_remove.push(comp.id);
            }
        }

        // Check Wires
        for (i, wire) in app.state.schematic.wires.iter().enumerate() {
            let p1 = app.to_screen(wire.start);
            let p2 = app.to_screen(wire.end);
            let wire_rect = Rect::from_two_pos(p1, p2).expand(2.0); // slight expansion

            if selection_rect.intersects(wire_rect) {
                wires_to_remove.push(i);
            }
        }

        // Apply removals (Components)
        for id in comps_to_remove {
            app.state.schematic.remove_component(id);
        }

        // Apply removals (Wires)
        wires_to_remove.sort_by(|a, b| b.cmp(a));
        for idx in wires_to_remove {
            app.state.schematic.wires.remove(idx);
        }

        if let Some(prev) = app.temp_state_snapshot.take() {
            if prev.schematic != app.state.schematic {
                app.undo_stack.push(prev);
            }
        }

        app.selected_tool = Tool::Select;
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
            StrokeKind::Inside,
        );

        // Handle Release
        if response.drag_stopped_by(egui::PointerButton::Primary) {
            app.temp_state_snapshot = Some(app.state.clone());
            ctx.data_mut(|d| d.remove::<Pos2>(erase_id));

            // Collect IDs to remove
            let mut comps_to_remove = Vec::new();
            let mut wires_to_remove = Vec::new();

            // Check Components
            for comp in &app.state.schematic.components {
                let comp_screen_pos = app.to_screen(comp.pos);
                let size = Vec2::new(2.0 * app.zoom, 1.0 * app.zoom);
                let comp_rect = Rect::from_center_size(comp_screen_pos, size);

                if selection_rect.intersects(comp_rect) {
                    comps_to_remove.push(comp.id);
                }
            }

            // Check Wires
            // We convert the wire endpoints to screen space to compare with selection_rect
            for (i, wire) in app.state.schematic.wires.iter().enumerate() {
                let p1 = app.to_screen(wire.start);
                let p2 = app.to_screen(wire.end);

                let wire_rect = Rect::from_two_pos(p1, p2);

                let tolerance = if selection_rect.width() < 5.0 {
                    5.0
                } else {
                    0.0
                };
                let hit_rect = wire_rect.expand(tolerance);

                if selection_rect.intersects(hit_rect) {
                    if check_line_rect_intersection(p1, p2, selection_rect) {
                        wires_to_remove.push(i);
                    }
                }
            }

            // Apply removals (Components)
            for id in comps_to_remove {
                app.state.schematic.remove_component(id);
            }

            // Apply removals (Wires)
            wires_to_remove.sort_by(|a, b| b.cmp(a));
            for idx in wires_to_remove {
                app.state.schematic.wires.remove(idx);
            }

            if let Some(prev) = app.temp_state_snapshot.take() {
                if prev.schematic != app.state.schematic {
                    app.undo_stack.push(prev);
                }
            }

            app.selected_tool = Tool::Select;
        }
    }
}
