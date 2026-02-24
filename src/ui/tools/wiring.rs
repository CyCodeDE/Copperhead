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
use crate::model::GridPos;
use crate::ui::app::{CircuitApp, Tool};
use crate::ui::VisualWire;
use egui::{Color32, Pos2, Stroke};

pub fn handle(
    app: &mut CircuitApp,
    ui: &egui::Ui,
    ctx: &egui::Context,
    response: &egui::Response,
    painter: &egui::Painter,
    start_node: &Option<GridPos>,
    grid_pos: GridPos,
    snap_pos: Pos2,
) {
    painter.circle_stroke(snap_pos, 4.0, Stroke::new(2.0, Color32::GREEN));

    if let Some(start) = start_node {
        // Start point is set, dragging to End point

        // Calculate L-Shape Routing (Manhattan)
        let knee = GridPos {
            x: grid_pos.x,
            y: start.y,
        };

        let mut ghost_segments = Vec::new();

        if start.x != knee.x {
            ghost_segments.push(VisualWire {
                start: *start,
                end: knee,
            });
        }

        if knee.y != grid_pos.y {
            ghost_segments.push(VisualWire {
                start: knee,
                end: grid_pos,
            });
        }

        // Draw Ghost Wires
        for wire in &ghost_segments {
            let w_start = app.to_screen(wire.start);
            let w_end = app.to_screen(wire.end);
            // Draw dashed or semi-transparent line
            painter.line_segment(
                [w_start, w_end],
                Stroke::new(2.0, Color32::from_rgba_unmultiplied(0, 255, 0, 128)),
            );
        }

        // Handle Second Click (Commit)
        if response.clicked_by(egui::PointerButton::Primary) {
            app.undo_stack.push(app.state.clone());
            // Add generated segments to the schematic
            app.state.schematic.wires.extend(ghost_segments);

            // Reset to wait for a new wire
            app.selected_tool = Tool::PlaceWire(None);
            // Alternative option: set new wire at end point of current wire
            // app.selected_tool = Tool::PlaceWire(Some(grid_pos));
        }
    } else {
        // Waiting for first click to set Start point
        if response.clicked_by(egui::PointerButton::Primary) {
            app.selected_tool = Tool::PlaceWire(Some(grid_pos));
        }
    }

    // Cancel on Right Click
    if response.clicked_by(egui::PointerButton::Secondary) {
        app.selected_tool = Tool::Select;
    }
}
