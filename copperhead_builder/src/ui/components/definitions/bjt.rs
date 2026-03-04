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
use crate::ui::components::definitions::ComponentUIExt;
use crate::ui::drawing::{Anchor, LabelEngine, rotate_vec};
use copperhead_core::components::transistor::bjt::{BjtDef, BjtModel};
use egui::{Color32, ComboBox, Painter, Pos2, Shape, Stroke, Ui, Vec2};

impl ComponentUIExt for BjtDef {
    fn prefix(&self) -> &'static str {
        "Q"
    }

    fn ui_name(&self) -> &'static str {
        "Bipolar Junction Transistor"
    }

    fn size(&self) -> (isize, isize) {
        (2, 2)
    }

    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        match self.model.polarity() {
            true => vec![(1, -1), (-1, 0), (1, 1)], // 3 Pins (Collector, Base, Emitter) -> C, B, E   | NPN
            false => vec![(1, 1), (-1, 0), (1, -1)], // 3 Pins (Collector, Base, Emitter) -> C, B, E  | PNP
        }
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        ui.horizontal(|ui| {
            ui.label("Model:");
            ComboBox::from_id_salt("bjt_combo")
                .selected_text(self.model.format_name())
                .show_ui(ui, |ui| {
                    ui.selectable_value(
                        &mut self.model,
                        BjtModel::GenericNPN,
                        BjtModel::GenericNPN.format_name(),
                    );
                    ui.selectable_value(
                        &mut self.model,
                        BjtModel::GenericPNP,
                        BjtModel::GenericPNP.format_name(),
                    );
                });
        });

        false
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        let label_anchor = match rotation % 4 {
            0 => Anchor::Right,
            1 => Anchor::Bottom,
            2 => Anchor::Left,
            3 => Anchor::Top,
            _ => Anchor::Right,
        };

        engine.draw_stacked_labels(name, &self.model.format_name(), label_anchor);

        let is_npn = self.model.polarity();
        let (top_char, bot_char) = if is_npn { ("C", "E") } else { ("E", "C") };

        engine.draw_pin_marker(Vec2::new(0.7, -1.0), top_char); // Top right pin
        engine.draw_pin_marker(Vec2::new(-1.0, -0.3), "B"); // Left pin
        engine.draw_pin_marker(Vec2::new(0.7, 1.0), bot_char); // Bottom right pin
    }

    fn draw_icon(
        &self,
        painter: &Painter,
        center: Pos2,
        rotation: u8,
        zoom: f32,
        fill_color: Color32,
        stroke_color: Color32,
    ) {
        match self.model.polarity() {
            true => draw_bjt_npn(painter, center, rotation, zoom, fill_color, stroke_color),
            false => draw_bjt_pnp(painter, center, rotation, zoom, fill_color, stroke_color),
        }
    }
}

fn draw_bjt_npn(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let stroke = Stroke::new(2.0, stroke_color);

    // 1. Draw the Circle Body
    // Center offset to shift the circle slightly right
    let circle_center_offset = Vec2::new(0.3, 0.0);
    let radius = 0.7;

    painter.circle(
        center + rotate_vec(circle_center_offset * zoom, rotation),
        radius * zoom,
        fill_color, // Use the fill color for the circle body
        stroke,
    );

    // 2. Draw Base (Left Side)
    // Base Pin: (-1.0, 0.0) -> Connects to Base Bar at (-0.2, 0.0)
    let base_pin = Vec2::new(-1.0, 0.0);
    let base_bar_x = -0.2;
    let base_connect = Vec2::new(base_bar_x, 0.0);

    // Wire from Pin to Bar
    painter.line_segment(
        [
            center + rotate_vec(base_pin * zoom, rotation),
            center + rotate_vec(base_connect * zoom, rotation),
        ],
        stroke,
    );

    // Base Bar (Vertical Line)
    let bar_height = 0.5;
    let bar_top = Vec2::new(base_bar_x, -bar_height);
    let bar_bot = Vec2::new(base_bar_x, bar_height);

    painter.line_segment(
        [
            center + rotate_vec(bar_top * zoom, rotation),
            center + rotate_vec(bar_bot * zoom, rotation),
        ],
        Stroke::new(2.5, stroke_color), // Make the base bar slightly thicker
    );

    // 3. Draw Collector (Top Right)
    // Pin: (1.0, -1.0)
    let col_pin = Vec2::new(1.0, -1.0);
    let col_corner = Vec2::new(1.0, -0.5); // The "Knee"
    let col_base_contact = Vec2::new(base_bar_x, -0.25); // Where it touches the base

    // Wire: Pin -> Corner
    painter.line_segment(
        [
            center + rotate_vec(col_pin * zoom, rotation),
            center + rotate_vec(col_corner * zoom, rotation),
        ],
        stroke,
    );
    // Wire: Corner -> Base
    painter.line_segment(
        [
            center + rotate_vec(col_corner * zoom, rotation),
            center + rotate_vec(col_base_contact * zoom, rotation),
        ],
        stroke,
    );

    // 4. Draw Emitter (Bottom Right)
    // Pin: (1.0, 1.0)
    let emit_pin = Vec2::new(1.0, 1.0);
    let emit_corner = Vec2::new(1.0, 0.5);
    let emit_base_contact = Vec2::new(base_bar_x, 0.25);

    // Wire: Pin -> Corner
    painter.line_segment(
        [
            center + rotate_vec(emit_pin * zoom, rotation),
            center + rotate_vec(emit_corner * zoom, rotation),
        ],
        stroke,
    );
    // Wire: Corner -> Base
    painter.line_segment(
        [
            center + rotate_vec(emit_corner * zoom, rotation),
            center + rotate_vec(emit_base_contact * zoom, rotation),
        ],
        stroke,
    );

    // 5. Draw Emitter Arrow (NPN Style - Pointing Out)
    // We calculate a position along the vector (Base -> Corner)
    let arrow_pos_t = 0.5; // Position 0.0 to 1.0 along the segment

    // Manual Lerp: A + (B - A) * t
    let arrow_center_local = emit_base_contact + (emit_corner - emit_base_contact) * arrow_pos_t;

    // Direction vector of the emitter leg (normalized)
    let dir = (emit_corner - emit_base_contact).normalized();
    // Perpendicular vector for arrow width
    let perp = Vec2::new(-dir.y, dir.x);

    let arrow_size = 0.15;
    let arrow_width = 0.1;

    // Triangle points relative to local center
    let tip = arrow_center_local + dir * arrow_size;
    let base_l = arrow_center_local - dir * arrow_size + perp * arrow_width;
    let base_r = arrow_center_local - dir * arrow_size - perp * arrow_width;

    let arrow_points_local = [tip, base_l, base_r];

    let rotated_arrow: Vec<Pos2> = arrow_points_local
        .iter()
        .map(|&p| center + rotate_vec(p * zoom, rotation))
        .collect();

    // Use fill_color or stroke_color based on preference (filled arrows are common)
    painter.add(Shape::convex_polygon(
        rotated_arrow,
        stroke_color,
        Stroke::NONE,
    ));
}

fn draw_bjt_pnp(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let stroke = Stroke::new(2.0, stroke_color);

    let circle_center_offset = Vec2::new(0.3, 0.0);
    let radius = 0.7;

    painter.circle(
        center + rotate_vec(circle_center_offset * zoom, rotation),
        radius * zoom,
        fill_color,
        stroke,
    );

    let base_pin = Vec2::new(-1.0, 0.0);
    let base_bar_x = -0.2;
    let base_connect = Vec2::new(base_bar_x, 0.0);

    // Wire from Pin to Bar
    painter.line_segment(
        [
            center + rotate_vec(base_pin * zoom, rotation),
            center + rotate_vec(base_connect * zoom, rotation),
        ],
        stroke,
    );

    // Base Bar (Vertical Line)
    let bar_height = 0.5;
    let bar_top = Vec2::new(base_bar_x, -bar_height);
    let bar_bot = Vec2::new(base_bar_x, bar_height);

    painter.line_segment(
        [
            center + rotate_vec(bar_top * zoom, rotation),
            center + rotate_vec(bar_bot * zoom, rotation),
        ],
        Stroke::new(2.5, stroke_color),
    );

    let emit_pin = Vec2::new(1.0, -1.0);
    let emit_corner = Vec2::new(1.0, -0.5);
    let emit_base_contact = Vec2::new(base_bar_x, -0.25);

    painter.line_segment(
        [
            center + rotate_vec(emit_pin * zoom, rotation),
            center + rotate_vec(emit_corner * zoom, rotation),
        ],
        stroke,
    );

    painter.line_segment(
        [
            center + rotate_vec(emit_corner * zoom, rotation),
            center + rotate_vec(emit_base_contact * zoom, rotation),
        ],
        stroke,
    );

    let col_pin = Vec2::new(1.0, 1.0);
    let col_corner = Vec2::new(1.0, 0.5);
    let col_base_contact = Vec2::new(base_bar_x, 0.25);

    painter.line_segment(
        [
            center + rotate_vec(col_pin * zoom, rotation),
            center + rotate_vec(col_corner * zoom, rotation),
        ],
        stroke,
    );
    painter.line_segment(
        [
            center + rotate_vec(col_corner * zoom, rotation),
            center + rotate_vec(col_base_contact * zoom, rotation),
        ],
        stroke,
    );

    let arrow_pos_t = 0.5;

    let arrow_center_local = emit_base_contact + (emit_corner - emit_base_contact) * arrow_pos_t;

    let dir = (emit_base_contact - emit_corner).normalized();

    let perp = Vec2::new(-dir.y, dir.x);

    let arrow_size = 0.15;
    let arrow_width = 0.1;

    let tip = arrow_center_local + dir * arrow_size;
    let base_l = arrow_center_local - dir * arrow_size + perp * arrow_width;
    let base_r = arrow_center_local - dir * arrow_size - perp * arrow_width;

    let arrow_points_local = [tip, base_l, base_r];

    let rotated_arrow: Vec<Pos2> = arrow_points_local
        .iter()
        .map(|&p| center + rotate_vec(p * zoom, rotation))
        .collect();

    painter.add(Shape::convex_polygon(
        rotated_arrow,
        stroke_color,
        Stroke::NONE,
    ));
}
