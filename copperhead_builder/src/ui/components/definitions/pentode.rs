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
use copperhead_core::components::pentode::{PentodeDef, PentodeType};
use eframe::emath::Pos2;
use eframe::epaint::Color32;
use egui::{ComboBox, Painter, Stroke, Ui, Vec2};
use strum::IntoEnumIterator;

impl ComponentUIExt for PentodeDef {
    fn prefix(&self) -> &'static str {
        "P"
    }

    fn size(&self) -> (isize, isize) {
        (2, 3)
    }

    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    fn ui_name(&self) -> &'static str {
        "Pentode"
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        vec![(0, -2), (-1, 0), (1, -1), (0, 1)]
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        ComboBox::from_label("Pentode Type")
            .selected_text(self.pentode_type.format_name())
            .show_ui(ui, |ui| {
                for t in PentodeType::iter() {
                    ui.selectable_value(&mut self.pentode_type, t, t.format_name());
                }
            });

        let valid_fidelities = self.pentode_type.available_fidelities();

        // If the newly selected triode does not support the current fidelity setting,
        // force a fallback to the first available supported fidelity.
        if !valid_fidelities.contains(&self.fidelity) {
            if let Some(fallback) = valid_fidelities.first() {
                self.fidelity = fallback.clone();
            }
        }

        ComboBox::from_label("Fidelity")
            .selected_text(format!("{:?}", self.fidelity))
            .show_ui(ui, |ui| {
                for f in valid_fidelities {
                    ui.selectable_value(&mut self.fidelity, f.clone(), format!("{:?}", f));
                }
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

        engine.draw_stacked_labels(name, self.pentode_type.format_name(), label_anchor);

        engine.draw_pin_marker(Vec2::new(0.3, -2.0), "P");
        engine.draw_pin_marker(Vec2::new(1.3, -1.3), "Screen");
        engine.draw_pin_marker(Vec2::new(-1.7, -0.3), "Control");
        engine.draw_pin_marker(Vec2::new(0.5, 1.0), "C");
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
        let stroke = Stroke::new(2.0, stroke_color);

        // Draw the Glass Envelope (Circle)
        // Offset center to y = -0.5 to balance the [-2.0, 1.0] vertical pin spread
        let envelope_center = center + rotate_vec(Vec2::new(0.0, -0.5) * zoom, rotation);
        let radius = 0.9;
        painter.circle(envelope_center, radius * zoom, fill_color, stroke);

        // --- Plate / Anode (Pin 0) ---
        let plate_pin = Vec2::new(0.0, -2.0);
        let plate_y = -1.1;
        let plate_w = 0.35;

        // Wire: Pin -> Plate Bar
        painter.line_segment(
            [
                center + rotate_vec(plate_pin * zoom, rotation),
                center + rotate_vec(Vec2::new(0.0, plate_y) * zoom, rotation),
            ],
            stroke,
        );
        // Plate Bar
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(-plate_w, plate_y) * zoom, rotation),
                center + rotate_vec(Vec2::new(plate_w, plate_y) * zoom, rotation),
            ],
            stroke,
        );

        // --- Cathode (Pin 3) ---
        let cathode_pin = Vec2::new(0.0, 1.0);
        let cathode_y = 0.1;
        let cathode_w = 0.35;

        // Wire: Pin -> Cathode Bar
        painter.line_segment(
            [
                center + rotate_vec(cathode_pin * zoom, rotation),
                center + rotate_vec(Vec2::new(0.0, cathode_y) * zoom, rotation),
            ],
            stroke,
        );
        // Cathode Bar
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(-cathode_w, cathode_y) * zoom, rotation),
                center + rotate_vec(Vec2::new(cathode_w, cathode_y) * zoom, rotation),
            ],
            stroke,
        );

        // --- Helper for drawing dashed grids ---
        let draw_grid = |y_pos: f32| {
            let num_dashes = 4;
            let total_grid_width = 0.7;
            let gap_width = 0.08;
            let dash_width =
                (total_grid_width - (gap_width * (num_dashes - 1) as f32)) / num_dashes as f32;

            let mut current_x = -total_grid_width / 2.0;

            for _ in 0..num_dashes {
                let dash_start = current_x;
                let dash_end = current_x + dash_width;

                painter.line_segment(
                    [
                        center + rotate_vec(Vec2::new(dash_start, y_pos) * zoom, rotation),
                        center + rotate_vec(Vec2::new(dash_end, y_pos) * zoom, rotation),
                    ],
                    stroke,
                );

                current_x = dash_end + gap_width;
            }
        };

        let g1_y = -0.2;
        let g2_y = -0.5;
        let g3_y = -0.8;

        // Draw the 3 parallel grids
        draw_grid(g1_y);
        draw_grid(g2_y);
        draw_grid(g3_y);

        // --- Control Grid / G1 (Pin 1) ---
        let g1_pin = Vec2::new(-1.0, 0.0);
        // Route: Pin -> Right to x=-0.6 -> Up to G1 height -> Right to grid edge
        painter.line_segment(
            [
                center + rotate_vec(g1_pin * zoom, rotation),
                center + rotate_vec(Vec2::new(-0.6, 0.0) * zoom, rotation),
            ],
            stroke,
        );
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(-0.6, 0.0) * zoom, rotation),
                center + rotate_vec(Vec2::new(-0.6, g1_y) * zoom, rotation),
            ],
            stroke,
        );
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(-0.6, g1_y) * zoom, rotation),
                center + rotate_vec(Vec2::new(-0.35, g1_y) * zoom, rotation),
            ],
            stroke,
        );

        // --- Screen Grid / G2 (Pin 2) ---
        let g2_pin = Vec2::new(1.0, -1.0);
        // Route: Pin -> Left to x=0.6 -> Down to G2 height -> Left to grid edge
        painter.line_segment(
            [
                center + rotate_vec(g2_pin * zoom, rotation),
                center + rotate_vec(Vec2::new(0.6, -1.0) * zoom, rotation),
            ],
            stroke,
        );
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(0.6, -1.0) * zoom, rotation),
                center + rotate_vec(Vec2::new(0.6, g2_y) * zoom, rotation),
            ],
            stroke,
        );
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(0.6, g2_y) * zoom, rotation),
                center + rotate_vec(Vec2::new(0.35, g2_y) * zoom, rotation),
            ],
            stroke,
        );

        // --- Suppressor Grid / G3 (Internal tie to Cathode) ---
        // Route: Left edge of G3 -> Left to x=-0.65 -> Down to Cathode height -> Right to Cathode edge
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(-0.35, g3_y) * zoom, rotation),
                center + rotate_vec(Vec2::new(-0.65, g3_y) * zoom, rotation),
            ],
            stroke,
        );
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(-0.65, g3_y) * zoom, rotation),
                center + rotate_vec(Vec2::new(-0.65, cathode_y) * zoom, rotation),
            ],
            stroke,
        );
        painter.line_segment(
            [
                center + rotate_vec(Vec2::new(-0.65, cathode_y) * zoom, rotation),
                center + rotate_vec(Vec2::new(-0.35, cathode_y) * zoom, rotation),
            ],
            stroke,
        );
    }
}
