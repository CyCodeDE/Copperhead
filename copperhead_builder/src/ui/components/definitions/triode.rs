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
use copperhead_core::components::triode::{TriodeDef, TriodeFidelity, TriodeType};
use egui::{Color32, ComboBox, Painter, Pos2, Stroke, Ui, Vec2};
use strum::IntoEnumIterator;

impl ComponentUIExt for TriodeDef {
    fn prefix(&self) -> &'static str {
        "T"
    }

    fn ui_name(&self) -> &'static str {
        "Triode"
    }

    fn size(&self) -> (isize, isize) {
        (2, 2)
    }

    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        vec![(0, -1), (-1, 0), (0, 1)]
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        ComboBox::from_label("Triode Type")
            .selected_text(self.triode_type.format_name())
            .show_ui(ui, |ui| {
                for t in TriodeType::iter() {
                    ui.selectable_value(&mut self.triode_type, t, t.format_name());
                }
            });

        let valid_fidelities = self.triode_type.available_fidelities();

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
        let radius = 0.7;
        painter.circle(center, radius * zoom, fill_color, stroke);

        // Draw Plate / Anode
        let plate_pin = Vec2::new(0.0, -1.0);
        let plate_y = -0.4;
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

        // Draw Cathode
        let cathode_pin = Vec2::new(0.0, 1.0);
        let cathode_y = 0.4;
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

        // Draw Control Grid
        let grid_pin = Vec2::new(-1.0, 0.0);
        let grid_wire_end = -0.4;

        // Wire: Pin -> Start of Grid
        painter.line_segment(
            [
                center + rotate_vec(grid_pin * zoom, rotation),
                center + rotate_vec(Vec2::new(grid_wire_end - 0.2, 0.0) * zoom, rotation),
            ],
            stroke,
        );

        // Grid Dashes
        let num_dashes = 3;
        let total_grid_width = 0.7;
        let gap_width = 0.06;
        let dash_width =
            (total_grid_width - (gap_width * (num_dashes - 1) as f32)) / num_dashes as f32;

        let mut current_x = -total_grid_width / 2.;

        for _ in 0..num_dashes {
            let dash_start = current_x;
            let dash_end = current_x + dash_width;

            painter.line_segment(
                [
                    center + rotate_vec(Vec2::new(dash_start, 0.0) * zoom, rotation),
                    center + rotate_vec(Vec2::new(dash_end, 0.0) * zoom, rotation),
                ],
                stroke,
            );

            current_x = dash_end + gap_width;
        }
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

        engine.draw_stacked_labels(name, self.triode_type.format_name(), label_anchor);

        engine.draw_pin_marker(Vec2::new(0.3, -1.0), "P");
        engine.draw_pin_marker(Vec2::new(-1.0, -0.3), "G");
        engine.draw_pin_marker(Vec2::new(0.3, 1.0), "C");
    }
}
