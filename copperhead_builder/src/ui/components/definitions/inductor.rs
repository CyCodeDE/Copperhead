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
use crate::ui::drawing::{LabelEngine, rotate_vec};
use crate::ui::util::{format_si_single, parse_si};
use copperhead_core::components::inductor::InductorDef;
use egui::{Color32, Painter, Pos2, Stroke, Ui, Vec2};

impl ComponentUIExt for InductorDef {
    fn prefix(&self) -> &'static str {
        "L"
    }

    fn ui_name(&self) -> &'static str {
        "Inductor"
    }

    fn size(&self) -> (isize, isize) {
        (2, 1)
    }

    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        vec![(-1, 0), (1, 0)]
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        ui.horizontal(|ui| {
            ui.label("Inductance:");
            ui.add(
                egui::DragValue::new(&mut self.inductance)
                    .suffix("H")
                    .speed(1e-4)
                    .range(0.0..=f64::INFINITY)
                    .custom_formatter(|val, _range| format_si_single(val, 3))
                    .custom_parser(|text| parse_si(text)),
            );
        });

        ui.horizontal(|ui| {
            ui.label("Series Resistance:");
            ui.add(
                egui::DragValue::new(&mut self.series_resistance)
                    .suffix("Ω")
                    .speed(1e-4)
                    .range(0.0..=f64::INFINITY)
                    .custom_formatter(|val, _range| format_si_single(val, 3))
                    .custom_parser(|text| parse_si(text)),
            );
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

        let num_coils = 4;
        let lead_length = 0.2; // Straight wire segment at ends
        let total_width = 2.0;

        let coil_section_width = total_width - (2.0 * lead_length);
        let loop_width = coil_section_width / num_coils as f32;
        let loop_radius = loop_width / 2.0;

        let k = 1.33333;
        let ctrl_height = loop_radius * k;

        let start_x = -1.0;
        let coil_start_x = -1.0 + lead_length;

        let p_lead_start = center + rotate_vec(Vec2::new(start_x, 0.0) * zoom, rotation);
        let p_coil_start = center + rotate_vec(Vec2::new(coil_start_x, 0.0) * zoom, rotation);

        painter.line_segment([p_lead_start, p_coil_start], stroke);

        let mut current_x = coil_start_x;

        for _ in 0..num_coils {
            let next_x = current_x + loop_width;

            let p1_local = Vec2::new(current_x, 0.0);
            let p2_local = Vec2::new(next_x, 0.0);

            let c1_local = Vec2::new(current_x, -ctrl_height);
            let c2_local = Vec2::new(next_x, -ctrl_height);

            let p1 = center + rotate_vec(p1_local * zoom, rotation);
            let p2 = center + rotate_vec(p2_local * zoom, rotation);
            let c1 = center + rotate_vec(c1_local * zoom, rotation);
            let c2 = center + rotate_vec(c2_local * zoom, rotation);

            let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                [p1, c1, c2, p2],
                false,
                Color32::TRANSPARENT,
                stroke,
            );
            painter.add(bezier);

            current_x = next_x;
        }

        let p_coil_end = center + rotate_vec(Vec2::new(current_x, 0.0) * zoom, rotation);
        let p_lead_end = center + rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);

        painter.line_segment([p_coil_end, p_lead_end], stroke);
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        let formatted_value = format_si_single(self.inductance, 2) + "H";

        engine.draw_axial_labels(name, &formatted_value);
    }
}
