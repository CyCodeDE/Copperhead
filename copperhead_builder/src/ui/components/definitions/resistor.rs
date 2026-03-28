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
use copperhead_core::components::resistor::ResistorDef;
use egui::{Color32, Painter, Pos2, Shape, Stroke, Ui, Vec2};

impl ComponentUIExt for ResistorDef {
    fn prefix(&self) -> &'static str {
        "R"
    }

    fn ui_name(&self) -> &'static str {
        "Resistor"
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
            ui.label("Resistance:");
            ui.add(
                egui::DragValue::new(&mut self.resistance)
                    .speed(10.0)
                    .range(0.0..=f64::INFINITY)
                    .suffix("Ω")
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
        let half_w = 1.0;
        let half_h = 0.5;

        let points = [
            Vec2::new(-half_w, -half_h),
            Vec2::new(half_w, -half_h),
            Vec2::new(half_w, half_h),
            Vec2::new(-half_w, half_h),
        ];

        let rotated_points: Vec<Pos2> = points
            .iter()
            .map(|&p| center + rotate_vec(p * zoom, rotation))
            .collect();

        painter.add(Shape::convex_polygon(
            rotated_points,
            fill_color,
            Stroke::new(1.5, stroke_color),
        ));
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        let formatted_value = format_si_single(self.resistance, 2) + "Ω";

        engine.draw_axial_labels(name, &formatted_value);
    }
}
