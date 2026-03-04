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
use crate::ui::drawing::{rotate_vec, LabelEngine};
use crate::ui::util::{format_si_single, parse_si};
use egui::{Color32, ComboBox, Painter, Pos2, Shape, Stroke, Ui, Vec2};
use copperhead_core::components::diode::{DiodeDef, DiodeModel};

impl ComponentUIExt for DiodeDef {
    fn prefix(&self) -> &'static str {
        "D"
    }

    fn ui_name(&self) -> &'static str {
        "Diode"
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
            ui.label("Model:");
            ComboBox::from_id_salt("diode_combo")
                .selected_text(self.model.format_name())
                .show_ui(ui, |ui| {
                    ui.selectable_value(
                        &mut self.model,
                        DiodeModel::_1N4148,
                        DiodeModel::_1N4148.format_name(),
                    );
                });
        });

        false
    }

    fn draw_icon(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, fill_color: Color32, stroke_color: Color32) {
        let stroke = Stroke::new(2.0, stroke_color);

        let half_len = 0.5;
        let half_h = 0.5;

        let lead_anode_start = rotate_vec(Vec2::new(-1.0, 0.0) * zoom, rotation);
        let lead_anode_end = rotate_vec(Vec2::new(-half_len, 0.0) * zoom, rotation);
        painter.line_segment([center + lead_anode_start, center + lead_anode_end], stroke);

        let lead_cathode_start = rotate_vec(Vec2::new(half_len, 0.0) * zoom, rotation);
        let lead_cathode_end = rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);
        painter.line_segment(
            [center + lead_cathode_start, center + lead_cathode_end],
            stroke,
        );

        let triangle_points_local = [
            Vec2::new(-half_len, -half_h),
            Vec2::new(-half_len, half_h),
            Vec2::new(half_len, 0.0),
        ];

        let triangle_points_screen: Vec<Pos2> = triangle_points_local
            .iter()
            .map(|&p| center + rotate_vec(p * zoom, rotation))
            .collect();

        painter.add(Shape::convex_polygon(
            triangle_points_screen,
            fill_color,
            stroke,
        ));

        let bar_top = center + rotate_vec(Vec2::new(half_len, -half_h) * zoom, rotation);
        let bar_bot = center + rotate_vec(Vec2::new(half_len, half_h) * zoom, rotation);

        painter.line_segment([bar_top, bar_bot], stroke);
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        engine.draw_axial_labels(name, self.model.format_name());
    }
}