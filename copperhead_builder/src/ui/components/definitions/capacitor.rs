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
use copperhead_core::components::capacitor::CapacitorDef;
use egui::{Color32, Painter, Pos2, Shape, Stroke, Ui, Vec2};

impl ComponentUIExt for CapacitorDef {
    fn prefix(&self) -> &'static str {
        "C"
    }

    fn ui_name(&self) -> &'static str {
        "Capacitor"
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
            ui.label("Capacitance:");
            ui.add(
                egui::DragValue::new(&mut self.capacitance)
                    .suffix("F")
                    .speed(1e-7)
                    .range(0.0..=f64::INFINITY)
                    .custom_formatter(|val, _range| {
                        format_si_single(val, 3)
                    })
                    .custom_parser(|text| parse_si(text)),
            );
        });
        ui.horizontal(|ui| {
            ui.label("ESR:");
            ui.add(
                egui::DragValue::new(&mut self.esr)
                    .suffix("Ω")
                    .speed(1e-4)
                    .range(0.0..=f64::INFINITY)
                    .custom_formatter(|val, _range| {
                        format_si_single(val, 3)
                    })
                    .custom_parser(|text| parse_si(text)),
            );
        });

        false
    }

    fn draw_icon(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, fill_color: Color32, stroke_color: Color32) {
        let plate_gap = 0.15; // Distance from center to plate
        let plate_height = 0.8; // Total height of the plate
        let stroke = Stroke::new(2.0, stroke_color);

        let wire_left_start = rotate_vec(Vec2::new(-1.0, 0.0) * zoom, rotation);
        let wire_left_end = rotate_vec(Vec2::new(-plate_gap, 0.0) * zoom, rotation);
        painter.line_segment([center + wire_left_start, center + wire_left_end], stroke);

        let wire_right_start = rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);
        let wire_right_end = rotate_vec(Vec2::new(plate_gap, 0.0) * zoom, rotation);
        painter.line_segment([center + wire_right_start, center + wire_right_end], stroke);

        let p1_top = rotate_vec(Vec2::new(-plate_gap, -plate_height / 2.0) * zoom, rotation);
        let p1_bot = rotate_vec(Vec2::new(-plate_gap, plate_height / 2.0) * zoom, rotation);
        painter.line_segment([center + p1_top, center + p1_bot], stroke);

        let p2_top = rotate_vec(Vec2::new(plate_gap, -plate_height / 2.0) * zoom, rotation);
        let p2_bot = rotate_vec(Vec2::new(plate_gap, plate_height / 2.0) * zoom, rotation);
        painter.line_segment([center + p2_top, center + p2_bot], stroke);
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        let formatted_value = format_si_single(self.capacitance, 2) + "F";

        engine.draw_axial_labels(name, &formatted_value);
    }
}