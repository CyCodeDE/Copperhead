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
use crate::ui::util::{format_si_single, is_valid_param_str};
use copperhead_core::components::capacitor::CapacitorDef;
use egui::{Color32, Painter, Pos2, Stroke, Ui, Vec2};

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
        let ps = app.sim_state.param_system.as_deref();
        ui.horizontal(|ui| {
            ui.label("Capacitance (F):");
            let resp = ui.text_edit_singleline(&mut self.capacitance);
            if resp.lost_focus() && !is_valid_param_str(&self.capacitance, ps) && ps.is_some() {
                ui.colored_label(Color32::RED, "Invalid value or formula");
            }
        });
        ui.horizontal(|ui| {
            ui.label("ESR (Ω):");
            let resp = ui.text_edit_singleline(&mut self.esr);
            if resp.lost_focus() && !is_valid_param_str(&self.esr, ps) && ps.is_some() {
                ui.colored_label(Color32::RED, "Invalid value or formula");
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
        _fill_color: Color32,
        stroke_color: Color32,
    ) {
        let plate_gap = 0.15;
        let plate_height = 0.8;
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

        let formatted_value = if let Ok(v) = self.capacitance.trim().parse::<f64>() {
            format_si_single(v, 2) + "F"
        } else {
            self.capacitance.clone() + "F"
        };

        engine.draw_axial_labels(name, &formatted_value);
    }
}
