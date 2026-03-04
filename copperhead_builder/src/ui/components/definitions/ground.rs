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
use crate::ui::drawing::rotate_vec;
use egui::{Color32, Painter, Pos2, Stroke, Vec2};

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct GroundDef;

impl ComponentUIExt for GroundDef {
    fn prefix(&self) -> &'static str {
        ""
    }
    fn ui_name(&self) -> &'static str {
        "Ground"
    }
    fn local_pins(&self) -> Vec<(isize, isize)> {
        vec![(0, 0)]
    }
    fn draw_modal(&mut self, _app: &mut CircuitApp, _ui: &mut egui::Ui) -> bool {
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

        let l1_start = center + rotate_vec(Vec2::new(-0.4, 0.0) * zoom, rotation);
        let l1_end = center + rotate_vec(Vec2::new(0.4, 0.0) * zoom, rotation);
        painter.line_segment([l1_start, l1_end], stroke);

        let l2_y_offset = 0.15;
        let l2_start = center + rotate_vec(Vec2::new(-0.25, l2_y_offset) * zoom, rotation);
        let l2_end = center + rotate_vec(Vec2::new(0.25, l2_y_offset) * zoom, rotation);
        painter.line_segment([l2_start, l2_end], stroke);

        let l3_y_offset = 0.3;
        let l3_start = center + rotate_vec(Vec2::new(-0.1, l3_y_offset) * zoom, rotation);
        let l3_end = center + rotate_vec(Vec2::new(0.1, l3_y_offset) * zoom, rotation);
        painter.line_segment([l3_start, l3_end], stroke);
    }
}
