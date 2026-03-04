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
use egui::{Color32, Painter, Pos2, Shape, Stroke, Vec2};
use crate::ui::app::CircuitApp;
use crate::ui::components::definitions::ComponentUIExt;
use crate::ui::drawing::{rotate_vec, Anchor, LabelEngine};

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct LabelDef;

impl ComponentUIExt for LabelDef {
    fn prefix(&self) -> &'static str { "" }
    fn ui_name(&self) -> &'static str { "Label" }

    fn size(&self) -> (isize, isize) {
        (1, 1)
    }

    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> { vec![(0, 0)] }
    fn draw_modal(&mut self, _app: &mut CircuitApp, _ui: &mut egui::Ui) -> bool { false }
    fn draw_icon(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, fill_color: Color32, stroke_color: Color32) {
        let w = 0.6;
        let h = -0.5;

        let points = [
            Vec2::new(0.0, 0.0),
            Vec2::new(w, h / 2.),
            Vec2::new(0.0, h),
            Vec2::new(-w, h / 2.),
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

        let label_anchor = match rotation % 4 {
            0 => Anchor::Top,
            1 => Anchor::Right,
            2 => Anchor::Bottom,
            3 => Anchor::Left,
            _ => Anchor::Right,
        };

        engine.draw_text_at_anchor(name, label_anchor, false, Color32::WHITE);
    }
}

