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

use crate::ui::SimCommand;
use crate::ui::app::CircuitApp;
use crate::ui::components::definitions::{ComponentUIExt};
use crate::ui::drawing::{Anchor, LabelEngine, rotate_vec};
use crate::ui::util::{format_si_single, parse_si};
use copperhead_core::components::potentiometer::PotentiometerDef;
use crossbeam::channel::Sender;
use egui::{CollapsingHeader, Color32, Painter, Pos2, Shape, Stroke, Ui, Vec2};

impl ComponentUIExt for PotentiometerDef {
    fn prefix(&self) -> &'static str {
        "VR"
    }

    fn ui_name(&self) -> &'static str {
        "Potentiometer"
    }

    fn comment(&self) -> Option<String> {
        self.comment.clone()
    }

    fn size(&self) -> (isize, isize) {
        (2, 2)
    }

    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        // Order: A, B, Wiper
        vec![(-1, 0), (1, 0), (0, 1)]
    }

    fn draw_property_panel(
        &mut self,
        tx: &Sender<SimCommand>,
        ui: &mut Ui,
        id: Option<usize>,
        running: bool,
        name: &str,
    ) {
        CollapsingHeader::new(match &self.comment {
            Some(t) => format!("{t}({name})"),
            None => name.to_string(),
        })
        .default_open(true)
        .show(ui, |ui| {
            ui.horizontal(|ui| {
                let response = ui.add(
                    egui::Slider::new(&mut self.position, self.min..=self.max)
                        .step_by(self.step)
                        .text("Position"),
                );

                if running && (response.drag_stopped() || response.lost_focus()) {
                    let _ = tx.send(SimCommand::UpdateValue {
                        component_idx: id.expect("Component idx shouldn't be None"),
                        name: "position".to_string(),
                        value: self.position,
                    });
                }

                ui.menu_button("⚙", |ui| {
                    ui.label("Potentiometer Settings");
                    ui.add(
                        egui::DragValue::new(&mut self.min)
                            .prefix("Min: ")
                            .speed(0.01),
                    );
                    ui.add(
                        egui::DragValue::new(&mut self.max)
                            .prefix("Max: ")
                            .speed(0.01),
                    );
                    ui.add(
                        egui::DragValue::new(&mut self.step)
                            .prefix("Step: ")
                            .speed(0.01),
                    );
                });
            });
        });
    }

    fn draw_modal(&mut self, _app: &mut CircuitApp, ui: &mut Ui) -> bool {
        let mut changed = false;
        ui.horizontal(|ui| {
            ui.label("Resistance:");
            if ui
                .add(
                    egui::DragValue::new(&mut self.resistance)
                        .speed(10.0)
                        .range(0.0..=f64::INFINITY)
                        .suffix("Ω")
                        .custom_formatter(|val, _range| format_si_single(val, 3))
                        .custom_parser(|text| parse_si(text)),
                )
                .changed()
            {
                changed = true;
            }
        });
        ui.horizontal(|ui| {
            ui.label("Wiper Position:");
            if ui
                .add(
                    egui::DragValue::new(&mut self.position)
                        .speed(0.01)
                        .range(self.min..=self.max)
                        .suffix("%")
                        .custom_formatter(|val, _range| format!("{:.1}", val * 100.0)),
                )
                .changed()
            {
                changed = true;
            }
        });

        changed
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

        // Resistor body
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

        // Position Indicator
        let indicator_x = -half_w + (self.position as f32) * (2.0 * half_w);
        let ind_p1 = center + rotate_vec(Vec2::new(indicator_x, -half_h * 0.6) * zoom, rotation);
        let ind_p2 = center + rotate_vec(Vec2::new(indicator_x, half_h * 0.6) * zoom, rotation);

        painter.line_segment([ind_p1, ind_p2], Stroke::new(1.0, stroke_color));

        // Wiper
        let wiper_start = Vec2::new(0.0, 1.0);
        let wiper_end = Vec2::new(0.0, 0.5);

        let wiper_line_start_pos = center + rotate_vec(wiper_start * zoom, rotation);
        let wiper_line_end_pos = center + rotate_vec(wiper_end * zoom, rotation);

        painter.line_segment(
            [wiper_line_start_pos, wiper_line_end_pos],
            Stroke::new(1.5, stroke_color),
        );

        // Arrow head
        let arrow_size = 0.25;
        // Arrow points up towards the resistor body (negative Y in local coords relative to start? No, Y decreases going up)
        // Wait, Y=1 is bottom, Y=0.5 is body edge.
        // Arrow should point towards Y=0.5.
        // Triangle base at bottom, tip at top.
        let arrow_tip = Vec2::new(0.0, 0.5);
        let arrow_left = Vec2::new(-arrow_size * 0.5, 0.5 + arrow_size);
        let arrow_right = Vec2::new(arrow_size * 0.5, 0.5 + arrow_size);

        let arrow_points = [arrow_tip, arrow_left, arrow_right];
        let rotated_arrow_points: Vec<Pos2> = arrow_points
            .iter()
            .map(|&p| center + rotate_vec(p * zoom, rotation))
            .collect();

        painter.add(Shape::convex_polygon(
            rotated_arrow_points,
            fill_color,
            Stroke::new(1.5, stroke_color),
        ));
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        let formatted_value = format_si_single(self.resistance, 2) + "Ω";

        // Always draw labels on the side opposite to the wiper
        // Wiper is at (0, 1) in local space
        let anchor = match rotation % 4 {
            0 => Anchor::Top,    // Wiper is Bottom (0, 1) -> Labels Top
            1 => Anchor::Right,  // Wiper is Left (-1, 0) -> Labels Right
            2 => Anchor::Bottom, // Wiper is Top (0, -1) -> Labels Bottom
            3 => Anchor::Left,   // Wiper is Right (1, 0) -> Labels Left
            _ => Anchor::Top,
        };

        engine.draw_stacked_labels(name, &formatted_value, anchor);
    }
}
