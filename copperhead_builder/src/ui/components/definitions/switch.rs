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
use crate::ui::components::definitions::ComponentUIExt;
use crate::ui::drawing::{Anchor, LabelEngine, rotate_vec};
use copperhead_core::components::switch::SwitchDef;
use crossbeam::channel::Sender;
use egui::{Color32, CollapsingHeader, Painter, Pos2, Stroke, Ui, Vec2};

impl ComponentUIExt for SwitchDef {
    fn prefix(&self) -> &'static str {
        "S"
    }

    fn ui_name(&self) -> &'static str {
        "Switch"
    }

    fn comment(&self) -> Option<String> {
        self.comment.clone()
    }

    fn size(&self) -> (isize, isize) {
        (1, 1)
    }

    fn offset(&self) -> (f32, f32) {
        (-0.5, 0.)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        vec![(-1, 0), (0, 0)]
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
            // Show a checkbox when the value is a simple constant; otherwise show a formula hint.
            let is_formula = !matches!(
                self.closed.trim().to_lowercase().as_str(),
                "0" | "1" | "true" | "false"
            ) && self.closed.trim().parse::<f64>().is_err();

            if is_formula {
                ui.label(format!("Closed: {} (formula)", self.closed));
            } else {
                let mut is_closed = self.is_closed();
                let response = ui.add(egui::Checkbox::new(&mut is_closed, "Closed"));
                if response.changed() {
                    self.closed = if is_closed { "1".to_string() } else { "0".to_string() };
                    if running {
                        let _ = tx.send(SimCommand::UpdateValue {
                            component_idx: id.expect("Component idx shouldn't be None"),
                            name: "closed".to_string(),
                            value: if is_closed { 1.0 } else { 0.0 },
                        });
                    }
                }
            }
        });
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        ui.horizontal(|ui| {
            ui.label("Closed (0/1 or formula):");
            let resp = ui.text_edit_singleline(&mut self.closed);
            if resp.lost_focus() {
                let is_literal = matches!(
                    self.closed.trim().to_lowercase().as_str(),
                    "0" | "1" | "true" | "false"
                ) || self.closed.trim().parse::<f64>().is_ok();
                let is_formula = !is_literal
                    && app
                        .sim_state
                        .param_system
                        .as_ref()
                        .map(|ps| ps.compile(&self.closed).is_ok())
                        .unwrap_or(false);
                if !is_literal && !is_formula && app.sim_state.param_system.is_some() {
                    ui.colored_label(Color32::RED, "Invalid value or formula");
                }
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
        let stroke = Stroke::new(2.5, stroke_color);

        let pin_l = Vec2::new(-1., 0.0);
        let pin_r = Vec2::new(0., 0.0);

        let closed = self.is_closed();
        let lever_end = if closed {
            pin_r
        } else {
            let len = 1.;
            let angle = -30.0f32.to_radians();
            Vec2::new(pin_l.x + len * angle.cos(), pin_l.y + len * angle.sin())
        };

        painter.line_segment(
            [
                center + rotate_vec(pin_l * zoom, rotation),
                center + rotate_vec(lever_end * zoom, rotation),
            ],
            stroke,
        );
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        let state_label = if self.is_closed() { "Closed" } else { "Open" };

        let anchor = match rotation % 4 {
            0 => Anchor::Top,
            1 => Anchor::Right,
            2 => Anchor::Bottom,
            3 => Anchor::Left,
            _ => Anchor::Top,
        };

        engine.draw_stacked_labels(name, state_label, anchor);
    }
}
