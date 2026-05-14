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
        _tx: &Sender<SimCommand>,
        ui: &mut Ui,
        _id: Option<usize>,
        _running: bool,
        name: &str,
    ) {
        CollapsingHeader::new(match &self.comment {
            Some(t) => format!("{t} ({name})"),
            None => name.to_string(),
        })
        .default_open(true)
        .show(ui, |ui| {
            if self.is_formula_mode() {
                let f = self.state_formula.as_deref().unwrap_or("");
                ui.label(format!("State: {f} (formula)"));
            } else {
                let state_str = if self.current_closed { "Closed" } else { "Open" };
                ui.label(format!("State: {} (param: {})", state_str, self.param_name));
            }
        });
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        let mut changed = false;

        // Parameter name (only editable when not in formula mode)
        if !self.is_formula_mode() {
            ui.horizontal(|ui| {
                ui.label("Parameter:");
                let edit_id = egui::Id::new("sw_param_name_orig");
                let resp = ui.text_edit_singleline(&mut self.param_name);
                if resp.gained_focus() {
                    ui.ctx().data_mut(|d| d.insert_temp(edit_id, self.param_name.clone()));
                }
                if resp.lost_focus() {
                    if let Some(original) = ui.ctx().data(|d| d.get_temp::<String>(edit_id)) {
                        if original != self.param_name {
                            for p in &mut app.state.parameters {
                                if p.name == original {
                                    p.name = self.param_name.clone();
                                    break;
                                }
                            }
                            changed = true;
                        }
                        ui.ctx().data_mut(|d| d.remove_temp::<String>(edit_id));
                    }
                }
            });
        }

        // Formula override
        ui.separator();
        ui.label("Formula override (optional — bypasses param):");
        let mut formula_str = self.state_formula.clone().unwrap_or_default();
        let resp = ui.text_edit_singleline(&mut formula_str);
        if resp.changed() {
            self.state_formula = if formula_str.trim().is_empty() {
                None
            } else {
                Some(formula_str.clone())
            };
            changed = true;
        }
        if resp.lost_focus() && !formula_str.trim().is_empty() {
            let valid = app
                .sim_state
                .param_system
                .as_deref()
                .map(|ps| ps.compile(formula_str.trim()).is_ok())
                .unwrap_or(true);
            if !valid {
                ui.colored_label(Color32::RED, "Invalid formula");
            }
        }
        if self.state_formula.is_some() {
            ui.small("Clear the field above to re-enable parameter mode.");
        }

        changed
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
