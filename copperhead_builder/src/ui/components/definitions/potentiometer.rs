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
use crate::ui::util::{format_si_single, is_valid_param_str};
use copperhead_core::components::potentiometer::{PotScale, PotentiometerDef};
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
        vec![(-1, 0), (1, 0), (0, 1)]
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
            let r_label = if let Ok(v) = self.resistance.trim().parse::<f64>() {
                format_si_single(v, 2) + "Ω"
            } else {
                self.resistance.clone() + "Ω"
            };
            ui.label(format!("R: {r_label}  ·  {}", self.scale.label()));

            if self.is_formula_mode() {
                let f = self.position_formula.as_deref().unwrap_or("");
                ui.label(format!("Position: {f} (formula)"));
            } else {
                ui.label(format!("Position: {} (param)", self.param_name));
            }
        });
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        let mut changed = false;
        let ps = app.sim_state.param_system.as_deref();

        // Resistance
        ui.horizontal(|ui| {
            ui.label("Resistance (Ω):");
            let resp = ui.text_edit_singleline(&mut self.resistance);
            if resp.lost_focus() && !is_valid_param_str(&self.resistance, ps) && ps.is_some() {
                ui.colored_label(Color32::RED, "Invalid");
            }
            if resp.changed() {
                changed = true;
            }
        });

        // Parameter name (only editable when not in formula mode)
        if !self.is_formula_mode() {
            ui.horizontal(|ui| {
                ui.label("Parameter:");
                let edit_id = egui::Id::new("pot_param_name_orig");
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

        // Scale picker
        ui.horizontal(|ui| {
            ui.label("Scale:");
            let cur_linear = matches!(self.scale, PotScale::Linear);
            if ui.selectable_label(cur_linear, "Linear").clicked() && !cur_linear {
                self.scale = PotScale::Linear;
                changed = true;
            }
            if ui.selectable_label(!cur_linear, "Audio Taper").clicked() && cur_linear {
                self.scale = PotScale::AudioTaper;
                changed = true;
            }
        });

        // Formula override
        ui.separator();
        ui.label("Formula override (optional — bypasses param and scale):");
        let mut formula_str = self.position_formula.clone().unwrap_or_default();
        let resp = ui.text_edit_singleline(&mut formula_str);
        if resp.changed() {
            self.position_formula = if formula_str.trim().is_empty() {
                None
            } else {
                Some(formula_str.clone())
            };
            changed = true;
        }
        if resp.lost_focus() && !formula_str.trim().is_empty() {
            let valid = ps
                .map(|ps| ps.compile(formula_str.trim()).is_ok())
                .unwrap_or(true);
            if !valid {
                ui.colored_label(Color32::RED, "Invalid formula");
            }
        }
        if self.position_formula.is_some() {
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

        // Position indicator based on current_position
        let pos_f = self.current_position.clamp(0.0, 1.0) as f32;
        let indicator_x = -half_w + pos_f * (2.0 * half_w);
        let ind_p1 = center + rotate_vec(Vec2::new(indicator_x, -half_h * 0.6) * zoom, rotation);
        let ind_p2 = center + rotate_vec(Vec2::new(indicator_x, half_h * 0.6) * zoom, rotation);
        painter.line_segment([ind_p1, ind_p2], Stroke::new(1.0, stroke_color));

        let wiper_start = Vec2::new(0.0, 1.0);
        let wiper_end = Vec2::new(0.0, 0.5);
        painter.line_segment(
            [
                center + rotate_vec(wiper_start * zoom, rotation),
                center + rotate_vec(wiper_end * zoom, rotation),
            ],
            Stroke::new(1.5, stroke_color),
        );

        let arrow_size = 0.25;
        let arrow_tip = Vec2::new(0.0, 0.5);
        let arrow_left = Vec2::new(-arrow_size * 0.5, 0.5 + arrow_size);
        let arrow_right = Vec2::new(arrow_size * 0.5, 0.5 + arrow_size);

        let rotated_arrow: Vec<Pos2> = [arrow_tip, arrow_left, arrow_right]
            .iter()
            .map(|&p| center + rotate_vec(p * zoom, rotation))
            .collect();

        painter.add(Shape::convex_polygon(
            rotated_arrow,
            fill_color,
            Stroke::new(1.5, stroke_color),
        ));
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());

        let formatted_value = if let Ok(v) = self.resistance.trim().parse::<f64>() {
            format_si_single(v, 2) + "Ω"
        } else {
            self.resistance.clone() + "Ω"
        };

        let anchor = match rotation % 4 {
            0 => Anchor::Top,
            1 => Anchor::Right,
            2 => Anchor::Bottom,
            3 => Anchor::Left,
            _ => Anchor::Top,
        };

        engine.draw_stacked_labels(name, &formatted_value, anchor);
    }
}
