use crate::ui::app::CircuitApp;
use crate::ui::components::definitions::ComponentUIExt;
use crate::ui::drawing::{rotate_vec, LabelEngine};
use crate::ui::util::{format_si_single, parse_si};
use copperhead_core::components::transformer::{
    Coupling, TransformerDef, Winding, WindingLocation,
};
use egui::{Color32, Painter, Pos2, Rect, Stroke, Ui, Vec2};

impl ComponentUIExt for TransformerDef {
    fn prefix(&self) -> &'static str {
        "TR"
    }

    fn ui_name(&self) -> &'static str {
        "Transformer"
    }

    fn size(&self) -> (isize, isize) {
        if self.windings.is_empty() {
            return (0, 0);
        }

        let mut left_count = 0;
        let mut right_count = 0;

        for w in &self.windings {
            match w.location {
                WindingLocation::Left => left_count += 1,
                WindingLocation::Right => right_count += 1,
            }
        }

        let width = if left_count > 0 && right_count > 0 {
            2
        } else {
            1
        };

        let left_height = if left_count > 0 {
            (left_count * 3) - 1
        } else {
            0
        };

        let right_height = if right_count > 0 {
            (right_count * 3) - 1
        } else {
            0
        };

        let height = left_height.max(right_height);

        (width, height)
    }

    fn offset(&self) -> (f32, f32) {
        if self.windings.is_empty() {
            return (0.0, 0.0);
        }

        let mut left_count = 0;
        let mut right_count = 0;

        for w in &self.windings {
            match w.location {
                WindingLocation::Left => left_count += 1,
                WindingLocation::Right => right_count += 1,
            }
        }

        let x_center = if left_count > 0 && right_count > 0 {
            0.0
        } else if left_count > 0 {
            -0.5
        } else if right_count > 0 {
            0.5
        } else {
            0.0
        };

        let max_windings = left_count.max(right_count);
        let y_center = if max_windings > 0 {
            ((max_windings as f32 * 3.0) - 3.0) / 2.0
        } else {
            0.0
        };

        (x_center, y_center)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        let mut pins = Vec::new();
        let mut left_y = -1;
        let mut right_y = -1;

        for w in &self.windings {
            match w.location {
                WindingLocation::Left => {
                    pins.push((-1, left_y));
                    pins.push((-1, left_y + 2));
                    left_y += 3;
                }
                WindingLocation::Right => {
                    pins.push((1, right_y));
                    pins.push((1, right_y + 2));
                    right_y += 3;
                }
            }
        }
        pins
    }

    fn draw_modal(&mut self, _app: &mut CircuitApp, ui: &mut Ui) -> bool {
        let mut changed = false;

        // Wrap everything in a scroll area so the modal doesn't run off the screen
        egui::ScrollArea::vertical()
            .auto_shrink([false, true])
            .show(ui, |ui| {
                ui.horizontal(|ui| {
                    ui.label("Saturation Current:");
                    ui.add(
                        egui::DragValue::new(&mut self.i_sat)
                            .suffix("A")
                            .speed(1e-3)
                            .range(0.0..=f64::INFINITY)
                            .custom_formatter(|val, _range| format_si_single(val, 3))
                            .custom_parser(|text| parse_si(text)));
                });
                ui.horizontal(|ui| {
                    ui.label("Saturation Flux:");
                    ui.add(
                        egui::DragValue::new(&mut self.phi_sat)
                            .suffix("Wb")
                            .speed(1e-3)
                            .range(0.0..=f64::INFINITY)
                            .custom_formatter(|val, _range| format_si_single(val, 3))
                            .custom_parser(|text| parse_si(text)));
                });
                ui.add_space(10.0);
                changed |= draw_windings_section(self, ui);
                ui.add_space(10.0);
                ui.separator();
                ui.add_space(10.0);
                changed |= draw_couplings_section(self, ui);
            });

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
        let stroke = Stroke::new(2.0, stroke_color);

        let mut left_y = -1;
        let mut right_y = -1;

        let draw_winding = |start_pos: Vec2, end_pos: Vec2, phase_inverted: bool, is_left: bool| {
            let num_coils = 4;
            let lead_length = 0.2;
            let total_length = (end_pos - start_pos).length();

            let dir = (end_pos - start_pos).normalized();
            let coil_normal = if is_left {
                Vec2::new(-1.0, 0.0)
            } else {
                Vec2::new(1.0, 0.0)
            };

            let coil_section_length = total_length - (2.0 * lead_length);
            let loop_length = coil_section_length / num_coils as f32;
            let loop_radius = loop_length / 2.0;

            let k = 1.33333;
            let ctrl_height = loop_radius * k;

            let p_start = start_pos;
            let p_coil_start = start_pos + dir * lead_length;

            painter.line_segment(
                [
                    center + rotate_vec(p_start * zoom, rotation),
                    center + rotate_vec(p_coil_start * zoom, rotation),
                ],
                stroke,
            );

            let mut current_p = p_coil_start;

            for _ in 0..num_coils {
                let next_p = current_p + dir * loop_length;

                let c1_local = current_p + coil_normal * ctrl_height;
                let c2_local = next_p + coil_normal * ctrl_height;

                let p1 = center + rotate_vec(current_p * zoom, rotation);
                let p2 = center + rotate_vec(next_p * zoom, rotation);
                let c1 = center + rotate_vec(c1_local * zoom, rotation);
                let c2 = center + rotate_vec(c2_local * zoom, rotation);

                let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                    [p1, c1, c2, p2],
                    false,
                    Color32::TRANSPARENT,
                    stroke,
                );
                painter.add(bezier);

                current_p = next_p;
            }

            let p_end = end_pos;
            painter.line_segment(
                [
                    center + rotate_vec(current_p * zoom, rotation),
                    center + rotate_vec(p_end * zoom, rotation),
                ],
                stroke,
            );

            // Draw dot for phase
            let dot_pos = if phase_inverted {
                p_end - dir * 0.4 + coil_normal * 0.4
            } else {
                p_start + dir * 0.4 + coil_normal * 0.4
            };
            painter.circle_filled(
                center + rotate_vec(dot_pos * zoom, rotation),
                0.1 * zoom,
                stroke_color,
            );
        };

        for w in &self.windings {
            match w.location {
                WindingLocation::Left => {
                    draw_winding(
                        Vec2::new(-1.0, left_y as f32),
                        Vec2::new(-1.0, (left_y + 2) as f32),
                        w.phase_inverted,
                        true,
                    );
                    left_y += 3;
                }
                WindingLocation::Right => {
                    draw_winding(
                        Vec2::new(1.0, right_y as f32),
                        Vec2::new(1.0, (right_y + 2) as f32),
                        w.phase_inverted,
                        false,
                    );
                    right_y += 3;
                }
            }
        }

        // Draw core
        let max_y = (left_y - 3).max(right_y - 3).max(-1);
        if self.windings.len() > 0 && max_y >= -1 {
            let core_start_y = -1.0;
            let core_end_y = max_y as f32 + 2.0;

            let core_left = -0.15;
            let core_right = 0.15;

            painter.line_segment(
                [
                    center + rotate_vec(Vec2::new(core_left, core_start_y) * zoom, rotation),
                    center + rotate_vec(Vec2::new(core_left, core_end_y) * zoom, rotation),
                ],
                stroke,
            );

            painter.line_segment(
                [
                    center + rotate_vec(Vec2::new(core_right, core_start_y) * zoom, rotation),
                    center + rotate_vec(Vec2::new(core_right, core_end_y) * zoom, rotation),
                ],
                stroke,
            );
        }
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let engine = LabelEngine::new(painter, center, rotation, zoom, self.size(), self.offset());
        engine.draw_axial_labels(name, "TR");
    }
}

fn draw_windings_section(def: &mut TransformerDef, ui: &mut Ui) -> bool {
    let mut changed = false;
    let mut to_remove = None;
    let mut added_this_frame = false;

    ui.horizontal(|ui| {
        ui.heading("Windings");
        if ui.button("+ Add").clicked() {
            def.windings.push(Winding {
                node_a: copperhead_core::model::NodeId(0),
                node_b: copperhead_core::model::NodeId(0),
                inductance: 1.0,
                series_resistance: 0.1,
                location: WindingLocation::Right,
                phase_inverted: false,
            });
            added_this_frame = true;
            changed = true;
        }
    });

    ui.add_space(5.0);

    let new_winding_idx = def.windings.len().saturating_sub(1);

    for (i, winding) in def.windings.iter_mut().enumerate() {
        let mut header = egui::CollapsingHeader::new(format!("Winding {}", i))
            .default_open(true)
            .id_source(format!("winding_header_{}", i));

        // Force the state only on the frame a new winding is added
        if added_this_frame {
            header = header.open(Some(i == new_winding_idx));
        }

        header.show(ui, |ui| {
            egui::Grid::new(format!("winding_grid_{}", i))
                .num_columns(2)
                .spacing([40.0, 8.0])
                .show(ui, |ui| {
                    ui.label("Location:");
                    ui.horizontal(|ui| {
                        changed |= ui.selectable_value(&mut winding.location, WindingLocation::Left, "Left").changed();
                        changed |= ui.selectable_value(&mut winding.location, WindingLocation::Right, "Right").changed();
                    });
                    ui.end_row();

                    ui.label("Inductance:");
                    changed |= ui.add(
                        egui::DragValue::new(&mut winding.inductance)
                            .suffix("H")
                            .speed(1e-4)
                            .range(0.0..=f64::INFINITY)
                            .custom_formatter(|val, _range| format_si_single(val, 3))
                            .custom_parser(|text| parse_si(text)),
                    ).changed();
                    ui.end_row();

                    ui.label("Series Resistance:");
                    changed |= ui.add(
                        egui::DragValue::new(&mut winding.series_resistance)
                            .suffix("Ω")
                            .speed(1e-4)
                            .range(0.0..=f64::INFINITY)
                            .custom_formatter(|val, _range| format_si_single(val, 3))
                            .custom_parser(|text| parse_si(text)),
                    ).changed();
                    ui.end_row();

                    ui.label("Phase Inverted:");
                    changed |= ui.checkbox(&mut winding.phase_inverted, "").changed();
                    ui.end_row();
                });

            ui.add_space(5.0);
            if ui.button("🗑 Remove Winding").clicked() {
                to_remove = Some(i);
                changed = true;
            }
        });
    }

    if let Some(i) = to_remove {
        def.windings.remove(i);
        def.couplings.retain(|c| c.winding_1 != i && c.winding_2 != i);
        for c in &mut def.couplings {
            if c.winding_1 > i { c.winding_1 -= 1; }
            if c.winding_2 > i { c.winding_2 -= 1; }
        }
    }

    changed
}

fn draw_couplings_section(def: &mut TransformerDef, ui: &mut Ui) -> bool {
    let mut changed = false;
    let mut coupling_to_remove = None;
    let num_windings = def.windings.len();

    ui.horizontal(|ui| {
        ui.heading("Couplings");
        if ui.button("+ Add").clicked() {
            def.couplings.push(Coupling {
                winding_1: 0,
                winding_2: if num_windings > 1 { 1 } else { 0 },
                k: 0.99,
            });
            changed = true;
        }
    });

    ui.add_space(5.0);

    if num_windings < 2 {
        ui.label(egui::RichText::new("Requires at least 2 windings to couple.").italics().color(egui::Color32::GRAY));
        return changed;
    }

    for (i, coupling) in def.couplings.iter_mut().enumerate() {
        ui.group(|ui| {
            ui.horizontal(|ui| {
                ui.label("K:");
                changed |= ui.add(egui::DragValue::new(&mut coupling.k).range(0.0..=1.0).speed(0.01)).changed();

                ui.label(" between ");

                // ComboBox instead of DragValue for Winding 1
                egui::ComboBox::from_id_source(format!("c1_{}", i))
                    .selected_text(format!("Winding {}", coupling.winding_1))
                    .show_ui(ui, |ui| {
                        for w in 0..num_windings {
                            changed |= ui.selectable_value(&mut coupling.winding_1, w, format!("Winding {}", w)).changed();
                        }
                    });

                ui.label(" and ");

                // ComboBox instead of DragValue for Winding 2
                egui::ComboBox::from_id_source(format!("c2_{}", i))
                    .selected_text(format!("Winding {}", coupling.winding_2))
                    .show_ui(ui, |ui| {
                        for w in 0..num_windings {
                            changed |= ui.selectable_value(&mut coupling.winding_2, w, format!("Winding {}", w)).changed();
                        }
                    });

                if ui.button("🗑").clicked() {
                    coupling_to_remove = Some(i);
                    changed = true;
                }
            });
        });
    }

    if let Some(i) = coupling_to_remove {
        def.couplings.remove(i);
    }

    changed
}