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

use crate::ui::app::{CircuitApp, FileDialogState};
use crate::ui::components::definitions::ComponentUIExt;
use crate::ui::drawing::{rotate_vec, LabelEngine};
use crate::ui::util::{format_si, format_si_single, parse_si};
use copperhead_core::components::voltage_source::{VoltageSourceDef, VoltageSourceType};
use egui::{Color32, Painter, Pos2, Sense, Stroke, StrokeKind, Ui, Vec2};

impl ComponentUIExt for VoltageSourceDef {
    fn prefix(&self) -> &'static str {
        "R"
    }

    fn ui_name(&self) -> &'static str {
        match self.source_type {
            VoltageSourceType::DC { .. } => "DC Voltage Source",
            VoltageSourceType::AC { .. } => "AC Voltage Source",
            VoltageSourceType::AudioBuffer { .. } => "Audio Source",
        }
    }

    fn size(&self) -> (isize, isize) {
        (1, 2)
    }

    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        vec![(0, -1), (0, 1)]
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        match *&mut self.source_type {
            VoltageSourceType::DC { mut voltage } => {
                ui.horizontal(|ui| {
                    ui.label("Voltage:");
                    ui.add(
                        egui::DragValue::new(&mut voltage)
                            .speed(0.1)
                            .range(-f64::INFINITY..=f64::INFINITY)
                            .suffix("V")
                            .custom_formatter(|val, _range| {
                                format_si_single(val, 3)
                            })
                            .custom_parser(|text| parse_si(text)),
                    );
                });
            }
            VoltageSourceType::AC { mut amplitude, mut frequency, mut phase } => {
                ui.horizontal(|ui| {
                    ui.label("Voltage:");
                    ui.add(
                        egui::DragValue::new(&mut amplitude)
                            .speed(0.1)
                            .range(-f64::INFINITY..=f64::INFINITY)
                            .suffix("V")
                            .custom_formatter(|val, _range| {
                                format_si_single(val, 3)
                            })
                            .custom_parser(|text| parse_si(text)),
                    );
                });
                ui.horizontal(|ui| {
                    ui.label("Frequency:");
                    ui.add(
                        egui::DragValue::new(&mut frequency)
                            .speed(1.0)
                            .range(0.0..=f64::INFINITY)
                            .suffix("Hz")
                            .custom_formatter(|val, _range| {
                                format_si_single(val, 3)
                            })
                            .custom_parser(|text| parse_si(text)),
                    );
                });
                ui.horizontal(|ui| {
                    ui.label("Phase:");
                    ui.add(
                        egui::DragValue::new(&mut phase)
                            .speed(1.0)
                            .range(0.0..=360.0)
                            .suffix("°")
                            .custom_formatter(|val, _range| {
                                format_si_single(val, 3)
                            })
                            .custom_parser(|text| parse_si(text)),
                    );
                });
            }
            VoltageSourceType::AudioBuffer { ref mut file_path } => {
                if let Some(new_path) = app.temp_audio_path.take() {
                    *file_path = new_path;
                }

                ui.label("Select input file:");

                let (rect, response) = ui.allocate_exact_size(
                    Vec2::new(ui.available_width() / 2., 20.0f32),
                    Sense::click(),
                );
                let is_hovered = response.hovered();
                let has_dragged_files =
                    !ui.ctx().input(|i| i.raw.hovered_files.clone()).is_empty();
                let is_dragged_over = is_hovered && has_dragged_files;

                let visuals = ui.style().interact(&response);
                let fill_color = if is_dragged_over {
                    ui.visuals().selection.bg_fill
                } else {
                    visuals.bg_fill
                };

                ui.painter().rect(
                    rect,
                    6.0,
                    fill_color,
                    visuals.bg_stroke,
                    StrokeKind::Inside,
                );
                let display_text = if !file_path.is_empty() {
                    format!(
                        "🎵 {}",
                        file_path.file_name().unwrap_or_default().to_string_lossy()
                    )
                } else {
                    "Select File".to_string()
                };

                ui.painter().text(
                    rect.center(),
                    egui::Align2::CENTER_CENTER,
                    display_text,
                    egui::FontId::proportional(14.0),
                    visuals.text_color(),
                );

                if response.clicked()
                    && app.file_dialog_state == FileDialogState::Closed
                {
                    app.file_dialog_state = FileDialogState::LoadAudio;
                    let tx = app.file_sender.clone();
                    let ctx_clone = ui.ctx().clone();

                    // Spawn RFD thread
                    std::thread::spawn(move || {
                        let file = rfd::FileDialog::new()
                            .add_filter("Audio", &["wav", "mp3", "flac"])
                            .pick_file();
                        let _ = tx.send(file);
                        ctx_clone.request_repaint();
                    });
                }

                ui.ctx().input(|i| {
                    if is_hovered && !i.raw.dropped_files.is_empty() {
                        if let Some(dropped_file) = i.raw.dropped_files.first() {
                            if let Some(dropped_path) = &dropped_file.path {
                                *file_path = dropped_path.clone();
                            }
                        }
                    }
                });
            }
        }

        false
    }

    fn draw_icon(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, fill_color: Color32, stroke_color: Color32) {
        match self.source_type {
            VoltageSourceType::DC { .. } => draw_dc_source(painter, center, rotation, zoom, fill_color, stroke_color),
            VoltageSourceType::AC { .. } => draw_ac_source(painter, center, rotation, zoom, fill_color, stroke_color),
            VoltageSourceType::AudioBuffer { .. } => draw_audio_source(painter, center, rotation, zoom, fill_color, stroke_color),
        }
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let shifted_rotation = (rotation + 1) % 4;
        let shifted_size = (self.size().1, self.size().0);

        // We lie to the engine about the rotation just for the layout logic
        let engine = LabelEngine::new(painter, center, shifted_rotation, zoom, shifted_size, self.offset());

        let value = match &self.source_type {
            VoltageSourceType::DC { voltage } => format_si(&[(*voltage, "V")], 0.1, 2),
            VoltageSourceType::AC { frequency, amplitude, phase } => format_si(&[
                (*amplitude, "V"),
                (*frequency, "Hz"),
                (*phase, "°"),
            ], 0.1, 2),
            VoltageSourceType::AudioBuffer { file_path } => {
                if let Some(file_name) = file_path.file_name() {
                    if let Some(file_str) = file_name.to_str() {
                        file_str.to_string()
                    } else {
                        String::new()
                    }
                } else {
                    String::new()
                }
            },
        };


        engine.draw_axial_labels(name, &value);
    }


}

fn draw_dc_source(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let radius = 0.4;
    let stroke = Stroke::new(2.0, stroke_color);

    painter.circle(center, radius * zoom, fill_color, stroke);

    let top_pin = rotate_vec(Vec2::new(0.0, -1.0) * zoom, rotation);
    let top_circle = rotate_vec(Vec2::new(0.0, -radius) * zoom, rotation);
    painter.line_segment([center + top_pin, center + top_circle], stroke);

    let bot_pin = rotate_vec(Vec2::new(0.0, 1.0) * zoom, rotation);
    let bot_circle = rotate_vec(Vec2::new(0.0, radius) * zoom, rotation);
    painter.line_segment([center + bot_pin, center + bot_circle], stroke);

    let pos_plus = center + rotate_vec(Vec2::new(0.0, -0.15) * zoom, rotation);
    painter.text(
        pos_plus,
        egui::Align2::CENTER_CENTER,
        "+",
        egui::FontId::proportional(zoom * 0.4),
        stroke_color,
    );

    let pos_minus = center + rotate_vec(Vec2::new(0.0, 0.15) * zoom, rotation);
    painter.text(
        pos_minus,
        egui::Align2::CENTER_CENTER,
        "-",
        egui::FontId::proportional(zoom * 0.4),
        stroke_color,
    );
}

fn draw_ac_source(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let radius = 0.4;
    let stroke = Stroke::new(2.0, stroke_color);

    painter.circle(center, radius * zoom, fill_color, stroke);

    let top_pin = rotate_vec(Vec2::new(0.0, -1.0) * zoom, rotation);
    let top_circle = rotate_vec(Vec2::new(0.0, -radius) * zoom, rotation);
    painter.line_segment([center + top_pin, center + top_circle], stroke);

    let bot_pin = rotate_vec(Vec2::new(0.0, 1.0) * zoom, rotation);
    let bot_circle = rotate_vec(Vec2::new(0.0, radius) * zoom, rotation);
    painter.line_segment([center + bot_pin, center + bot_circle], stroke);

    let wave_width = 0.25;
    let wave_amp = 0.15;

    let start = center + rotate_vec(Vec2::new(-wave_width, 0.0) * zoom, rotation);
    let end = center + rotate_vec(Vec2::new(wave_width, 0.0) * zoom, rotation);

    let c1 = center
        + rotate_vec(
        Vec2::new(-wave_width / 2.0, -wave_amp * 2.0) * zoom,
        rotation,
    );
    let c2 = center + rotate_vec(Vec2::new(wave_width / 2.0, wave_amp * 2.0) * zoom, rotation);

    let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
        [start, c1, c2, end],
        false,
        Color32::TRANSPARENT,
        stroke,
    );
    painter.add(bezier);
}

fn draw_audio_source(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let radius = 0.4;
    let stroke = Stroke::new(2.0, stroke_color);

    // Draw the source body
    painter.circle(center, radius * zoom, fill_color, stroke);

    // Draw the top pin
    let top_pin = rotate_vec(Vec2::new(0.0, -1.0) * zoom, rotation);
    let top_circle = rotate_vec(Vec2::new(0.0, -radius) * zoom, rotation);
    painter.line_segment([center + top_pin, center + top_circle], stroke);

    // Draw the bottom pin
    let bot_pin = rotate_vec(Vec2::new(0.0, 1.0) * zoom, rotation);
    let bot_circle = rotate_vec(Vec2::new(0.0, radius) * zoom, rotation);
    painter.line_segment([center + bot_pin, center + bot_circle], stroke);

    // Draw an irregular, jagged waveform to represent arbitrary audio samples
    let wave_points = vec![
        Vec2::new(-0.25, 0.0),
        Vec2::new(-0.15, -0.15),
        Vec2::new(-0.05, 0.20),
        Vec2::new(0.05, -0.20),
        Vec2::new(0.15, 0.10),
        Vec2::new(0.25, 0.0),
    ];

    // Transform points based on position, zoom, and rotation
    let transformed_points: Vec<Pos2> = wave_points
        .into_iter()
        .map(|p| center + rotate_vec(p * zoom, rotation))
        .collect();

    // Draw the continuous jagged line
    painter.add(egui::epaint::PathShape::line(transformed_points, stroke));
}