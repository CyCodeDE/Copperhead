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
use crate::ui::drawing::{Anchor, LabelEngine, rotate_vec};
use copperhead_core::components::audio_probe::AudioProbeDef;
use eframe::emath::Pos2;
use eframe::epaint::Color32;
use egui::{Align2, Painter, Sense, Stroke, StrokeKind, Ui, Vec2};

impl ComponentUIExt for AudioProbeDef {
    fn prefix(&self) -> &'static str {
        "Probe"
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        vec![(0, 0)]
    }

    fn size(&self) -> (isize, isize) {
        (1, 2)
    }

    fn offset(&self) -> (f32, f32) {
        (0., -0.75)
    }

    fn ui_name(&self) -> &'static str {
        "Audio Probe"
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        if let Some(new_path) = app.temp_audio_path.take() {
            self.file_path = new_path;
        }

        ui.label("Select output path:");

        let (rect, response) = ui.allocate_exact_size(
            Vec2::new(ui.available_width() / 2., 20.0f32),
            Sense::click(),
        );
        let is_hovered = response.hovered();
        let has_dragged_files = !ui.ctx().input(|i| i.raw.hovered_files.clone()).is_empty();
        let is_dragged_over = is_hovered && has_dragged_files;

        let visuals = ui.style().interact(&response);
        let fill_color = if is_dragged_over {
            ui.visuals().selection.bg_fill // Highlight color when dragging a file over
        } else {
            visuals.bg_fill // Normal button color
        };

        ui.painter()
            .rect(rect, 6.0, fill_color, visuals.bg_stroke, StrokeKind::Inside);
        let display_text = if !self.file_path.is_empty() {
            format!(
                "🎵 {}",
                self.file_path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
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

        if response.clicked() && app.file_dialog_state == FileDialogState::Closed {
            app.file_dialog_state = FileDialogState::SaveAudio;
            let tx = app.file_sender.clone();
            let ctx_clone = ui.ctx().clone();

            // Spawn RFD thread
            std::thread::spawn(move || {
                let file = rfd::FileDialog::new()
                    .add_filter("Audio", &["wav"])
                    .save_file();
                let _ = tx.send(file);
                ctx_clone.request_repaint();
            });
        }

        ui.ctx().input(|i| {
            if is_hovered && !i.raw.dropped_files.is_empty() {
                if let Some(dropped_file) = i.raw.dropped_files.first() {
                    if let Some(dropped_path) = &dropped_file.path {
                        // Update the source of truth directly!
                        self.file_path = dropped_path.clone();
                    }
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
        fill_color: Color32,
        stroke_color: Color32,
    ) {
        let stroke = Stroke::new(2.0, stroke_color);

        let radius = 0.4;
        let circle_center = center + rotate_vec(Vec2::new(0.0, -1.) * zoom, rotation);
        painter.circle(circle_center, radius * zoom, fill_color, stroke);

        // The pin connecting the node (center) to the probe body
        let pin_end = center + rotate_vec(Vec2::new(0.0, -0.6) * zoom, rotation);
        painter.line_segment([center, pin_end], stroke);

        painter.text(
            circle_center,
            Align2::CENTER_CENTER,
            "V",
            egui::FontId::proportional(zoom * 0.35),
            stroke_color,
        );
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        let shifted_rotation = (rotation + 1) % 4;
        let shifted_offset = rotate_vec(Vec2::new(0.0, -0.75), rotation);

        let engine = LabelEngine::new(
            painter,
            center,
            shifted_rotation,
            zoom,
            self.size(),
            (shifted_offset.x, shifted_offset.y),
        );

        let anchor = match rotation {
            0 => Anchor::Top,
            1 => Anchor::Right,
            2 => Anchor::Bottom,
            3 => Anchor::Left,
            _ => Anchor::Top,
        };

        engine.draw_stacked_labels(
            name,
            self.file_path
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
                .as_ref(),
            anchor,
        );
    }
}
