/*
 * Copyright (c) 2026-2026 CyCode and the Copperhead contributors
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

use crate::ui::components::definitions::ComponentUIExt;
use crate::ui::{GridPos, VisualComponent};
use eframe::emath::{Pos2, Rect, Vec2};
use eframe::epaint::Color32;
use egui::{Align2, Painter};
// DISCLAIMER:
// Most of this file is vibe-coded. Performance is probably not optimal, but tbh I couldn't give less of a fuck.
// Is the performance good? No. Probably not. But that is a problem for the me/myself/and I of tomorrow.

pub fn draw_grid(painter: &Painter, rect: Rect, zoom: f32, pan: Vec2, color: Color32) {
    let min_visible = (rect.min.to_vec2() - pan) / zoom;
    let max_visible = (rect.max.to_vec2() - pan) / zoom;
    let (min_col, max_col) = (min_visible.x.floor() as i32, max_visible.x.ceil() as i32);
    let (min_row, max_row) = (min_visible.y.floor() as i32, max_visible.y.ceil() as i32);

    let draw_minor = zoom > 10.0;
    let dot_radius = 1.25;

    for x in min_col..=max_col {
        if !draw_minor && x % 10 != 0 {
            continue;
        }

        let sx = x as f32 * zoom + pan.x;

        for y in min_row..=max_row {
            if !draw_minor && y % 10 != 0 {
                continue;
            }

            let sy = y as f32 * zoom + pan.y;

            painter.circle_filled(Pos2::new(sx, sy), dot_radius, color);
        }
    }
}

/// A generic function to draw a component.
pub fn draw_component<F>(
    painter: &Painter,
    comp: &VisualComponent,
    transform: F,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) where
    F: Fn(GridPos) -> Pos2,
{
    let center = transform(comp.pos);
    let rotation = comp.rotation % 4;

    // Draw the specific symbol
    comp.element
        .draw_icon(painter, center, rotation, zoom, fill_color, stroke_color);

    // Draw Pins (Red Dots) on top of everything
    let pin_color = Color32::RED;
    for pin_grid_pos in comp.get_pin_locations() {
        let pin_screen_pos = transform(pin_grid_pos);
        painter.circle_filled(pin_screen_pos, zoom * 0.075, pin_color);
    }
}

/// Rotates a vector based on grid rotation (0, 1, 2, 3)
pub fn rotate_vec(vec: Vec2, rotation: u8) -> Vec2 {
    match rotation {
        0 => vec,                       // 0 deg
        1 => Vec2::new(-vec.y, vec.x),  // 90 deg CW
        2 => Vec2::new(-vec.x, -vec.y), // 180 deg
        3 => Vec2::new(vec.y, -vec.x),  // 270 deg CW
        _ => vec,
    }
}

// Helper to check if a line segment (p1, p2) intersects a Rectangle
pub fn check_line_rect_intersection(p1: Pos2, p2: Pos2, rect: Rect) -> bool {
    if rect.contains(p1) || rect.contains(p2) {
        return true;
    }

    let line_bounds = Rect::from_two_pos(p1, p2);
    if !rect.intersects(line_bounds) {
        return false;
    }

    let corners = [
        rect.min,
        Pos2::new(rect.max.x, rect.min.y),
        rect.max,
        Pos2::new(rect.min.x, rect.max.y),
    ];

    for i in 0..4 {
        let c1 = corners[i];
        let c2 = corners[(i + 1) % 4];
        if lines_intersect(p1, p2, c1, c2) {
            return true;
        }
    }

    false
}

// Standard line-line intersection
fn lines_intersect(a1: Pos2, a2: Pos2, b1: Pos2, b2: Pos2) -> bool {
    let den = (b2.y - b1.y) * (a2.x - a1.x) - (b2.x - b1.x) * (a2.y - a1.y);
    if den == 0.0 {
        return false;
    } // Parallel

    let ua = ((b2.x - b1.x) * (a1.y - b1.y) - (b2.y - b1.y) * (a1.x - b1.x)) / den;
    let ub = ((a2.x - a1.x) * (a1.y - b1.y) - (a2.y - a1.y) * (a1.x - b1.x)) / den;

    ua >= 0.0 && ua <= 1.0 && ub >= 0.0 && ub <= 1.0
}

#[derive(Clone, Copy)]
pub enum Anchor {
    Top,
    Bottom,
    Left,
    Right,
    TopRight,
    BottomRight,
    // Add others if needed (TopLeft, BottomLeft)
}

impl Anchor {
    /// Returns the text alignment required so text grows AWAY from the anchor point.
    fn text_align(&self) -> Align2 {
        match self {
            Anchor::Top => Align2::CENTER_BOTTOM,
            Anchor::Bottom => Align2::CENTER_TOP,
            Anchor::Left => Align2::RIGHT_CENTER,
            Anchor::Right => Align2::LEFT_CENTER,
            Anchor::TopRight => Align2::LEFT_TOP,
            Anchor::BottomRight => Align2::LEFT_BOTTOM,
        }
    }

    /// Returns a normalized vector pointing outwards from the bounding box.
    fn push_dir(&self) -> Vec2 {
        match self {
            Anchor::Top => Vec2::new(0.0, -1.0),
            Anchor::Bottom => Vec2::new(0.0, 1.0),
            Anchor::Left => Vec2::new(-1.0, 0.0),
            Anchor::Right => Vec2::new(1.0, 0.0),
            Anchor::TopRight => Vec2::new(1.0, -1.0),
            Anchor::BottomRight => Vec2::new(1.0, 1.0),
        }
    }
}

pub struct LabelEngine<'a> {
    painter: &'a Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    bounds: Rect,
    padding: f32, // Replaces all the arbitrary 0.2, 0.4 offsets
}

impl<'a> LabelEngine<'a> {
    pub fn new(
        painter: &'a Painter,
        center: Pos2,
        rotation: u8,
        zoom: f32,
        size: (isize, isize),
        offset: (f32, f32),
    ) -> Self {
        // Calculate dimensions in screen space
        let width = size.0 as f32 * zoom;
        let height = size.1 as f32 * zoom;

        // Handle bounding box rotation swapping
        let (w, h) = if rotation % 2 != 0 {
            (height, width)
        } else {
            (width, height)
        };

        // Offset the center based on the component's internal offset, scaled by zoom
        let adjusted_center = center + Vec2::new(offset.0, offset.1) * zoom;

        let bounds = Rect::from_center_size(adjusted_center, Vec2::new(w, h));

        Self {
            painter,
            center,
            rotation,
            zoom,
            bounds,
            padding: 0.3 * zoom, // Standardized spacing
        }
    }

    /// Base method to draw text relative to the bounding box
    pub fn draw_text_at_anchor(&self, text: &str, anchor: Anchor, is_strong: bool, color: Color32) {
        if text.is_empty() {
            return;
        }

        // Find the absolute coordinate on the bounding box
        let anchor_pos = match anchor {
            Anchor::Top => self.bounds.center_top(),
            Anchor::Bottom => self.bounds.center_bottom(),
            Anchor::Left => self.bounds.left_center(),
            Anchor::Right => self.bounds.right_center(),
            Anchor::TopRight => self.bounds.right_top(),
            Anchor::BottomRight => self.bounds.right_bottom(),
        };

        // Push the text away from the box by the padding amount
        let final_pos = anchor_pos + (anchor.push_dir() * self.padding);

        let font_id = egui::FontId::monospace(self.zoom * 0.4);

        // You can build a LayoutJob here or use painter.text
        self.painter
            .text(final_pos, anchor.text_align(), text, font_id, color);
    }

    pub fn draw_stacked_labels(&self, name: &str, value: &str, anchor: Anchor) {
        // Find the center of the corresponding edge
        let edge_center = match anchor {
            Anchor::Top => self.bounds.center_top(),
            Anchor::Bottom => self.bounds.center_bottom(),
            Anchor::Left => self.bounds.left_center(),
            Anchor::Right => self.bounds.right_center(),
            _ => return, // TopRight/BottomRight are ignored for grouped stacking
        };

        // Push outward from the bounding box by the standard padding
        let base_pos = edge_center + (anchor.push_dir() * self.padding);

        let font_size = self.zoom * 0.4;
        let spacing = self.zoom * 0.1;

        // Determine how to stack and align the text relative to the base position
        let (align_name, align_value, offset_name, offset_value) = match anchor {
            Anchor::Right => (
                Align2::LEFT_BOTTOM,
                Align2::LEFT_TOP,
                Vec2::new(0.0, -spacing / 2.0),
                Vec2::new(0.0, spacing / 2.0),
            ),
            Anchor::Left => (
                Align2::RIGHT_BOTTOM,
                Align2::RIGHT_TOP,
                Vec2::new(0.0, -spacing / 2.0),
                Vec2::new(0.0, spacing / 2.0),
            ),
            Anchor::Top => (
                Align2::CENTER_BOTTOM,
                Align2::CENTER_BOTTOM,
                Vec2::new(0.0, -font_size - spacing), // Name sits above Value
                Vec2::new(0.0, 0.0),                  // Value sits right on the line
            ),
            Anchor::Bottom => (
                Align2::CENTER_TOP,
                Align2::CENTER_TOP,
                Vec2::new(0.0, 0.0), // Name sits right on the line
                Vec2::new(0.0, font_size + spacing), // Value sits below Name
            ),
            _ => return,
        };

        if !name.is_empty() {
            self.painter.text(
                base_pos + offset_name,
                align_name,
                name,
                egui::FontId::monospace(font_size),
                Color32::WHITE,
            );
        }

        if !value.is_empty() {
            self.painter.text(
                base_pos + offset_value,
                align_value,
                value,
                egui::FontId::monospace(font_size),
                Color32::LIGHT_GRAY,
            );
        }
    }

    /// Abstracted layout for Resistors, Capacitors, Inductors, Diodes
    pub fn draw_axial_labels(&self, name: &str, value: &str) {
        if self.rotation % 2 == 0 {
            // Horizontal: Name centered above, Value centered below
            self.draw_text_at_anchor(name, Anchor::Top, true, Color32::WHITE);
            self.draw_text_at_anchor(value, Anchor::Bottom, false, Color32::LIGHT_GRAY);
        } else {
            // Vertical: Both on the right, centered vertically together.
            // Get the center-right point of the bounding box and apply padding
            let right_center =
                self.bounds.right_center() + (Anchor::Right.push_dir() * self.padding);

            let font_size = self.zoom * 0.4;
            let spacing = self.zoom * 0.5;

            // Anchor Name just above the center line
            self.painter.text(
                right_center - Vec2::new(0.0, spacing / 2.0),
                Align2::LEFT_BOTTOM,
                name,
                egui::FontId::monospace(font_size),
                Color32::WHITE,
            );

            // Anchor Value just below the center line
            self.painter.text(
                right_center + Vec2::new(0.0, spacing / 2.0),
                Align2::LEFT_TOP,
                value,
                egui::FontId::monospace(font_size),
                Color32::LIGHT_GRAY,
            );
        }
    }

    /// Helper for rendering transistor/tube pin indicators (C, B, E, etc.)
    pub fn draw_pin_marker(&self, local_pin_pos: Vec2, label: &str) {
        // Rotate the local pin position according to the component's rotation
        let rotated_pin = self.rotate_vec(local_pin_pos) * self.zoom;
        let pin_world_pos = self.center + rotated_pin;

        // Calculate a small push direction away from the center of the component
        let push_dir = rotated_pin.normalized();
        let text_pos = pin_world_pos + (push_dir);

        self.painter.text(
            text_pos,
            Align2::CENTER_CENTER, // Pin markers are usually small enough to just center
            label,
            egui::FontId::monospace(self.zoom * 0.3),
            Color32::WHITE,
        );
    }

    // Helper to rotate local vectors (0=0deg, 1=90deg, 2=180deg, 3=270deg)
    fn rotate_vec(&self, v: Vec2) -> Vec2 {
        match self.rotation % 4 {
            0 => Vec2::new(v.x, v.y),
            1 => Vec2::new(-v.y, v.x),
            2 => Vec2::new(-v.x, -v.y),
            3 => Vec2::new(v.y, -v.x),
            _ => v,
        }
    }
}
