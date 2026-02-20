use crate::components::transistor::bjt::BjtModel;
use crate::model::GridPos;
use crate::ui::{ComponentBuildData, VisualComponent};
use crate::util::format_si;
use eframe::emath::{Align, Pos2, Rect, Vec2};
use eframe::epaint::text::LayoutJob;
use eframe::epaint::{Color32, Shape, Stroke, StrokeKind};
use egui::{Align2, FontSelection, Painter, RichText, Style};

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
    match comp.component {
        ComponentBuildData::Resistor { .. } => {
            draw_resistor(painter, center, rotation, zoom, fill_color, stroke_color);
        }
        ComponentBuildData::Capacitor { .. } => {
            draw_capacitor(painter, center, rotation, zoom, stroke_color);
        }
        ComponentBuildData::DCSource { .. } => {
            draw_dc_source(painter, center, rotation, zoom, fill_color, stroke_color);
        }
        ComponentBuildData::ASource { .. } => {
            draw_ac_source(painter, center, rotation, zoom, fill_color, stroke_color);
        }
        ComponentBuildData::Ground => {
            draw_ground(painter, center, rotation, zoom, stroke_color);
        }
        ComponentBuildData::Inductor { .. } => {
            draw_inductor(painter, center, rotation, zoom, stroke_color);
        }
        ComponentBuildData::Diode { .. } => {
            draw_diode(painter, center, rotation, zoom, fill_color, stroke_color);
        }
        ComponentBuildData::Label => {
            draw_label(painter, center, rotation, zoom, fill_color, stroke_color);
        }
        ComponentBuildData::Bjt { model } => match model.polarity() {
            true => draw_bjt_npn(painter, center, rotation, zoom, fill_color, stroke_color),
            false => draw_bjt_pnp(painter, center, rotation, zoom, fill_color, stroke_color),
        },
        // Fallback for unimplemented components
        _ => {
            draw_generic_box(painter, center, rotation, zoom, fill_color, stroke_color);
        }
    }

    // Draw Pins (Red Dots) on top of everything
    let pin_color = Color32::RED;
    for pin_grid_pos in comp.get_pin_locations() {
        let pin_screen_pos = transform(pin_grid_pos);
        painter.circle_filled(pin_screen_pos, zoom * 0.075, pin_color);
    }
}

/// Rotates a vector based on grid rotation (0, 1, 2, 3)
fn rotate_vec(vec: Vec2, rotation: u8) -> Vec2 {
    match rotation {
        0 => vec,                       // 0 deg
        1 => Vec2::new(-vec.y, vec.x),  // 90 deg CW
        2 => Vec2::new(-vec.x, -vec.y), // 180 deg
        3 => Vec2::new(vec.y, -vec.x),  // 270 deg CW
        _ => vec,
    }
}

fn draw_label(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
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

fn draw_resistor(
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
}

fn draw_capacitor(painter: &Painter, center: Pos2, rotation: u8, zoom: f32, stroke_color: Color32) {
    let plate_gap = 0.15; // Distance from center to plate
    let plate_height = 0.8; // Total height of the plate
    let stroke = Stroke::new(2.0, stroke_color);

    let wire_left_start = rotate_vec(Vec2::new(-1.0, 0.0) * zoom, rotation);
    let wire_left_end = rotate_vec(Vec2::new(-plate_gap, 0.0) * zoom, rotation);
    painter.line_segment([center + wire_left_start, center + wire_left_end], stroke);

    let wire_right_start = rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);
    let wire_right_end = rotate_vec(Vec2::new(plate_gap, 0.0) * zoom, rotation);
    painter.line_segment([center + wire_right_start, center + wire_right_end], stroke);

    let p1_top = rotate_vec(Vec2::new(-plate_gap, -plate_height / 2.0) * zoom, rotation);
    let p1_bot = rotate_vec(Vec2::new(-plate_gap, plate_height / 2.0) * zoom, rotation);
    painter.line_segment([center + p1_top, center + p1_bot], stroke);

    let p2_top = rotate_vec(Vec2::new(plate_gap, -plate_height / 2.0) * zoom, rotation);
    let p2_bot = rotate_vec(Vec2::new(plate_gap, plate_height / 2.0) * zoom, rotation);
    painter.line_segment([center + p2_top, center + p2_bot], stroke);
}

fn draw_inductor(painter: &Painter, center: Pos2, rotation: u8, zoom: f32, stroke_color: Color32) {
    let stroke = Stroke::new(2.0, stroke_color);

    let num_coils = 4;
    let lead_length = 0.2; // Straight wire segment at ends
    let total_width = 2.0;

    let coil_section_width = total_width - (2.0 * lead_length);
    let loop_width = coil_section_width / num_coils as f32;
    let loop_radius = loop_width / 2.0;

    let k = 1.33333;
    let ctrl_height = loop_radius * k;

    let start_x = -1.0;
    let coil_start_x = -1.0 + lead_length;

    let p_lead_start = center + rotate_vec(Vec2::new(start_x, 0.0) * zoom, rotation);
    let p_coil_start = center + rotate_vec(Vec2::new(coil_start_x, 0.0) * zoom, rotation);

    painter.line_segment([p_lead_start, p_coil_start], stroke);

    let mut current_x = coil_start_x;

    for _ in 0..num_coils {
        let next_x = current_x + loop_width;

        let p1_local = Vec2::new(current_x, 0.0);
        let p2_local = Vec2::new(next_x, 0.0);

        let c1_local = Vec2::new(current_x, -ctrl_height);
        let c2_local = Vec2::new(next_x, -ctrl_height);

        let p1 = center + rotate_vec(p1_local * zoom, rotation);
        let p2 = center + rotate_vec(p2_local * zoom, rotation);
        let c1 = center + rotate_vec(c1_local * zoom, rotation);
        let c2 = center + rotate_vec(c2_local * zoom, rotation);

        let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
            [p1, c1, c2, p2],
            false,
            Color32::TRANSPARENT,
            stroke,
        );
        painter.add(bezier);

        current_x = next_x;
    }

    let p_coil_end = center + rotate_vec(Vec2::new(current_x, 0.0) * zoom, rotation);
    let p_lead_end = center + rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);

    painter.line_segment([p_coil_end, p_lead_end], stroke);
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

fn draw_ground(painter: &Painter, center: Pos2, rotation: u8, zoom: f32, stroke_color: Color32) {
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

fn draw_generic_box(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let (base_w, base_h) = (2.0, 1.0);

    let (w, h) = if rotation % 2 == 1 {
        (base_h, base_w)
    } else {
        (base_w, base_h)
    };

    let size = Vec2::new(w * zoom, h * zoom);
    let rect = Rect::from_center_size(center, size);

    painter.rect_filled(rect, 4.0, fill_color);
    painter.rect_stroke(
        rect,
        4.0,
        Stroke::new(1.5, stroke_color),
        StrokeKind::Outside,
    );

    painter.text(
        center,
        egui::Align2::CENTER_CENTER,
        "?",
        egui::FontId::proportional(zoom * 0.5),
        Color32::WHITE,
    );
}

fn draw_diode(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let stroke = Stroke::new(2.0, stroke_color);

    let half_len = 0.5;
    let half_h = 0.5;

    let lead_anode_start = rotate_vec(Vec2::new(-1.0, 0.0) * zoom, rotation);
    let lead_anode_end = rotate_vec(Vec2::new(-half_len, 0.0) * zoom, rotation);
    painter.line_segment([center + lead_anode_start, center + lead_anode_end], stroke);

    let lead_cathode_start = rotate_vec(Vec2::new(half_len, 0.0) * zoom, rotation);
    let lead_cathode_end = rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);
    painter.line_segment(
        [center + lead_cathode_start, center + lead_cathode_end],
        stroke,
    );

    let triangle_points_local = [
        Vec2::new(-half_len, -half_h),
        Vec2::new(-half_len, half_h),
        Vec2::new(half_len, 0.0),
    ];

    let triangle_points_screen: Vec<Pos2> = triangle_points_local
        .iter()
        .map(|&p| center + rotate_vec(p * zoom, rotation))
        .collect();

    painter.add(Shape::convex_polygon(
        triangle_points_screen,
        fill_color,
        stroke,
    ));

    let bar_top = center + rotate_vec(Vec2::new(half_len, -half_h) * zoom, rotation);
    let bar_bot = center + rotate_vec(Vec2::new(half_len, half_h) * zoom, rotation);

    painter.line_segment([bar_top, bar_bot], stroke);
}

fn draw_bjt_npn(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let stroke = Stroke::new(2.0, stroke_color);

    // 1. Draw the Circle Body
    // Center offset to shift the circle slightly right
    let circle_center_offset = Vec2::new(0.3, 0.0);
    let radius = 0.7;

    painter.circle(
        center + rotate_vec(circle_center_offset * zoom, rotation),
        radius * zoom,
        fill_color, // Use the fill color for the circle body
        stroke,
    );

    // 2. Draw Base (Left Side)
    // Base Pin: (-1.0, 0.0) -> Connects to Base Bar at (-0.2, 0.0)
    let base_pin = Vec2::new(-1.0, 0.0);
    let base_bar_x = -0.2;
    let base_connect = Vec2::new(base_bar_x, 0.0);

    // Wire from Pin to Bar
    painter.line_segment(
        [
            center + rotate_vec(base_pin * zoom, rotation),
            center + rotate_vec(base_connect * zoom, rotation),
        ],
        stroke,
    );

    // Base Bar (Vertical Line)
    let bar_height = 0.5;
    let bar_top = Vec2::new(base_bar_x, -bar_height);
    let bar_bot = Vec2::new(base_bar_x, bar_height);

    painter.line_segment(
        [
            center + rotate_vec(bar_top * zoom, rotation),
            center + rotate_vec(bar_bot * zoom, rotation),
        ],
        Stroke::new(2.5, stroke_color), // Make the base bar slightly thicker
    );

    // 3. Draw Collector (Top Right)
    // Pin: (1.0, -1.0)
    let col_pin = Vec2::new(1.0, -1.0);
    let col_corner = Vec2::new(1.0, -0.5); // The "Knee"
    let col_base_contact = Vec2::new(base_bar_x, -0.25); // Where it touches the base

    // Wire: Pin -> Corner
    painter.line_segment(
        [
            center + rotate_vec(col_pin * zoom, rotation),
            center + rotate_vec(col_corner * zoom, rotation),
        ],
        stroke,
    );
    // Wire: Corner -> Base
    painter.line_segment(
        [
            center + rotate_vec(col_corner * zoom, rotation),
            center + rotate_vec(col_base_contact * zoom, rotation),
        ],
        stroke,
    );

    // 4. Draw Emitter (Bottom Right)
    // Pin: (1.0, 1.0)
    let emit_pin = Vec2::new(1.0, 1.0);
    let emit_corner = Vec2::new(1.0, 0.5);
    let emit_base_contact = Vec2::new(base_bar_x, 0.25);

    // Wire: Pin -> Corner
    painter.line_segment(
        [
            center + rotate_vec(emit_pin * zoom, rotation),
            center + rotate_vec(emit_corner * zoom, rotation),
        ],
        stroke,
    );
    // Wire: Corner -> Base
    painter.line_segment(
        [
            center + rotate_vec(emit_corner * zoom, rotation),
            center + rotate_vec(emit_base_contact * zoom, rotation),
        ],
        stroke,
    );

    // 5. Draw Emitter Arrow (NPN Style - Pointing Out)
    // We calculate a position along the vector (Base -> Corner)
    let arrow_pos_t = 0.5; // Position 0.0 to 1.0 along the segment

    // Manual Lerp: A + (B - A) * t
    let arrow_center_local = emit_base_contact + (emit_corner - emit_base_contact) * arrow_pos_t;

    // Direction vector of the emitter leg (normalized)
    let dir = (emit_corner - emit_base_contact).normalized();
    // Perpendicular vector for arrow width
    let perp = Vec2::new(-dir.y, dir.x);

    let arrow_size = 0.15;
    let arrow_width = 0.1;

    // Triangle points relative to local center
    let tip = arrow_center_local + dir * arrow_size;
    let base_l = arrow_center_local - dir * arrow_size + perp * arrow_width;
    let base_r = arrow_center_local - dir * arrow_size - perp * arrow_width;

    let arrow_points_local = [tip, base_l, base_r];

    let rotated_arrow: Vec<Pos2> = arrow_points_local
        .iter()
        .map(|&p| center + rotate_vec(p * zoom, rotation))
        .collect();

    // Use fill_color or stroke_color based on preference (filled arrows are common)
    painter.add(Shape::convex_polygon(
        rotated_arrow,
        stroke_color,
        Stroke::NONE,
    ));
}

fn draw_bjt_pnp(
    painter: &Painter,
    center: Pos2,
    rotation: u8,
    zoom: f32,
    fill_color: Color32,
    stroke_color: Color32,
) {
    let stroke = Stroke::new(2.0, stroke_color);

    let circle_center_offset = Vec2::new(0.3, 0.0);
    let radius = 0.7;

    painter.circle(
        center + rotate_vec(circle_center_offset * zoom, rotation),
        radius * zoom,
        fill_color,
        stroke,
    );

    let base_pin = Vec2::new(-1.0, 0.0);
    let base_bar_x = -0.2;
    let base_connect = Vec2::new(base_bar_x, 0.0);

    // Wire from Pin to Bar
    painter.line_segment(
        [
            center + rotate_vec(base_pin * zoom, rotation),
            center + rotate_vec(base_connect * zoom, rotation),
        ],
        stroke,
    );

    // Base Bar (Vertical Line)
    let bar_height = 0.5;
    let bar_top = Vec2::new(base_bar_x, -bar_height);
    let bar_bot = Vec2::new(base_bar_x, bar_height);

    painter.line_segment(
        [
            center + rotate_vec(bar_top * zoom, rotation),
            center + rotate_vec(bar_bot * zoom, rotation),
        ],
        Stroke::new(2.5, stroke_color),
    );

    let emit_pin = Vec2::new(1.0, -1.0);
    let emit_corner = Vec2::new(1.0, -0.5);
    let emit_base_contact = Vec2::new(base_bar_x, -0.25);

    painter.line_segment(
        [
            center + rotate_vec(emit_pin * zoom, rotation),
            center + rotate_vec(emit_corner * zoom, rotation),
        ],
        stroke,
    );

    painter.line_segment(
        [
            center + rotate_vec(emit_corner * zoom, rotation),
            center + rotate_vec(emit_base_contact * zoom, rotation),
        ],
        stroke,
    );

    let col_pin = Vec2::new(1.0, 1.0);
    let col_corner = Vec2::new(1.0, 0.5);
    let col_base_contact = Vec2::new(base_bar_x, 0.25);

    painter.line_segment(
        [
            center + rotate_vec(col_pin * zoom, rotation),
            center + rotate_vec(col_corner * zoom, rotation),
        ],
        stroke,
    );
    painter.line_segment(
        [
            center + rotate_vec(col_corner * zoom, rotation),
            center + rotate_vec(col_base_contact * zoom, rotation),
        ],
        stroke,
    );

    let arrow_pos_t = 0.5;

    let arrow_center_local = emit_base_contact + (emit_corner - emit_base_contact) * arrow_pos_t;

    let dir = (emit_base_contact - emit_corner).normalized();

    let perp = Vec2::new(-dir.y, dir.x);

    let arrow_size = 0.15;
    let arrow_width = 0.1;

    let tip = arrow_center_local + dir * arrow_size;
    let base_l = arrow_center_local - dir * arrow_size + perp * arrow_width;
    let base_r = arrow_center_local - dir * arrow_size - perp * arrow_width;

    let arrow_points_local = [tip, base_l, base_r];

    let rotated_arrow: Vec<Pos2> = arrow_points_local
        .iter()
        .map(|&p| center + rotate_vec(p * zoom, rotation))
        .collect();

    painter.add(Shape::convex_polygon(
        rotated_arrow,
        stroke_color,
        Stroke::NONE,
    ));
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

pub fn draw_component_labels(
    component: &VisualComponent,
    painter: &Painter,
    transform: impl Fn(GridPos) -> Pos2,
    zoom: f32,
) {
    if component.component == ComponentBuildData::Ground {
        return;
    }

    let center = transform(component.pos);
    let rotation = component.rotation % 4;

    let label = &component.name;

    if label.is_empty() {
        return; // No label to draw
    }

    let (mut valign_label, mut valign_value) = (Align::Center, Align::Center);
    let (mut halign_label, mut halign_value) = (Align::Center, Align::Center);

    // Determine label position based on component type and rotation
    let (offset_label, offset_value) = match &component.component {
        ComponentBuildData::Resistor { .. }
        | ComponentBuildData::Inductor { .. }
        | ComponentBuildData::Capacitor { .. }
        | ComponentBuildData::Diode { .. } => {
            // Place label above for horizontal, right for vertical
            let label_pos = match rotation {
                0 | 2 => Vec2::new(0.0, -1.07 * zoom),        // Above
                1 | 3 => Vec2::new(0.7 * zoom, -0.65 * zoom), // Right top
                _ => Vec2::ZERO,
            };

            let value_pos = match rotation {
                0 | 2 => Vec2::new(0.0, 0.62 * zoom),       // Below
                1 | 3 => Vec2::new(0.7 * zoom, 0.2 * zoom), // Right Bottom
                _ => Vec2::ZERO,
            };

            (valign_label, halign_label) = match rotation {
                0 | 2 => (Align::BOTTOM, Align::Center),
                1 | 3 => (Align::TOP, Align::TOP),
                _ => (Align::BOTTOM, Align::Center),
            };

            (valign_value, halign_value) = match rotation {
                0 | 2 => (Align::TOP, Align::Center),
                1 | 3 => (Align::BOTTOM, Align::LEFT),
                _ => (Align::TOP, Align::Center),
            };

            (label_pos, value_pos)
        }
        ComponentBuildData::Label => {
            // Place label above for horizontal, right for vertical
            let label_pos = match rotation {
                0 | 2 => Vec2::new(0.0, -1.1 * zoom),         // Above
                1 | 3 => Vec2::new(0.7 * zoom, -0.35 * zoom), // Right top
                _ => Vec2::ZERO,
            };
            (label_pos, Vec2::ZERO)
        }
        ComponentBuildData::DCSource { .. } | ComponentBuildData::ASource { .. } => {
            // Place label below for vertical, right for horizontal
            let label_pos = match rotation {
                0 | 2 => Vec2::new(0.7 * zoom, -0.62 * zoom), // Below
                1 | 3 => Vec2::new(0.0, -1.1 * zoom),         // Right
                _ => Vec2::ZERO,
            };

            let value_pos = match rotation {
                0 | 2 => Vec2::new(0.7 * zoom, 0.2 * zoom), // Above
                1 | 3 => Vec2::new(0.0, 0.7 * zoom),        // Left
                _ => Vec2::ZERO,
            };

            // here the rotation is swapped compared to above
            (valign_label, halign_label) = match rotation {
                0 | 2 => (Align::TOP, Align::TOP),
                1 | 3 => (Align::BOTTOM, Align::Center),
                _ => (Align::TOP, Align::TOP),
            };

            (valign_value, halign_value) = match rotation {
                0 | 2 => (Align::BOTTOM, Align::LEFT),
                1 | 3 => (Align::TOP, Align::Center),
                _ => (Align::BOTTOM, Align::LEFT),
            };

            (label_pos, value_pos)
        }
        ComponentBuildData::Bjt { .. } => {
            let offset_dist = 1.5 * zoom;

            let label_pos = match rotation {
                0 => Vec2::new(offset_dist, -0.62 * zoom), // Right, slightly up
                1 => Vec2::new(1. * zoom, -0.45 * zoom),   // Down, slightly right
                2 => Vec2::new(-offset_dist, -0.62 * zoom), // Left, slightly up
                3 => Vec2::new(1. * zoom, -0.1 * zoom),    // Up, slightly right
                _ => Vec2::ZERO,
            };

            let value_pos = match rotation {
                0 => Vec2::new(offset_dist, 0.2 * zoom), // Right, slightly down
                1 => Vec2::new(-0.0 * zoom, 1.2 * zoom), // Down, slightly left
                2 => Vec2::new(-offset_dist, 0.2 * zoom), // Left, slightly down
                3 => Vec2::new(-0.0 * zoom, -1.65 * zoom), // Up, slightly left
                _ => Vec2::ZERO,
            };

            (valign_label, halign_label) = match rotation {
                0 => (Align::BOTTOM, Align::LEFT),
                1 => (Align::TOP, Align::LEFT),
                2 => (Align::BOTTOM, Align::RIGHT),
                3 => (Align::BOTTOM, Align::LEFT),
                _ => (Align::Center, Align::Center),
            };

            (valign_value, halign_value) = match rotation {
                0 => (Align::TOP, Align::LEFT),
                1 => (Align::TOP, Align::Center),
                2 => (Align::TOP, Align::RIGHT),
                3 => (Align::BOTTOM, Align::Center),
                _ => (Align::Center, Align::Center),
            };

            (label_pos, value_pos)
        }
        _ => (Vec2::new(0.0, -0.7 * zoom), Vec2::new(0.0, 0.7 * zoom)),
    };

    let label_pos = center + offset_label;
    let value_pos = center + offset_value;

    let mut label_layout = LayoutJob::default();
    label_layout.halign = halign_label;
    RichText::new(label).strong().append_to(
        &mut label_layout,
        &Style::default(),
        FontSelection::FontId(egui::FontId::monospace(zoom * 0.4)),
        valign_label,
    );

    let label_galley = painter.layout_job(label_layout);

    painter.galley(label_pos, label_galley, Color32::WHITE);

    if component.component == ComponentBuildData::Label {
        return;
    }

    let mapping = match &component.component {
        ComponentBuildData::Resistor { resistance } => vec![(*resistance, "Ω")],
        ComponentBuildData::Capacitor {
            capacitance,
            esr: _,
        } => vec![(*capacitance, "F")],
        ComponentBuildData::DCSource { voltage } => vec![(*voltage, "V")],
        ComponentBuildData::ASource {
            amplitude,
            frequency,
        } => vec![(*amplitude, "V"), (*frequency, "Hz")],
        ComponentBuildData::Inductor {
            inductance,
            series_resistance: _,
        } => vec![(*inductance, "H")],
        _ => vec![(0.0, "")],
    };

    let value = match &component.component {
        ComponentBuildData::Diode { model } => model.format_name().to_string(),
        ComponentBuildData::Bjt { model } => model.format_name().to_string(),
        _ => format_si(mapping.as_slice(), 0.1, 2),
    };

    if !value.is_empty() {
        let formatted_value = value;
        let mut value_layout = LayoutJob::default();
        value_layout.halign = halign_value;
        // For some reason the valign doesn't do anything here? I'm adjusting when applying the offsets instead.
        RichText::new(&formatted_value).append_to(
            &mut value_layout,
            &Style::default(),
            FontSelection::FontId(egui::FontId::monospace(zoom * 0.4)),
            valign_value,
        );
        let value_galley = painter.layout_job(value_layout);
        painter.galley(
            value_pos,
            value_galley,
            Color32::WHITE.blend(Color32::LIGHT_GRAY),
        );
    }

    if let ComponentBuildData::Bjt { model } = &component.component {
        draw_bjt_pin_labels(component, painter, transform, zoom, model);
    }
}

fn draw_bjt_pin_labels(
    component: &VisualComponent,
    painter: &Painter,
    transform: impl Fn(GridPos) -> Pos2,
    zoom: f32,
    model: &BjtModel,
) {
    let center = transform(component.pos);
    let rotation = component.rotation % 4;
    let is_npn = model.polarity();

    let rotate_vec = |x: f32, y: f32| -> Vec2 {
        match rotation {
            0 => Vec2::new(x, y),
            1 => Vec2::new(-y, x),
            2 => Vec2::new(-x, -y),
            3 => Vec2::new(y, -x),
            _ => Vec2::new(x, y),
        }
    };

    let top_pin_offset = rotate_vec(1.0, -1.0) * zoom;
    let base_pin_offset = rotate_vec(-1.0, 0.0) * zoom;
    let bot_pin_offset = rotate_vec(1.0, 1.0) * zoom;

    let (top_char, bot_char) = if is_npn { ("C", "E") } else { ("E", "C") };
    let base_char = "B";

    let (base_text_offset, pin_text_offset) = match rotation {
        1 => (
            Vec2::new(-0.4 * zoom, 0.0), // Base: Left
            Vec2::new(0.0, -0.4 * zoom), // Pins: Top
        ),
        3 => (
            Vec2::new(-0.4 * zoom, 0.0), // Base: Left
            Vec2::new(0.0, 0.4 * zoom),  // Pins: Bottom
        ),
        _ => (
            Vec2::new(0.0, -0.4 * zoom), // Base: Over
            Vec2::new(0.3 * zoom, 0.0),  // Pins: Right
        ),
    };

    let top_pos = center + top_pin_offset + pin_text_offset;
    let base_pos = center + base_pin_offset + base_text_offset;
    let bot_pos = center + bot_pin_offset + pin_text_offset;

    let draw_text = |pos: Pos2, text: &str| {
        painter.text(
            pos,
            Align2::CENTER_CENTER,
            text,
            egui::FontId::monospace(zoom * 0.3),
            Color32::WHITE,
        );
    };

    draw_text(top_pos, top_char);
    draw_text(base_pos, base_char);
    draw_text(bot_pos, bot_char);
}
