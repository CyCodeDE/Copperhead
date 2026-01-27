use crate::model::GridPos;
use crate::ui::{ComponentBuildData, VisualComponent};
use crate::util::format_si;
use eframe::emath::{Align, Pos2, Rect, Vec2};
use eframe::epaint::text::LayoutJob;
use eframe::epaint::{Color32, Shape, Stroke, StrokeKind};
use egui::{FontSelection, Painter, RichText, Style};

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

// Tbh I vibe coded the component visuals. Looks decent enough. Might not be most optimal for performance but who tf cares

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

    // Draw Wires (From edge to plates)
    // Left Wire: (-1.0, 0) -> (-plate_gap, 0)
    // Right Wire: (1.0, 0) -> (plate_gap, 0)
    let wire_left_start = rotate_vec(Vec2::new(-1.0, 0.0) * zoom, rotation);
    let wire_left_end = rotate_vec(Vec2::new(-plate_gap, 0.0) * zoom, rotation);
    painter.line_segment([center + wire_left_start, center + wire_left_end], stroke);

    let wire_right_start = rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);
    let wire_right_end = rotate_vec(Vec2::new(plate_gap, 0.0) * zoom, rotation);
    painter.line_segment([center + wire_right_start, center + wire_right_end], stroke);

    // 2. Draw Plates (Vertical lines perpendicular to wire)
    // Left Plate: (-plate_gap, -h/2) -> (-plate_gap, h/2)
    let p1_top = rotate_vec(Vec2::new(-plate_gap, -plate_height / 2.0) * zoom, rotation);
    let p1_bot = rotate_vec(Vec2::new(-plate_gap, plate_height / 2.0) * zoom, rotation);
    painter.line_segment([center + p1_top, center + p1_bot], stroke);

    // Right Plate
    let p2_top = rotate_vec(Vec2::new(plate_gap, -plate_height / 2.0) * zoom, rotation);
    let p2_bot = rotate_vec(Vec2::new(plate_gap, plate_height / 2.0) * zoom, rotation);
    painter.line_segment([center + p2_top, center + p2_bot], stroke);
}

fn draw_inductor(painter: &Painter, center: Pos2, rotation: u8, zoom: f32, stroke_color: Color32) {
    // Inductor is 2x1.
    // Visual Pins are at x = -1.0 and x = 1.0.

    let stroke = Stroke::new(2.0, stroke_color);

    // Geometry settings
    let num_coils = 4;
    let lead_length = 0.2; // Straight wire segment at ends
    let total_width = 2.0;

    // Calculate coil dimensions
    // Available width for coils = Total - (Left Lead + Right Lead)
    let coil_section_width = total_width - (2.0 * lead_length);
    let loop_width = coil_section_width / num_coils as f32;
    let loop_radius = loop_width / 2.0;

    // Determine Y-offset for the "hump"
    // For a cubic bezier to approximate a semi-circle, the control points
    // should be at distance (4/3)*r from the baseline.
    let k = 1.33333;
    let ctrl_height = loop_radius * k;

    // 1. Draw Left Lead: (-1.0, 0) -> (-0.8, 0)
    let start_x = -1.0;
    let coil_start_x = -1.0 + lead_length;

    let p_lead_start = center + rotate_vec(Vec2::new(start_x, 0.0) * zoom, rotation);
    let p_coil_start = center + rotate_vec(Vec2::new(coil_start_x, 0.0) * zoom, rotation);

    painter.line_segment([p_lead_start, p_coil_start], stroke);

    // 2. Draw 4 Semi-Circles
    let mut current_x = coil_start_x;

    for _ in 0..num_coils {
        let next_x = current_x + loop_width;

        // Local points
        let p1_local = Vec2::new(current_x, 0.0);
        let p2_local = Vec2::new(next_x, 0.0);

        // Control points go "Up" (Negative Y in local coords usually implies up in screen space logic
        // depending on rotation, but we maintain consistency with the resistor/cap alignment).
        // Using -ctrl_height makes the humps point "up" relative to the wire line.
        let c1_local = Vec2::new(current_x, -ctrl_height);
        let c2_local = Vec2::new(next_x, -ctrl_height);

        // Transform to screen space
        let p1 = center + rotate_vec(p1_local * zoom, rotation);
        let p2 = center + rotate_vec(p2_local * zoom, rotation);
        let c1 = center + rotate_vec(c1_local * zoom, rotation);
        let c2 = center + rotate_vec(c2_local * zoom, rotation);

        // Draw Bezier
        let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
            [p1, c1, c2, p2],
            false,
            Color32::TRANSPARENT,
            stroke,
        );
        painter.add(bezier);

        current_x = next_x;
    }

    // 3. Draw Right Lead: (0.8, 0) -> (1.0, 0)
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
    // DC Source is 1x2 (Vertical).
    // Pins are at Top (0, -1) and Bottom (0, 1) in local space (assuming vertical default).

    let radius = 0.4;
    let stroke = Stroke::new(2.0, stroke_color);

    // 1. Draw Circle Body
    painter.circle(center, radius * zoom, fill_color, stroke);

    // 2. Draw Wires extending from circle to pins
    // Top Wire: (0, -1.0) -> (0, -radius)
    let top_pin = rotate_vec(Vec2::new(0.0, -1.0) * zoom, rotation);
    let top_circle = rotate_vec(Vec2::new(0.0, -radius) * zoom, rotation);
    painter.line_segment([center + top_pin, center + top_circle], stroke);

    // Bottom Wire: (0, 1.0) -> (0, radius)
    let bot_pin = rotate_vec(Vec2::new(0.0, 1.0) * zoom, rotation);
    let bot_circle = rotate_vec(Vec2::new(0.0, radius) * zoom, rotation);
    painter.line_segment([center + bot_pin, center + bot_circle], stroke);

    // 3. Draw Symbols (+ and -) inside
    // Because we rotate the whole coordinate system, the text would rotate too.
    // To keep text upright, we calculate position but don't rotate the text orientation itself.

    // "+" Position (Top inside)
    let pos_plus = center + rotate_vec(Vec2::new(0.0, -0.15) * zoom, rotation);
    painter.text(
        pos_plus,
        egui::Align2::CENTER_CENTER,
        "+",
        egui::FontId::proportional(zoom * 0.4),
        stroke_color,
    );

    // "-" Position (Bottom inside)
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
    // Structure is identical to DC Source (1x2 vertical)
    let radius = 0.4;
    let stroke = Stroke::new(2.0, stroke_color);

    // 1. Draw Circle Body
    painter.circle(center, radius * zoom, fill_color, stroke);

    // 2. Draw Wires
    let top_pin = rotate_vec(Vec2::new(0.0, -1.0) * zoom, rotation);
    let top_circle = rotate_vec(Vec2::new(0.0, -radius) * zoom, rotation);
    painter.line_segment([center + top_pin, center + top_circle], stroke);

    let bot_pin = rotate_vec(Vec2::new(0.0, 1.0) * zoom, rotation);
    let bot_circle = rotate_vec(Vec2::new(0.0, radius) * zoom, rotation);
    painter.line_segment([center + bot_pin, center + bot_circle], stroke);

    // 3. Draw Sine Wave
    // A sine wave inside the circle.
    // Let's approximate it with 4 points or a bezier.
    // Local coords: roughly from (-0.25, 0) to (0.25, 0)

    // We construct a path relative to (0,0) and rotate points
    let sine_points_local = [
        Vec2::new(-0.25, 0.0),   // Start
        Vec2::new(-0.12, -0.15), // Peak 1 Control/Point
        Vec2::new(0.12, 0.15),   // Peak 2 Control/Point
        Vec2::new(0.25, 0.0),    // End
    ];

    // For a smoother look in Egui we can use a Cubic Bezier, but line segments are easier to implement quickly:
    // Simple line approximation: Start -> Peak Top -> Crossing -> Peak Bottom -> End
    let wave_width = 0.25;
    let wave_amp = 0.15;

    // We will draw a cubic bezier curve manually-ish or just small segments.
    // Let's use a cubic bezier shape for smoothness.
    let start = center + rotate_vec(Vec2::new(-wave_width, 0.0) * zoom, rotation);
    let end = center + rotate_vec(Vec2::new(wave_width, 0.0) * zoom, rotation);

    // Control points for the S-shape
    let c1 = center
        + rotate_vec(
            Vec2::new(-wave_width / 2.0, -wave_amp * 2.0) * zoom,
            rotation,
        );
    let c2 = center + rotate_vec(Vec2::new(wave_width / 2.0, wave_amp * 2.0) * zoom, rotation);

    let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
        [start, c1, c2, end],
        false,
        Color32::TRANSPARENT, // fill
        stroke,
    );
    painter.add(bezier);
}

fn draw_ground(painter: &Painter, center: Pos2, rotation: u8, zoom: f32, stroke_color: Color32) {
    // Ground is 1x1.
    // Pin location is usually the "node" point.
    // Standard Ground Symbol:
    // Pin is at (0, -0.5) (Top of the box).
    // Main vertical line goes down to (0, 0) or (0, 0.2).
    // Let's say pin is at (0, -0.5) and the symbol is centered around y=0.

    let stroke = Stroke::new(2.0, stroke_color);

    /*// 1. Main Vertical Wire: From Pin (Top) to Center
    // Assuming the grid pin is at the edge of the 1x1 cell, which is (0, -0.5) locally.
    let pin_pos = rotate_vec(Vec2::new(0.0, -0.5) * zoom, rotation);
    let symbol_top = rotate_vec(Vec2::new(0.0, 0.0) * zoom, rotation); // Center of the visual
    painter.line_segment([center + pin_pos, center + symbol_top], stroke);*/

    // 2. Horizontal Lines (Inverted Triangle)
    // Line 1: Wide
    let l1_start = center + rotate_vec(Vec2::new(-0.4, 0.0) * zoom, rotation);
    let l1_end = center + rotate_vec(Vec2::new(0.4, 0.0) * zoom, rotation);
    painter.line_segment([l1_start, l1_end], stroke);

    // Line 2: Medium
    let l2_y_offset = 0.15;
    let l2_start = center + rotate_vec(Vec2::new(-0.25, l2_y_offset) * zoom, rotation);
    let l2_end = center + rotate_vec(Vec2::new(0.25, l2_y_offset) * zoom, rotation);
    painter.line_segment([l2_start, l2_end], stroke);

    // Line 3: Small
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
    // Fallback logic from original code
    // Assuming 2x1 default
    let (base_w, base_h) = (2.0, 1.0);

    // Adjust logic for bounding box rotation
    // Note: This logic assumes 0/180 are Horizontal, 90/270 are Vertical
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

    // Dimensions
    // The symbol body fits roughly between x = -0.5 and x = 0.5
    let half_len = 0.5; // Distance from center to the edge of the symbol body
    let half_h = 0.5; // Half height of the triangle/bar

    // 1. Draw Wires (Leads)
    // Left (Anode) Lead: (-1.0, 0) -> (-0.5, 0)
    let lead_anode_start = rotate_vec(Vec2::new(-1.0, 0.0) * zoom, rotation);
    let lead_anode_end = rotate_vec(Vec2::new(-half_len, 0.0) * zoom, rotation);
    painter.line_segment([center + lead_anode_start, center + lead_anode_end], stroke);

    // Right (Cathode) Lead: (0.5, 0) -> (1.0, 0)
    let lead_cathode_start = rotate_vec(Vec2::new(half_len, 0.0) * zoom, rotation);
    let lead_cathode_end = rotate_vec(Vec2::new(1.0, 0.0) * zoom, rotation);
    painter.line_segment(
        [center + lead_cathode_start, center + lead_cathode_end],
        stroke,
    );

    // 2. Draw Triangle (Anode Body)
    // Points relative to center (before rotation)
    // - Base is at x = -0.5
    // - Tip is at x = 0.5
    let triangle_points_local = [
        Vec2::new(-half_len, -half_h), // Top Left
        Vec2::new(-half_len, half_h),  // Bottom Left
        Vec2::new(half_len, 0.0),      // Tip (Right)
    ];

    let triangle_points_screen: Vec<Pos2> = triangle_points_local
        .iter()
        .map(|&p| center + rotate_vec(p * zoom, rotation))
        .collect();

    // Use convex_polygon to allow for filling (standard DIN is often hollow, but fill_color is supported)
    painter.add(Shape::convex_polygon(
        triangle_points_screen,
        fill_color,
        stroke,
    ));

    // 3. Draw Cathode Bar (Vertical line at the tip)
    // Line from (0.5, -0.4) to (0.5, 0.4)
    let bar_top = center + rotate_vec(Vec2::new(half_len, -half_h) * zoom, rotation);
    let bar_bot = center + rotate_vec(Vec2::new(half_len, half_h) * zoom, rotation);

    painter.line_segment([bar_top, bar_bot], stroke);
}

// Helper to check if a line segment (p1, p2) intersects a Rectangle
pub fn check_line_rect_intersection(p1: Pos2, p2: Pos2, rect: Rect) -> bool {
    // 1. Trivial accept: Start or End is inside
    if rect.contains(p1) || rect.contains(p2) {
        return true;
    }

    // 2. Trivial reject: Bounding box of line does not intersect rect
    let line_bounds = Rect::from_two_pos(p1, p2);
    if !rect.intersects(line_bounds) {
        return false;
    }

    // 3. Line intersection test (Liang-Barsky or checking intersection with rect sides)
    // A simplified approach for 2D UI:
    // The line intersects the rect if it intersects any of the 4 segments of the rect
    // OR if the line is completely inside (covered by step 1).
    let corners = [
        rect.min,
        Pos2::new(rect.max.x, rect.min.y),
        rect.max,
        Pos2::new(rect.min.x, rect.max.y),
    ];

    // Check intersection with Top, Right, Bottom, Left edges
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
        return; // No label for ground
    }

    let center = transform(component.pos);
    let rotation = component.rotation % 4; // Ensure 0-3 range

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
                0 | 2 => Vec2::new(0.0, -1.1 * zoom),        // Above
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
        _ => {
            (Vec2::new(0.0, -0.7 * zoom), Vec2::new(0.0, 0.7 * zoom))
        }
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
        return; // No value for label components
    }


    let mapping = match &component.component {
        ComponentBuildData::Resistor { resistance } => vec![(*resistance, "Ω")],
        ComponentBuildData::Capacitor { capacitance } => vec![(*capacitance, "F")],
        ComponentBuildData::DCSource { voltage } => vec![(*voltage, "V")],
        ComponentBuildData::ASource {
            amplitude,
            frequency,
        } => vec![(*amplitude, "V"), (*frequency, "Hz")],
        ComponentBuildData::Inductor { inductance } => vec![(*inductance, "H")],
        _ => vec![(0.0, "")],
    };

    let value = match &component.component {
        ComponentBuildData::Diode { model } => model.format_name(),
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
}
