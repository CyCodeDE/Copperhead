use crate::ui::SimStepData;
use crate::util::format_si;
use egui::epaint::PathShape;
use egui::{
    Align2, Color32, FontId, Painter, PointerButton, Pos2, Rect, Response, Sense, Stroke,
    StrokeKind, Ui, Vec2, pos2, vec2,
};

#[derive(Debug, Clone)]
pub struct ScopeState {
    pub time_range: (f64, f64),
    pub voltage_range: (f64, f64),
    pub current_range: (f64, f64),
    pub paused: bool,
}

impl Default for ScopeState {
    fn default() -> Self {
        Self {
            time_range: (0.0, 0.02),    // 20ms
            voltage_range: (-5.0, 5.0), // +/- 5V
            current_range: (-0.1, 0.1), // +/- 100mA
            paused: false,
        }
    }
}

struct ScopeContext {
    rect: Rect,
    time_range: (f64, f64),
    volt_range: (f64, f64),
    curr_range: (f64, f64),
    time_scale: f64,
    volt_scale: f64,
    curr_scale: f64,
}

impl ScopeContext {
    fn new(rect: Rect, state: &ScopeState) -> Self {
        let w = rect.width() as f64;
        let h = rect.height() as f64;

        let t_span = (state.time_range.1 - state.time_range.0).max(1e-12);
        let v_span = (state.voltage_range.1 - state.voltage_range.0).max(1e-12);
        let i_span = (state.current_range.1 - state.current_range.0).max(1e-12);

        Self {
            rect,
            time_range: state.time_range,
            volt_range: state.voltage_range,
            curr_range: state.current_range,
            time_scale: w / t_span,
            volt_scale: h / v_span,
            curr_scale: h / i_span,
        }
    }

    fn t_to_x(&self, t: f64) -> f32 {
        (self.rect.left() as f64 + (t - self.time_range.0) * self.time_scale) as f32
    }

    fn x_to_t(&self, x: f32) -> f64 {
        self.time_range.0 + (x as f64 - self.rect.left() as f64) / self.time_scale
    }

    fn v_to_y(&self, v: f64) -> f32 {
        (self.rect.bottom() as f64 - (v - self.volt_range.0) * self.volt_scale) as f32
    }

    fn y_to_v(&self, y: f32) -> f64 {
        self.volt_range.0 + (self.rect.bottom() as f64 - y as f64) / self.volt_scale
    }

    fn i_to_y(&self, i: f64) -> f32 {
        (self.rect.bottom() as f64 - (i - self.curr_range.0) * self.curr_scale) as f32
    }

    fn y_to_i(&self, y: f32) -> f64 {
        self.curr_range.0 + (self.rect.bottom() as f64 - y as f64) / self.curr_scale
    }
}
pub fn draw_oscilloscope(
    ui: &mut Ui,
    state: &mut ScopeState,
    data: &[SimStepData],
    voltage_node_idx: Option<usize>,
    current_comp_idx: Option<usize>,
) {
    let height = ui.available_height();
    let (mut response, painter) = ui.allocate_painter(
        Vec2::new(ui.available_width(), height),
        Sense::click_and_drag(),
    );

    // Draw Background
    painter.rect_filled(response.rect, 2.0, Color32::from_rgb(15, 17, 26));
    painter.rect_stroke(
        response.rect,
        2.0,
        Stroke::new(1.0, Color32::from_gray(60)),
        StrokeKind::Inside,
    );

    let ctx_input = ScopeContext::new(response.rect, state);

    handle_input(
        ui,
        &response,
        state,
        &ctx_input,
        data,
        voltage_node_idx,
        current_comp_idx,
    );

    let ctx_draw = ScopeContext::new(response.rect, state);

    draw_grid(
        &painter,
        &ctx_draw,
        voltage_node_idx.is_some(),
        current_comp_idx.is_some(),
    );

    // Draw Traces
    let content_rect = response.rect.shrink(1.0);
    let mut clip_painter = painter.with_clip_rect(content_rect);

    if !data.is_empty() {
        // Draw Voltage
        if let Some(v_idx) = voltage_node_idx {
            draw_trace_optimized(
                &mut clip_painter,
                &ctx_draw,
                data,
                TraceType::Voltage(v_idx),
                Color32::from_rgb(50, 255, 50),
            );
        }

        // Draw Current
        if let Some(i_idx) = current_comp_idx {
            draw_trace_optimized(
                &mut clip_painter,
                &ctx_draw,
                data,
                TraceType::Current(i_idx),
                Color32::from_rgb(255, 100, 50),
            );
        }

        // Cursor
        if response.hovered() && !response.dragged() {
            if let Some(hover_pos) = response.hover_pos() {
                draw_cursor(
                    &painter,
                    &ctx_draw,
                    data,
                    hover_pos,
                    voltage_node_idx,
                    current_comp_idx,
                );
            }
        }
    }

    // Legend
    draw_legend(
        ui,
        response.rect,
        voltage_node_idx.is_some(),
        current_comp_idx.is_some(),
    );
}

fn handle_input(
    ui: &mut Ui,
    response: &Response,
    state: &mut ScopeState,
    ctx: &ScopeContext,
    data: &[SimStepData],
    v_idx: Option<usize>,
    i_idx: Option<usize>,
) {
    let zoom_drag_id = response.id.with("box_zoom");

    if response.double_clicked() {
        auto_fit_ranges(state, data, v_idx, i_idx);
        return;
    }

    // Pan (Mouse Drag)
    if response.dragged_by(PointerButton::Middle) || response.dragged_by(PointerButton::Secondary) {
        let delta = response.drag_delta();
        let dt = -delta.x as f64 / ctx.time_scale;
        state.time_range.0 += dt;
        state.time_range.1 += dt;

        let dv = delta.y as f64 / ctx.volt_scale;
        state.voltage_range.0 += dv;
        state.voltage_range.1 += dv;

        let di = delta.y as f64 / ctx.curr_scale;
        state.current_range.0 += di;
        state.current_range.1 += di;
    }

    // Scroll Input: Horizontal Pan (Shift) OR Zoom (Default/Ctrl/Alt)
    if response.hovered() {
        // Retrieve input state once to handle priorities
        let (scroll_delta, modifiers) = ui.input(|i| (i.smooth_scroll_delta, i.modifiers));

        // Horizontal Pan
        if modifiers.shift && (scroll_delta.x != 0.0 || scroll_delta.y != 0.0) {
            // Some OS/Mice convert Shift+VerticalScroll into Horizontal Scroll (delta.x).
            // Others keep it as Vertical (delta.y). We check both.
            let pixel_delta = if scroll_delta.x != 0.0 {
                -scroll_delta.x
            } else {
                -scroll_delta.y
            };

            let dt = pixel_delta as f64 / ctx.time_scale;
            state.time_range.0 += dt;
            state.time_range.1 += dt;
        } else if scroll_delta.y != 0.0 {
            let zoom_factor = if scroll_delta.y > 0.0 { 0.90 } else { 1.10 };
            let hover_pos = response.hover_pos().unwrap_or(response.rect.center());

            let zoom_y = modifiers.alt;

            if zoom_y {
                let v_hover = ctx.y_to_v(hover_pos.y);
                let v_span = state.voltage_range.1 - state.voltage_range.0;
                let new_v_span = v_span * zoom_factor;
                let v_ratio = (v_hover - state.voltage_range.0) / v_span;
                state.voltage_range.0 = v_hover - new_v_span * v_ratio;
                state.voltage_range.1 = v_hover + new_v_span * (1.0 - v_ratio);

                let i_hover = ctx.y_to_i(hover_pos.y);
                let i_span = state.current_range.1 - state.current_range.0;
                let new_i_span = i_span * zoom_factor;
                let i_ratio = (i_hover - state.current_range.0) / i_span;
                state.current_range.0 = i_hover - new_i_span * i_ratio;
                state.current_range.1 = i_hover + new_i_span * (1.0 - i_ratio);
            } else {
                let t_hover = ctx.x_to_t(hover_pos.x);
                let t_span = state.time_range.1 - state.time_range.0;
                let new_t_span = t_span * zoom_factor;
                let ratio = (t_hover - state.time_range.0) / t_span;
                state.time_range.0 = t_hover - new_t_span * ratio;
                state.time_range.1 = t_hover + new_t_span * (1.0 - ratio);
            }
        }
    }

    // Box Zoom
    if response.drag_started_by(PointerButton::Primary) {
        if let Some(pos) = response.hover_pos() {
            ui.data_mut(|d| d.insert_temp(zoom_drag_id, pos));
        }
    }

    if response.dragged_by(PointerButton::Primary) {
        if let Some(start_pos) = ui.data(|d| d.get_temp::<Pos2>(zoom_drag_id)) {
            if let Some(curr_pos) = response.hover_pos() {
                let rect = Rect::from_two_pos(start_pos, curr_pos);
                ui.painter().rect(
                    rect,
                    0.0,
                    Color32::from_rgba_unmultiplied(255, 255, 255, 20),
                    Stroke::new(1.0, Color32::from_white_alpha(150)),
                    StrokeKind::Middle,
                );
            }
        }
    }

    if response.drag_stopped_by(PointerButton::Primary) {
        if let Some(start_pos) = ui.data(|d| d.get_temp::<Pos2>(zoom_drag_id)) {
            ui.data_mut(|d| d.remove::<Pos2>(zoom_drag_id));
            if let Some(end_pos) = response.hover_pos() {
                if start_pos.distance(end_pos) > 10.0 {
                    let selection = Rect::from_two_pos(start_pos, end_pos).intersect(ctx.rect);
                    if selection.is_positive() {
                        let t1 = ctx.x_to_t(selection.left());
                        let t2 = ctx.x_to_t(selection.right());
                        let v1 = ctx.y_to_v(selection.bottom());
                        let v2 = ctx.y_to_v(selection.top());
                        let i1 = ctx.y_to_i(selection.bottom());
                        let i2 = ctx.y_to_i(selection.top());

                        state.time_range = (t1.min(t2), t1.max(t2));
                        state.voltage_range = (v1.min(v2), v1.max(v2));
                        state.current_range = (i1.min(i2), i1.max(i2));
                    }
                }
            }
        }
    }
}

fn auto_fit_ranges(
    state: &mut ScopeState,
    data: &[SimStepData],
    v_idx: Option<usize>,
    i_idx: Option<usize>,
) {
    if data.is_empty() {
        return;
    }

    // Initialize bounds.
    // We use infinite defaults so the first data point always overwrites them.
    let mut t_min = f64::INFINITY;
    let mut t_max = f64::NEG_INFINITY;

    let mut v_min = f64::INFINITY;
    let mut v_max = f64::NEG_INFINITY;

    let mut i_min = f64::INFINITY;
    let mut i_max = f64::NEG_INFINITY;

    // Iterate data once to gather all stats
    for d in data {
        // Time bounds
        if d.time < t_min {
            t_min = d.time;
        }
        if d.time > t_max {
            t_max = d.time;
        }

        // Voltage bounds (if index provided)
        if let Some(idx) = v_idx {
            let v = d.voltages.get(idx).copied().unwrap_or(0.0);
            if v < v_min {
                v_min = v;
            }
            if v > v_max {
                v_max = v;
            }
        }

        // Current bounds (if index provided)
        if let Some(idx) = i_idx {
            let i = d.currents.get(idx).copied().unwrap_or(0.0);
            if i < i_min {
                i_min = i;
            }
            if i > i_max {
                i_max = i;
            }
        }
    }

    // Apply Time Range (2% padding)
    let t_pad = (t_max - t_min) * 0.02;
    state.time_range = (t_min - t_pad, t_max + t_pad);

    // Helper closure to calculate range with specific padding (margin).
    let calc_range = |min: f64, max: f64, margin_scale: f64, default_span: f64| -> (f64, f64) {
        let diff = max - min;
        if diff.abs() < 1e-9 {
            // Flat line handling: Center it with a fixed span
            let half = default_span / 2.0;
            (min - half, min + half)
        } else {
            // Calculate padding based on the amplitude
            let p = diff * margin_scale;
            (min - p, max + p)
        }
    };

    // We use different margin scales for Voltage and Current.

    // Voltage: 5% padding (Trace occupies ~90% of screen height)
    if v_idx.is_some() {
        state.voltage_range = calc_range(v_min, v_max, 0.05, 2.0);
    }

    // Current: 20% padding (Trace occupies ~60% of screen height)
    // This ensures that even if V and I are perfectly correlated,
    // the Current wave will look visually "shorter" than the Voltage wave,
    // separating them on the pixel grid.
    if i_idx.is_some() {
        state.current_range = calc_range(i_min, i_max, 0.20, 0.2);
    }
}

fn draw_grid(painter: &Painter, ctx: &ScopeContext, show_v: bool, show_i: bool) {
    let major_color = Color32::from_rgb(50, 60, 75);
    let minor_color = Color32::from_rgb(30, 35, 45);
    let text_color = Color32::from_gray(180);
    let font = FontId::monospace(10.0);

    fn calc_step(range: f64, steps: f64) -> f64 {
        let raw = range / steps;
        let mag = 10.0f64.powf(raw.log10().floor());
        let norm = raw / mag;
        let s = if norm < 1.5 {
            1.0
        } else if norm < 3.5 {
            2.0
        } else {
            5.0
        };
        s * mag
    }

    let t_step = calc_step(ctx.time_range.1 - ctx.time_range.0, 10.0);
    let t_start = (ctx.time_range.0 / t_step).floor() as i64;
    let t_end = (ctx.time_range.1 / t_step).ceil() as i64;

    for i in t_start..=t_end {
        let t = i as f64 * t_step;
        let x = ctx.t_to_x(t);
        if x >= ctx.rect.left() - 0.5 && x <= ctx.rect.right() + 0.5 {
            painter.line_segment(
                [pos2(x, ctx.rect.top()), pos2(x, ctx.rect.bottom())],
                Stroke::new(1.0, major_color),
            );
            let sub = t_step / 5.0;
            for j in 1..5 {
                let sx = ctx.t_to_x(t + j as f64 * sub);
                if sx < ctx.rect.right() {
                    painter.line_segment(
                        [pos2(sx, ctx.rect.top()), pos2(sx, ctx.rect.bottom())],
                        Stroke::new(0.5, minor_color),
                    );
                }
            }
            painter.text(
                pos2(x + 3.0, ctx.rect.bottom() - 12.0),
                Align2::LEFT_BOTTOM,
                format_si(&[(t, "s")], 0.1, 2),
                font.clone(),
                text_color,
            );
        }
    }

    if show_v {
        let v_step = calc_step(ctx.volt_range.1 - ctx.volt_range.0, 8.0);
        let v_start = (ctx.volt_range.0 / v_step).floor() as i64;
        let v_end = (ctx.volt_range.1 / v_step).ceil() as i64;
        for i in v_start..=v_end {
            let v = i as f64 * v_step;
            let y = ctx.v_to_y(v);
            if y >= ctx.rect.top() - 0.5 && y <= ctx.rect.bottom() + 0.5 {
                painter.line_segment(
                    [pos2(ctx.rect.left(), y), pos2(ctx.rect.right(), y)],
                    Stroke::new(1.0, major_color.gamma_multiply(0.5)),
                );
                let label = if i == 0 {
                    "GND".to_string()
                } else {
                    format_si(&[(v, "V")], 0.1, 2)
                };
                painter.text(
                    pos2(ctx.rect.left() + 2.0, y - 2.0),
                    Align2::LEFT_BOTTOM,
                    label,
                    font.clone(),
                    Color32::from_rgb(100, 200, 100),
                );
            }
        }
    }

    if show_i {
        let i_step = calc_step(ctx.curr_range.1 - ctx.curr_range.0, 8.0);
        let i_start = (ctx.curr_range.0 / i_step).floor() as i64;
        let i_end = (ctx.curr_range.1 / i_step).ceil() as i64;
        for i in i_start..=i_end {
            let val = i as f64 * i_step;
            let y = ctx.i_to_y(val);
            if y >= ctx.rect.top() && y <= ctx.rect.bottom() {
                painter.line_segment(
                    [pos2(ctx.rect.right() - 6.0, y), pos2(ctx.rect.right(), y)],
                    Stroke::new(1.5, Color32::from_rgb(200, 100, 100)),
                );
                painter.text(
                    pos2(ctx.rect.right() - 8.0, y - 2.0),
                    Align2::RIGHT_BOTTOM,
                    format_si(&[(val, "A")], 0.1, 2),
                    font.clone(),
                    Color32::from_rgb(200, 100, 100),
                );
            }
        }
    }
}

enum TraceType {
    Voltage(usize),
    Current(usize),
}

fn draw_trace_optimized(
    painter: &mut Painter,
    ctx: &ScopeContext,
    data: &[SimStepData],
    mode: TraceType,
    color: Color32,
) {
    let screen_w = ctx.rect.width();
    if screen_w < 1.0 {
        return;
    }

    // Cull data to visible range
    let start_idx = data
        .partition_point(|d| d.time < ctx.time_range.0)
        .saturating_sub(1);
    let end_idx = data
        .partition_point(|d| d.time < ctx.time_range.1)
        .min(data.len());
    let slice = &data[start_idx..end_idx];

    if slice.is_empty() {
        return;
    }

    // Helper to extract value
    let get_val = |d: &SimStepData| match mode {
        TraceType::Voltage(idx) => d.voltages.get(idx).copied().unwrap_or(0.0),
        TraceType::Current(idx) => d.currents.get(idx).copied().unwrap_or(0.0),
    };

    // Helper to map Y coordinate
    let map_y = |val: f64| match mode {
        TraceType::Voltage(_) => ctx.v_to_y(val),
        TraceType::Current(_) => ctx.i_to_y(val),
    };

    // Determine Strategy based on data density
    // Egui handles ~40k vertices comfortably.
    let max_raw_points = 40_000;
    let point_count = slice.len();

    let mut points = Vec::new();

    if point_count <= max_raw_points {
        // High Fidelity (Raw)
        // Best for zoomed in or medium datasets.
        points.reserve(point_count);
        for step in slice {
            let x = ctx.t_to_x(step.time);
            let y = map_y(get_val(step));
            points.push(pos2(x, y));
        }
    } else {
        // Aggregated (Pixel Binning)
        // Best for massive datasets (10M+ points) zoomed out.

        points.reserve((screen_w as usize) * 2);

        // We iterate pixels across the screen
        let rect_left = ctx.rect.left();
        let pixel_width = screen_w as i32;

        let mut current_idx = start_idx;

        // How much data to skip if the density is absurdly high (e.g. 1 million points per pixel)
        // This caps the inner loop performance cost.
        let stride_threshold = 500; // If a pixel has > 500 points, we skip some.

        for px in 0..=pixel_width {
            let x_screen = rect_left + px as f32;
            let t_end = ctx.x_to_t(x_screen + 1.0); // End time for this pixel

            // Find where this pixel ends in the data
            let remaining = &data[current_idx..end_idx];
            if remaining.is_empty() {
                break;
            }

            // Find count of points in this pixel
            let count = remaining.partition_point(|d| d.time < t_end);
            let next_idx = current_idx + count;

            if count == 0 {
                // No data in this pixel, connect line to next point
                continue;
            }

            // Calculate Min/Max for this pixel
            let pixel_slice = &data[current_idx..next_idx];
            let (min_val, max_val);

            if count > stride_threshold {
                // If we have 10,000 points in one pixel, don't read them all.
                // Step through them. We might miss a 1-sample spike, but we keep 60 FPS.
                let step = count / stride_threshold;
                let (min, max) =
                    pixel_slice
                        .iter()
                        .step_by(step)
                        .fold((f64::MAX, f64::MIN), |(min, max), d| {
                            let v = get_val(d);
                            (min.min(v), max.max(v))
                        });
                min_val = min;
                max_val = max;
            } else {
                // Read all points in this pixel.
                let (min, max) = pixel_slice
                    .iter()
                    .fold((f64::MAX, f64::MIN), |(min, max), d| {
                        let v = get_val(d);
                        (min.min(v), max.max(v))
                    });
                min_val = min;
                max_val = max;
            }

            // Draw a vertical line for the range at this pixel.
            // To make it look like a connected waveform, we add both min and max points at the same X.
            let y_min = map_y(min_val);
            let y_max = map_y(max_val);

            // Center the line in the pixel
            let x_center = x_screen + 0.5;

            points.push(pos2(x_center, y_min));
            points.push(pos2(x_center, y_max));

            current_idx = next_idx;
        }
    }

    // Draw the Path
    let opacity_factor = if point_count > 1000 { 0.8 } else { 1.0 };

    match mode {
        TraceType::Voltage(_) => {
            // Glow
            painter.add(PathShape::line(
                points.clone(),
                Stroke::new(4.0, color.gamma_multiply(0.25 * opacity_factor)),
            ));
            // Core
            painter.add(PathShape::line(
                points,
                Stroke::new(1.5, color.gamma_multiply(opacity_factor)),
            ));
        }
        TraceType::Current(_) => {
            // Current is drawn sharper
            painter.add(PathShape::line(
                points,
                Stroke::new(1.0, color.gamma_multiply(opacity_factor)),
            ));
        }
    }
}

fn draw_cursor(
    painter: &Painter,
    ctx: &ScopeContext,
    data: &[SimStepData],
    hover_pos: Pos2,
    v_idx: Option<usize>,
    i_idx: Option<usize>,
) {
    let t_hover = ctx.x_to_t(hover_pos.x);
    let idx = match data.binary_search_by(|d| d.time.partial_cmp(&t_hover).unwrap()) {
        Ok(i) => i,
        Err(i) => {
            if i > 0 {
                i - 1
            } else {
                0
            }
        }
    };

    if let Some(step) = data.get(idx) {
        let x = ctx.t_to_x(step.time);
        painter.line_segment(
            [pos2(x, ctx.rect.top()), pos2(x, ctx.rect.bottom())],
            Stroke::new(1.0, Color32::from_white_alpha(100)),
        );

        let mut lines = Vec::new();
        lines.push((
            format!("T: {}", format_si(&[(step.time, "s")], 0.1, 2)),
            Color32::WHITE,
        ));

        if let Some(vi) = v_idx {
            let val = step.voltages.get(vi).copied().unwrap_or(0.0);
            lines.push((format!("V: {:.4}V", val), Color32::GREEN));
            painter.circle_filled(pos2(x, ctx.v_to_y(val)), 3.0, Color32::GREEN);
        }
        if let Some(ii) = i_idx {
            let val = step.currents.get(ii).copied().unwrap_or(0.0);
            lines.push((format!("I: {:.4}A", val), Color32::from_rgb(255, 100, 50)));
            painter.circle_filled(
                pos2(x, ctx.i_to_y(val)),
                3.0,
                Color32::from_rgb(255, 100, 50),
            );
        }

        let font = FontId::monospace(12.0);
        let mut y_off = 0.0;
        let box_pos = pos2(x + 10.0, hover_pos.y);

        let mut max_w: f32 = 0.0;
        for (txt, _) in &lines {
            max_w = max_w.max(
                painter
                    .layout_no_wrap(txt.clone(), font.clone(), Color32::WHITE)
                    .size()
                    .x,
            );
        }
        let box_rect =
            Rect::from_min_size(box_pos, vec2(max_w + 10.0, lines.len() as f32 * 16.0 + 5.0));

        let final_rect = if box_rect.right() > ctx.rect.right() {
            box_rect.translate(vec2(-box_rect.width() - 20.0, 0.0))
        } else {
            box_rect.translate(vec2(15.0, 0.0))
        };

        painter.rect_filled(final_rect, 3.0, Color32::from_black_alpha(240));
        painter.rect_stroke(
            final_rect,
            3.0,
            Stroke::new(1.0, Color32::GRAY),
            StrokeKind::Inside,
        );

        for (txt, col) in lines {
            painter.text(
                final_rect.min + vec2(5.0, 5.0 + y_off),
                Align2::LEFT_TOP,
                txt,
                font.clone(),
                col,
            );
            y_off += 16.0;
        }
    }
}

fn draw_legend(ui: &mut Ui, rect: Rect, has_v: bool, has_i: bool) {
    let painter = ui.painter();
    let mut pos = rect.min + vec2(50.0, 10.0);
    if has_v {
        painter.circle_filled(pos + vec2(0.0, 6.0), 3.0, Color32::GREEN);
        painter.text(
            pos + vec2(8.0, 0.0),
            Align2::LEFT_TOP,
            "Voltage",
            FontId::proportional(12.0),
            Color32::GREEN,
        );
        pos.y += 15.0;
    }
    if has_i {
        painter.circle_filled(pos + vec2(0.0, 6.0), 3.0, Color32::from_rgb(255, 100, 50));
        painter.text(
            pos + vec2(8.0, 0.0),
            Align2::LEFT_TOP,
            "Current",
            FontId::proportional(12.0),
            Color32::from_rgb(255, 100, 50),
        );
    }
}
