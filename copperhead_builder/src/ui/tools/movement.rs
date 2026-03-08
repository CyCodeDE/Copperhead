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

use crate::ui::app::{CircuitApp, DragState, Tool};
use egui::{Color32, CursorIcon, PointerButton, Pos2, Rect, Stroke, StrokeKind, Vec2};
use crate::ui::drawing::check_line_rect_intersection;
use crate::ui::GridPos;

pub fn handle(
    app: &mut CircuitApp,
    ui: &egui::Ui,
    ctx: &egui::Context,
    response: &egui::Response,
    painter: &egui::Painter,
    mouse_pos: Pos2,
) {
    let move_id = response.id.with("move_tool_start");
    let escape_pressed = ctx.input(|i| i.key_pressed(egui::Key::Escape));
    let right_clicked = response.clicked_by(PointerButton::Secondary);
    let left_clicked = response.clicked_by(PointerButton::Primary);
    let ctrl_r_pressed = ctx.input(|i| i.modifiers.command && i.key_pressed(egui::Key::R));

    let mut current_drag_state = std::mem::replace(&mut app.drag_state, DragState::None);
    let mut next_drag_state = None;

    if right_clicked || escape_pressed {
        if let DragState::Moving { components, wires, .. } = &current_drag_state {
            for (id, orig_pos, orig_rot) in components {
                if let Some(comp) = app.state.schematic.components.iter_mut().find(|c| c.id == *id) {
                    comp.pos = *orig_pos;
                    comp.rotation = *orig_rot;
                }
            }
            for (idx, orig_wire) in wires {
                if let Some(wire) = app.state.schematic.wires.get_mut(*idx) {
                    *wire = orig_wire.clone();
                }
            }
        }

        ctx.data_mut(|d| d.remove::<Pos2>(move_id));
        app.selected_tool = Tool::Select;
        ctx.request_repaint();
        return;
    }

    match &mut current_drag_state {
        DragState::None => {
            ctx.set_cursor_icon(CursorIcon::Grab);

            if response.drag_started_by(PointerButton::Primary) {
                ctx.data_mut(|d| d.insert_temp(move_id, mouse_pos));
            }

            let mut to_move_comps = Vec::new();
            let mut to_move_wires = Vec::new();
            let mut move_started = false;

            if left_clicked && ctx.data(|d| d.get_temp::<Pos2>(move_id)).is_none() {
                let click_tolerance = 5.0;
                let selection_rect = Rect::from_center_size(mouse_pos, Vec2::splat(click_tolerance));

                for comp in &app.state.schematic.components {
                    let comp_screen_pos = app.to_screen(comp.pos);
                    let size = Vec2::new(comp.size.0 as f32, comp.size.1 as f32) * app.zoom;
                    let comp_rect = Rect::from_center_size(comp_screen_pos, size).translate(Vec2::new(
                        comp.offset.0 * app.zoom,
                        comp.offset.1 * app.zoom,
                    ));

                    if selection_rect.intersects(comp_rect) {
                        to_move_comps.push((comp.id, comp.pos, comp.rotation));
                        break;
                    }
                }

                if to_move_comps.is_empty() {
                    for (i, wire) in app.state.schematic.wires.iter().enumerate() {
                        let p1 = app.to_screen(wire.start);
                        let p2 = app.to_screen(wire.end);
                        let wire_rect = Rect::from_two_pos(p1, p2).expand(click_tolerance);

                        if selection_rect.intersects(wire_rect) {
                            to_move_wires.push((i, wire.clone()));
                            break;
                        }
                    }
                }

                if !to_move_comps.is_empty() || !to_move_wires.is_empty() {
                    move_started = true;
                }
            }

            if let Some(start_pos) = ctx.data(|d| d.get_temp::<Pos2>(move_id)) {
                let selection_rect = Rect::from_two_pos(start_pos, mouse_pos);

                painter.rect(
                    selection_rect,
                    5.0,
                    Color32::from_rgba_unmultiplied(1, 135, 200, 40),
                    Stroke::new(1.0, Color32::WHITE),
                    StrokeKind::Inside,
                );

                if response.drag_stopped_by(PointerButton::Primary) {
                    ctx.data_mut(|d| d.remove::<Pos2>(move_id));

                    for comp in &app.state.schematic.components {
                        let comp_screen_pos = app.to_screen(comp.pos);
                        let size = Vec2::new(comp.size.0 as f32, comp.size.1 as f32) * app.zoom;
                        let comp_rect = Rect::from_center_size(comp_screen_pos, size).translate(Vec2::new(
                            comp.offset.0 * app.zoom,
                            comp.offset.1 * app.zoom,
                        ));

                        if selection_rect.intersects(comp_rect) {
                            to_move_comps.push((comp.id, comp.pos, comp.rotation));
                        }
                    }

                    for (i, wire) in app.state.schematic.wires.iter().enumerate() {
                        let p1 = app.to_screen(wire.start);
                        let p2 = app.to_screen(wire.end);
                        let wire_rect = Rect::from_two_pos(p1, p2);
                        let hit_rect = wire_rect.expand(if selection_rect.width() < 5.0 { 5.0 } else { 0.0 });

                        // Note: Ensure check_line_rect_intersection is in scope
                        if selection_rect.intersects(hit_rect) {
                            to_move_wires.push((i, wire.clone()));
                        }
                    }

                    if !to_move_comps.is_empty() || !to_move_wires.is_empty() {
                        move_started = true;
                    }
                }
            }

            if move_started {
                let first_x = to_move_comps.first().map(|c| c.1.x).unwrap_or_else(|| to_move_wires.first().unwrap().1.start.x);
                let first_y = to_move_comps.first().map(|c| c.1.y).unwrap_or_else(|| to_move_wires.first().unwrap().1.start.y);

                let mut min_x = first_x;
                let mut max_x = first_x;
                let mut min_y = first_y;
                let mut max_y = first_y;

                let mut update_bounds = |x, y| {
                    if x < min_x { min_x = x; }
                    if x > max_x { max_x = x; }
                    if y < min_y { min_y = y; }
                    if y > max_y { max_y = y; }
                };

                for (_, pos, _) in &to_move_comps {
                    update_bounds(pos.x, pos.y);
                }

                for (_, wire) in &to_move_wires {
                    update_bounds(wire.start.x, wire.start.y);
                    update_bounds(wire.end.x, wire.end.y);
                }

                // Calculate the true center of the selected area
                let center_grid = GridPos {
                    x: (min_x + max_x) / 2,
                    y: (min_y + max_y) / 2,
                };

                next_drag_state = Some(DragState::Moving {
                    components: to_move_comps,
                    wires: to_move_wires,
                    reference_grid: center_grid,
                    rotation_steps: 0,
                });
            }
        }

        DragState::Moving { components, wires, reference_grid, rotation_steps } => {
            ctx.set_cursor_icon(CursorIcon::Grabbing);

            if ctrl_r_pressed {
                *rotation_steps = (*rotation_steps + 1) % 4;
            }

            if let Some(hover_pos) = response.hover_pos() {
                let current_mouse_grid = app.to_grid(hover_pos);
                let delta_x = current_mouse_grid.x - reference_grid.x;
                let delta_y = current_mouse_grid.y - reference_grid.y;
                let current_rot_steps = *rotation_steps;
                let pivot = *reference_grid;

                let rotate_point = |p: GridPos, steps: u8| -> GridPos {
                    let mut dx = p.x - pivot.x;
                    let mut dy = p.y - pivot.y;
                    for _ in 0..steps {
                        let temp = dx;
                        dx = -dy;
                        dy = temp;
                    }
                    GridPos {
                        x: pivot.x + dx,
                        y: pivot.y + dy,
                    }
                };

                for (id, orig_pos, orig_rot) in components.iter() {
                    if let Some(comp) = app.state.schematic.components.iter_mut().find(|c| c.id == *id) {
                        let rotated_pos = rotate_point(*orig_pos, current_rot_steps);
                        comp.pos = GridPos {
                            x: rotated_pos.x + delta_x,
                            y: rotated_pos.y + delta_y,
                        };
                        comp.rotation = (*orig_rot + current_rot_steps) % 4;
                    }
                }

                for (idx, orig_wire) in wires.iter() {
                    if let Some(wire) = app.state.schematic.wires.get_mut(*idx) {
                        let rotated_start = rotate_point(orig_wire.start, current_rot_steps);
                        let rotated_end = rotate_point(orig_wire.end, current_rot_steps);

                        wire.start = GridPos {
                            x: rotated_start.x + delta_x,
                            y: rotated_start.y + delta_y,
                        };
                        wire.end = GridPos {
                            x: rotated_end.x + delta_x,
                            y: rotated_end.y + delta_y,
                        };
                    }
                }
            }

            if left_clicked {
                next_drag_state = Some(DragState::None);
            }

            ctx.request_repaint();
        }
    }

    app.drag_state = next_drag_state.unwrap_or(current_drag_state);
}