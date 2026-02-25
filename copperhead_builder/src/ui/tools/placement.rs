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
use crate::ui::app::{CircuitApp, Tool};
use crate::ui::drawing::draw_component;
use crate::ui::{ComponentBuildData, GridPos, VisualComponent};
use egui::{Color32, Id, Pos2};

pub fn handle(
    app: &mut CircuitApp,
    ui: &egui::Ui,
    ctx: &egui::Context,
    response: &egui::Response,
    painter: &egui::Painter,
    comp_data: &ComponentBuildData,
    grid_pos: GridPos,
    snap_pos: Pos2,
) {
    let name = match comp_data {
        ComponentBuildData::Label => {
            let name = ctx
                .data(|d| {
                    d.get_temp::<String>(Id::new("label_tool_text"))
                        .unwrap_or_default()
                })
                .clone();
            name
        }
        _ => app.state.schematic.generate_next_name(&comp_data.prefix()),
    };

    // calculate size in grid units depending on the type of component
    let (size, offset) = match comp_data {
        ComponentBuildData::Resistor { .. }
        | ComponentBuildData::Capacitor { .. }
        | ComponentBuildData::Inductor { .. }
        | ComponentBuildData::Diode { .. } => (GridPos { x: 2, y: 1 }, (0., 0.)),
        ComponentBuildData::DCSource { .. }
        | ComponentBuildData::ASource { .. }
        | ComponentBuildData::AudioSource { .. } => (GridPos { x: 1, y: 2 }, (0., 0.)),

        ComponentBuildData::Ground | ComponentBuildData::Label => {
            (GridPos { x: 1, y: 1 }, (0., 0.))
        }
        ComponentBuildData::AudioProbe { .. } => (GridPos { x: 1, y: 2 }, (0., -0.75)),

        ComponentBuildData::Bjt { .. } => (GridPos { x: 2, y: 2 }, (0., 0.)),
        ComponentBuildData::Triode { .. } => (GridPos { x: 2, y: 2 }, (0., 0.)),
    };

    let rotated_size = match app.current_rotation % 4 {
        0 | 2 => size,
        1 | 3 => GridPos {
            x: size.y,
            y: size.x,
        },
        _ => unreachable!(),
    };

    let rotated_offset = match app.current_rotation % 4 {
        0 => offset,
        1 => (-offset.1, offset.0),
        2 => (-offset.0, -offset.1),
        3 => (offset.1, -offset.0),
        _ => unreachable!(),
    };

    let ghost_comp = VisualComponent {
        name: name.clone(),
        id: 0,
        component: comp_data.clone(),
        pos: grid_pos,
        size: rotated_size,
        rotation: app.current_rotation,
        offset: rotated_offset,
    };

    // Draw Ghost
    draw_component(
        &painter,
        &ghost_comp,
        |p| app.to_screen(p),
        app.zoom,
        Color32::from_white_alpha(30),
        Color32::from_white_alpha(150),
    );

    // Handle Click to Place
    if response.clicked_by(egui::PointerButton::Primary) {
        app.undo_stack.push(app.state.clone());
        app.state.schematic.add_component_with_name(
            comp_data.clone(),
            grid_pos,
            app.current_rotation,
            name,
            rotated_size,
            rotated_offset,
        );

        app.current_rotation = 0;

        if ui.input(|i| i.modifiers.shift) {
            // Keep placing the same component
        } else {
            if matches!(comp_data, ComponentBuildData::Label) {
                ctx.data_mut(|d| d.remove_temp::<String>(Id::new("label_tool_text")));
            }
            app.selected_tool = Tool::Select;
        }
    }

    if response.clicked_by(egui::PointerButton::Secondary) {
        app.selected_tool = Tool::Select
    }
}
