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
use crate::ui::components::definitions::{ComponentUIExt, SchematicElement};
use crate::ui::drawing::draw_component;
use crate::ui::{ComponentDef, GridPos, ParameterDecl, ParameterKind, VisualComponent};
use egui::{Color32, Id};

/// Generate a unique parameter name with `prefix` that doesn't clash with any
/// existing parameter name in the list.
fn next_param_name(params: &[ParameterDecl], prefix: &str) -> String {
    let mut indices: Vec<usize> = params
        .iter()
        .filter_map(|p| {
            p.name.strip_prefix(prefix)?.parse::<usize>().ok()
        })
        .collect();
    indices.sort_unstable();
    let mut next = 1;
    for idx in indices {
        if idx == next { next += 1; } else if idx > next { break; }
    }
    format!("{prefix}{next}")
}

/// After placing a pot or switch, assign its auto-param and register it.
fn register_auto_param(app: &mut CircuitApp, element: &mut SchematicElement) {
    match element {
        SchematicElement::Core(ComponentDef::Potentiometer(def)) => {
            let param_name = next_param_name(&app.state.parameters, "pot");
            def.param_name = param_name.clone();
            def.current_position = 0.5;
            app.state.parameters.push(ParameterDecl {
                name: param_name,
                default: 0.5,
                kind: ParameterKind::Slider { min: 0.0, max: 1.0 },
                auto: true,
            });
        }
        SchematicElement::Core(ComponentDef::Switch(def)) => {
            let param_name = next_param_name(&app.state.parameters, "sw");
            def.param_name = param_name.clone();
            app.state.parameters.push(ParameterDecl {
                name: param_name,
                default: 0.0,
                kind: ParameterKind::Boolean,
                auto: true,
            });
        }
        _ => {}
    }
}

pub fn handle(
    app: &mut CircuitApp,
    ui: &egui::Ui,
    ctx: &egui::Context,
    response: &egui::Response,
    painter: &egui::Painter,
    element: &SchematicElement,
    grid_pos: GridPos,
    _snap_pos: egui::Pos2,
) {
    let name = match element {
        SchematicElement::Label(_) => {
            let name = ctx
                .data(|d| {
                    d.get_temp::<String>(Id::new("label_tool_text"))
                        .unwrap_or_default()
                })
                .clone();
            name
        }
        _ => app.state.schematic.generate_next_name(element.prefix()),
    };

    let ghost_comp = VisualComponent {
        name: name.clone(),
        id: 0,
        element: element.clone(),
        pos: grid_pos,
        rotation: app.current_rotation,
    };

    // Draw Ghost
    draw_component(
        painter,
        &ghost_comp,
        |p| app.to_screen(p),
        app.zoom,
        Color32::from_white_alpha(30),
        Color32::from_white_alpha(150),
    );

    // Handle Click to Place
    if response.clicked_by(egui::PointerButton::Primary) {
        app.undo_stack.push(app.state.clone());

        let mut placed_element = element.clone();
        register_auto_param(app, &mut placed_element);

        app.state.schematic.add_component_with_name(
            placed_element,
            grid_pos,
            app.current_rotation,
            name,
        );

        app.current_rotation = 0;

        if ui.input(|i| i.modifiers.shift) {
            // Keep placing the same component
        } else {
            if matches!(element, SchematicElement::Label(_)) {
                ctx.data_mut(|d| d.remove_temp::<String>(Id::new("label_tool_text")));
            }
            app.selected_tool = Tool::Select;
        }
    }

    if response.clicked_by(egui::PointerButton::Secondary) {
        app.selected_tool = Tool::Select
    }
}
