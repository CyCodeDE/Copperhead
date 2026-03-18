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
use crate::ui::app::CircuitApp;
use crate::ui::components::definitions::{ComponentUIExt, ParameterType};

pub fn show(app: &mut CircuitApp, ui: &mut egui::Ui) {
    let tx = &app.tx_command;
    let running = app.sim_state.running;
    let active_netlist = app.active_netlist.as_ref();
    let components = &mut app.state.schematic.components;

    let scroll_area = egui::ScrollArea::vertical();
    scroll_area.show(ui, |ui| {
        components.iter_mut().for_each(|c| {
            let id = if let Some(netlist) = active_netlist {
                netlist.component_map.get(&c.id).copied()
            } else {
                None
            };

            c.element
                .draw_property_panel(tx, ui, id, running, c.name.as_str());
        });
    });
}
