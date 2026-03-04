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
use crate::ui::components::definitions::SchematicElement;
use crate::ui::{GridPos, Netlist, NetlistEntry};
use copperhead_core::model::NodeId;
use std::collections::{BTreeMap, HashMap, HashSet};

/// Turns a netlist into a circuit that can be executed
pub fn compile_netlist(app: &CircuitApp) -> Netlist {
    let mut entries = Vec::new();
    let mut point_map: BTreeMap<GridPos, usize> = BTreeMap::new();
    let mut next_point_id = 0;

    let effective_wires = app.get_normalized_wires();

    // Register Points
    for wire in &effective_wires {
        for pos in [wire.start, wire.end] {
            point_map.entry(pos).or_insert_with(|| {
                let i = next_point_id;
                next_point_id += 1;
                i
            });
        }
    }

    for comp in &app.state.schematic.components {
        for pin_pos in comp.get_pin_locations() {
            point_map.entry(pin_pos).or_insert_with(|| {
                let i = next_point_id;
                next_point_id += 1;
                i
            });
        }
    }

    // Build DSU
    let mut dsu = DisjointSet::new(point_map.len());

    for wire in &effective_wires {
        let start_id = point_map[&wire.start];
        let end_id = point_map[&wire.end];
        dsu.union(start_id, end_id);
    }

    let mut label_groups: BTreeMap<String, Vec<usize>> = BTreeMap::new();

    for comp in &app.state.schematic.components {
        if matches!(&comp.element, SchematicElement::Label(_)) {
            let name = comp.name.clone();

            if let Some(pos) = comp.get_pin_locations().first() {
                if let Some(&pt_id) = point_map.get(pos) {
                    label_groups.entry(name).or_default().push(pt_id);
                }
            }
        }
    }

    for points in label_groups.values() {
        if let Some(&first) = points.first() {
            for &other in points.iter().skip(1) {
                dsu.union(first, other);
            }
        }
    }

    // Identify Grounded Roots
    let mut grounded_roots = HashSet::new();

    for comp in &app.state.schematic.components {
        if let SchematicElement::Ground(_) = comp.element {
            if let Some(pin_pos) = comp.get_pin_locations().first() {
                if let Some(&pt_id) = point_map.get(pin_pos) {
                    grounded_roots.insert(dsu.find_p(pt_id));
                }
            }
        }
    }

    // Normalize Node IDs
    let mut final_node_map: HashMap<GridPos, NodeId> = HashMap::new();
    let mut root_to_node_id: HashMap<usize, NodeId> = HashMap::new();
    let mut next_node_id = 1;

    for (pos, &point_id) in &point_map {
        let root = dsu.find_p(point_id);

        // Check if we already assigned a NodeID to this electrical net
        let node_id = if let Some(&id) = root_to_node_id.get(&root) {
            id
        } else {
            // If this net is connected to a Ground component, it is Node 0
            let new_id = if grounded_roots.contains(&root) {
                NodeId(0)
            } else {
                let id = NodeId(next_node_id);
                next_node_id += 1;
                id
            };
            root_to_node_id.insert(root, new_id);
            new_id
        };

        final_node_map.insert(*pos, node_id);
    }

    let mut component_map: HashMap<usize, usize> = HashMap::new();
    let mut sim_comp_index = 0;

    // Generate instruction
    for comp in &app.state.schematic.components {
        // Skip the Ground and Label components in instructions, it's not a circuit element,
        // it's just a constraint we applied above.
        let core_def = match &comp.element {
            SchematicElement::Ground(_) => continue,
            SchematicElement::Label(_) => continue,
            SchematicElement::Core(def) => def,
        };

        let pins = comp.get_pin_locations();

        let resolved_nodes: Vec<NodeId> = pins
            .iter()
            .map(|pos| {
                final_node_map.get(pos).cloned().unwrap_or_else(|| {
                    panic!("Pin at position {:?} was not registered in node map", pos)
                })
            })
            .collect();

        let entry = NetlistEntry {
            component: core_def.clone(),
            nodes: resolved_nodes,
        };

        component_map.insert(comp.id, sim_comp_index);
        sim_comp_index += 1;

        entries.push(entry);
    }

    // Error check: Did we have a ground?
    if !root_to_node_id.values().any(|n| n.0 == 0) {
        println!(
            "WARNING: Circuit has no ground reference (Node 0). Simulation may drift or fail."
        );
    }

    Netlist {
        entries,
        node_map: final_node_map,
        component_map,
    }
}

struct DisjointSet {
    parent: Vec<usize>,
}

impl DisjointSet {
    fn new(size: usize) -> Self {
        Self {
            parent: (0..size).collect(),
        }
    }
    fn find_p(&mut self, i: usize) -> usize {
        if self.parent[i] != i {
            self.parent[i] = self.find_p(self.parent[i]);
        }
        self.parent[i]
    }
    fn union(&mut self, i: usize, j: usize) {
        let root_i = self.find_p(i);
        let root_j = self.find_p(j);
        if root_i != root_j {
            self.parent[root_j] = root_i;
        }
    }
}
