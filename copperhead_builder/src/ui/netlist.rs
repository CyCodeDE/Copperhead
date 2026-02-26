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
use crate::ui::{ComponentBuildData, GridPos, Netlist};
use copperhead_core::descriptor::ComponentDescriptor;
use copperhead_core::model::NodeId;
use std::collections::{BTreeMap, HashMap, HashSet};

/// Turns a netlist into a circuit that can be executed
pub fn compile_netlist(app: &CircuitApp) -> Netlist {
    let mut instructions = Vec::new();
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
        if matches!(&comp.component, ComponentBuildData::Label) {
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
        if let ComponentBuildData::Ground = comp.component {
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
        match comp.component {
            ComponentBuildData::Ground => continue,
            ComponentBuildData::Label => continue,
            _ => {}
        }

        let pins = comp.get_pin_locations();

        let node_a = pins.get(0).and_then(|p| final_node_map.get(p)).cloned();
        let node_b = pins.get(1).and_then(|p| final_node_map.get(p)).cloned();
        let node_c = pins.get(2).and_then(|p| final_node_map.get(p)).cloned();

        let c = node_c.map(|n| n.0);

        let descriptor = match comp.component.clone() {
            ComponentBuildData::Resistor { resistance } => ComponentDescriptor::Resistor {
                a: node_a.expect("A is not mapped").0,
                b: node_b.expect("B is not mapped").0,
                ohms: resistance,
            },
            ComponentBuildData::DCSource { voltage } => ComponentDescriptor::DCSource {
                pos: node_a.expect("A is not mapped").0,
                neg: node_b.expect("B is not mapped").0,
                volts: voltage,
            },
            ComponentBuildData::ASource {
                amplitude,
                frequency,
            } => ComponentDescriptor::ASource {
                pos: node_a.expect("A is not mapped").0,
                neg: node_b.expect("B is not mapped").0,
                amp: amplitude,
                freq: frequency,
            },
            ComponentBuildData::AudioSource { path } => ComponentDescriptor::AudioSource {
                pos: node_a.expect("A is not mapped").0,
                neg: node_b.expect("B is not mapped").0,
                file_path: path,
            },
            ComponentBuildData::Capacitor { capacitance, esr } => ComponentDescriptor::Capacitor {
                a: node_a.expect("A is not mapped").0,
                b: node_b.expect("B is not mapped").0,
                capacitance,
                esr,
            },
            ComponentBuildData::Inductor {
                inductance,
                series_resistance,
            } => ComponentDescriptor::Inductor {
                a: node_a.expect("A is not mapped").0,
                b: node_b.expect("B is not mapped").0,
                inductance,
                series_resistance,
            },
            ComponentBuildData::Diode { model } => {
                let (
                    saturation_current,
                    emission_coefficient,
                    series_resistance,
                    cjo,
                    m,
                    transit_time,
                    breakdown_voltage,
                    breakdown_current,
                ) = model.parameters();

                ComponentDescriptor::Diode {
                    a: node_a.expect("A is not mapped").0,
                    b: node_b.expect("B is not mapped").0,
                    saturation_current,
                    emission_coefficient,
                    series_resistance,
                    cjo,
                    m,
                    transit_time,
                    breakdown_voltage,
                    breakdown_current,
                }
            }
            ComponentBuildData::Bjt { model } => {
                assert!(c.is_some(), "BJT must have 3 pins");
                let (is, bf, br, vt, vaf, var, rc, rb, re, polarity) = model.parameters();

                ComponentDescriptor::Bjt {
                    c: node_a.expect("A is not mapped").0,
                    b: node_b.expect("B is not mapped").0,
                    e: c.unwrap(),
                    saturation_current: is,
                    beta_f: bf,
                    beta_r: br,
                    vt,
                    vaf,
                    var,
                    rc,
                    rb,
                    re,
                    polarity,
                }
            }
            ComponentBuildData::Triode { model } => {
                assert!(c.is_some(), "Triode must have 3 pins");
                let (mu, ex, kg1, kp, kvb, rgi, cgp, cgk, cpk, i_s, vt) = model.parameters();

                ComponentDescriptor::Triode {
                    p: node_a.expect("A is not mapped").0,
                    g: node_b.expect("B is not mapped").0,
                    c: c.unwrap(),
                    mu,
                    ex,
                    kg1,
                    kp,
                    kvb,
                    rgi,
                    cgp,
                    cgk,
                    cpk,
                    i_s,
                    vt,
                }
            }
            ComponentBuildData::AudioProbe { path } => ComponentDescriptor::AudioProbe {
                file_path: path,
                node: node_a.expect("A is not mapped").0,
            },
            _ => continue,
        };

        component_map.insert(comp.id, sim_comp_index);
        sim_comp_index += 1;

        instructions.push(descriptor);
    }

    // Error check: Did we have a ground?
    if !root_to_node_id.values().any(|n| n.0 == 0) {
        println!(
            "WARNING: Circuit has no ground reference (Node 0). Simulation may drift or fail."
        );
    }

    Netlist {
        instructions,
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
