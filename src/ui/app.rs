use crate::circuit::Circuit;
use crate::components::ComponentDescriptor;
use crate::model::{CircuitScalar, GridPos, NodeId};
use crate::simulation::run_simulation_loop;
use crate::ui::components::oscilloscope::ScopeState;
use crate::ui::{ComponentBuildData, Netlist, Schematic, SimCommand, SimState, VisualWire};
use crossbeam::channel::{Sender, unbounded};
use egui::{Color32, Pos2, Vec2, Visuals};
use parking_lot::RwLock;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use faer::prelude::default;

#[derive(Clone)]
pub enum Tool {
    Select,
    PlaceComponent(ComponentBuildData),
    PlaceWire(Option<GridPos>), // Optionally stores the starting point. None means waiting for 1st click.
    Erase,
}

pub struct CircuitApp {
    pub schematic: Schematic,
    pub selected_tool: Tool,
    pub pan: Vec2,
    pub zoom: f32,
    /// tracks rotation for the tool being placed
    pub current_rotation: u8,
    pub editing_component_id: Option<usize>,
    pub plotting_node_voltage: Option<NodeId>,
    pub plotting_component_current: Option<usize>,
    pub simulation_time: f64, // in seconds, for how long to simulate
    pub scope_state: ScopeState,
    pub realtime_mode: bool,
    pub tx_command: Sender<SimCommand>,
    pub shared_state: Arc<RwLock<SimState>>,

    pub active_netlist: Option<Netlist>,
}

impl CircuitApp {
    pub fn new(cc: &eframe::CreationContext) -> Self {
        // Create channels and shared state
        let (tx, rx) = unbounded();
        let state = Arc::new(RwLock::new(SimState {
            history: Vec::new(),
            running: false,
            current_sample: 0,
        }));

        // Spawn simulation thread
        let state_clone = state.clone();
        std::thread::spawn(move || {
            run_simulation_loop(rx, state_clone);
        });

        cc.egui_ctx.style_mut(|style| {
            style.visuals = Visuals {
                dark_mode: true,
                window_fill: Color32::from_hex("#05050D").unwrap(),
                panel_fill: Color32::from_hex("#05050D").unwrap(),
                ..default()
            }
        });

        Self {
            schematic: Schematic::default(),
            selected_tool: Tool::Select,
            pan: Vec2::ZERO,
            zoom: 30.0, // pixels per grid unit
            current_rotation: 0,
            editing_component_id: None,
            plotting_node_voltage: None,
            plotting_component_current: None,
            scope_state: ScopeState::default(),
            realtime_mode: false,
            simulation_time: -1.0,
            tx_command: tx,
            shared_state: state,
            active_netlist: None,
        }
    }

    /// Converts a Grid Position (logical) to Screen Position (pixels)
    pub(crate) fn to_screen(&self, pos: GridPos) -> Pos2 {
        let world_x = pos.x as f32 * self.zoom;
        let world_y = pos.y as f32 * self.zoom;
        Pos2::new(world_x, world_y) + Vec2::from(self.pan)
    }

    /// Converts a Screen Position (pixels) to the nearest Grid Position (logical)
    pub(crate) fn to_grid(&self, pos: Pos2) -> GridPos {
        let local = pos - Vec2::from(self.pan);
        let gx = (local.x / self.zoom).round() as i32;
        let gy = (local.y / self.zoom).round() as i32;
        GridPos {
            x: gx as isize,
            y: gy as isize,
        }
    }

    /// Helper to transform raw float coordinates
    fn world_to_screen(&self, x: f32, y: f32) -> Pos2 {
        Pos2::new(x * self.zoom, y * self.zoom) + Vec2::from(self.pan)
    }

    /// Turns a netlist into a circuit that can be executed
    pub fn compile_netlist(&self) -> Netlist {
        let mut instructions = Vec::new();
        let mut point_map: HashMap<GridPos, usize> = HashMap::new();
        let mut next_point_id = 0;

        let effective_wires = self.get_normalized_wires();

        // Register Points
        for wire in &effective_wires {
            // Use effective_wires here
            for pos in [wire.start, wire.end] {
                point_map.entry(pos).or_insert_with(|| {
                    let i = next_point_id;
                    next_point_id += 1;
                    i
                });
            }
        }

        for comp in &self.schematic.components {
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

        // Identify Grounded Roots
        let mut grounded_roots = HashSet::new();

        for comp in &self.schematic.components {
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
                    println!("WE GOT ZERO");
                    NodeId(0)
                } else {
                    let id = NodeId(next_node_id);
                    println!("ID ASSIGNED: {}", id.0);
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
        for comp in &self.schematic.components {
            // Skip the Ground component in instructions, it's not a circuit element,
            // it's just a constraint we applied above.
            if let ComponentBuildData::Ground = comp.component {
                continue;
            }

            let pins = comp.get_pin_locations();
            if pins.len() < 2 {
                continue;
            }

            let node_a = *final_node_map.get(&pins[0]).expect("Pin not mapped");
            let node_b = *final_node_map.get(&pins[1]).expect("Pin not mapped");

            let (a, b) = (node_a.0, node_b.0);

            let descriptor = match comp.component {
                ComponentBuildData::Resistor { resistance } => ComponentDescriptor::Resistor {
                    a,
                    b,
                    ohms: resistance,
                },
                ComponentBuildData::DCSource { voltage } => ComponentDescriptor::DCSource {
                    pos: a,
                    neg: b,
                    volts: voltage,
                },
                ComponentBuildData::ASource {
                    amplitude,
                    frequency,
                } => ComponentDescriptor::ASource {
                    pos: a,
                    neg: b,
                    amp: amplitude,
                    freq: frequency,
                },
                ComponentBuildData::Capacitor { capacitance } => {
                    ComponentDescriptor::Capacitor { a, b, capacitance }
                }
                ComponentBuildData::Inductor { inductance } => {
                    ComponentDescriptor::Inductor { a, b, inductance }
                }
                ComponentBuildData::Diode { model } => {
                    let (
                        saturation_current,
                        emission_coefficient,
                        series_resistance,
                        cjo,
                        m,
                        transit_time,
                    ) = model.parameters();

                    ComponentDescriptor::Diode {
                        a,
                        b,
                        saturation_current,
                        emission_coefficient,
                        series_resistance,
                        cjo,
                        m,
                        transit_time,
                    }
                }
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

    fn get_normalized_wires(&self) -> Vec<VisualWire> {
        let mut final_wires = self.schematic.wires.clone();

        // 1Collect all "interesting" points that must create nodes.
        // These are component pins and endpoints of all wires.
        let mut points_of_interest: HashSet<GridPos> = HashSet::new();

        for comp in &self.schematic.components {
            for pin in comp.get_pin_locations() {
                points_of_interest.insert(pin);
            }
        }

        // We also need wire endpoints to split other wires
        for wire in &self.schematic.wires {
            points_of_interest.insert(wire.start);
            points_of_interest.insert(wire.end);
        }

        // Iterative splitting
        // We keep looping until no splits happen (to handle cases where a split creates new points)
        let mut changed = true;
        while changed {
            changed = false;
            let mut new_wire_list = Vec::new();

            for wire in &final_wires {
                // Find a point that splits this wire
                let mut split_point: Option<GridPos> = None;

                for &pt in &points_of_interest {
                    // If point is on the wire, but is not the start or end
                    if wire.contains(pt) && pt != wire.start && pt != wire.end {
                        split_point = Some(pt);
                        break;
                    }
                }

                if let Some(pt) = split_point {
                    // Split the wire
                    new_wire_list.push(VisualWire {
                        start: wire.start,
                        end: pt,
                    });
                    new_wire_list.push(VisualWire {
                        start: pt,
                        end: wire.end,
                    });
                    changed = true;
                } else {
                    new_wire_list.push(wire.clone());
                }
            }
            final_wires = new_wire_list;
        }

        final_wires
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
