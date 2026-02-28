/*
 * Copyright (c) 2026-2026 CyCode and the Copperhead contributors
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

pub mod app;
mod app_state;
mod components;
mod drawing;
mod netlist;
mod tools;
pub mod ui;
pub mod util;

use copperhead_core::components::ComponentProbe;
use copperhead_core::components::diode::DiodeModel;
use copperhead_core::components::transistor::bjt::BjtModel;
use copperhead_core::components::triode::TriodeModel;
use copperhead_core::descriptor::ComponentDescriptor;
use copperhead_core::model::{NodeId, SimStepData};
use egui::{Color32, Pos2, Vec2};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ops::{Add, Sub};
use std::path::PathBuf;
use copperhead_core::components::pentode::PentodeModel;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Ord, PartialOrd)]
pub struct GridPos {
    pub x: isize,
    pub y: isize,
}

impl GridPos {
    pub(crate) fn to_vec2(&self) -> Vec2 {
        Vec2::new(self.x as f32, self.y as f32)
    }
}

impl GridPos {
    pub(crate) fn length(&self) -> isize {
        (self.x.abs() + self.y.abs())
    }
}

impl Sub for GridPos {
    type Output = Vec2;

    fn sub(self, rhs: Self) -> Self::Output {
        Vec2::new((self.x - rhs.x) as f32, (self.y - rhs.y) as f32)
    }
}

impl Add for GridPos {
    type Output = GridPos;

    fn add(self, other: GridPos) -> GridPos {
        GridPos {
            x: self.x + other.x,
            y: self.y + other.y,
        }
    }
}

impl Into<Pos2> for GridPos {
    fn into(self) -> Pos2 {
        Pos2::new(self.x as f32, self.y as f32)
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum ComponentBuildData {
    Resistor {
        resistance: f64,
    },
    Capacitor {
        capacitance: f64,
        esr: f64,
    },
    DCSource {
        voltage: f64,
    },
    ASource {
        amplitude: f64,
        frequency: f64,
    },
    AudioSource {
        path: PathBuf,
    },
    Inductor {
        inductance: f64,
        series_resistance: f64,
    },
    Diode {
        model: DiodeModel,
    },
    Bjt {
        model: BjtModel,
    },
    Triode {
        model: TriodeModel,
    },
    Pentode {
        model: PentodeModel,
    },
    AudioProbe {
        path: PathBuf,
    },
    Label,
    Ground,
}

impl ComponentBuildData {
    pub fn prefix(&self) -> &'static str {
        match self {
            Self::Resistor { .. } => "R",
            Self::Capacitor { .. } => "C",
            Self::Inductor { .. } => "L",
            Self::DCSource { .. } | Self::ASource { .. } => "V",
            Self::AudioSource { .. } => "WAV",
            Self::Diode { .. } => "D",
            Self::Bjt { .. } => "Q",
            Self::AudioProbe { .. } => "Probe",
            Self::Label { .. } => "",
            Self::Triode { .. } => "T", // Just "T" for Triode to avoid confusion with "V" for voltage sources
            Self::Pentode { .. } => "P",
            Self::Ground => "",
        }
    }
}

/// Represents a component on the grid
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct VisualComponent {
    pub id: usize, // UID to map to the solver
    pub name: String,
    pub component: ComponentBuildData,
    pub pos: GridPos,
    pub size: GridPos, // width and height in grid units at rotation 0, rotation has to be applied beforehand
    pub rotation: u8,  // 0, 1, 2, 3 (90 degree steps)
    pub offset: (f32, f32), // Tells the UI how to offset the size for the bounding box
}

impl VisualComponent {
    /// Returns the Grid coordinates of the component's ports (A, B, Pos, Neg, Grid, Plate, etc.)
    /// based on position and rotation
    /// Order is important: [NodeA/Pos, NodeB/Neg, ...]
    pub fn get_pin_locations(&self) -> Vec<GridPos> {
        // Define local offsets for a generic 2-port component (e.g. width 2 units)
        // Left pin: (-1, 0), Right pin: (1, 0)
        // ATTENTION: These order needs to match the order of ports() in the Component implementation or else the mapping of UI pins to simulation nodes will be wrong!
        let local_pins = match &self.component {
            ComponentBuildData::Ground => vec![(0, 0)],       // 1 Pin
            ComponentBuildData::Label { .. } => vec![(0, 0)], // 1 Pin (for positioning)
            ComponentBuildData::AudioProbe { .. } => vec![(0, 0)], // 1 Pin (for positioning)
            ComponentBuildData::Resistor { .. } => vec![(-1, 0), (1, 0)], // 2 Pins -> A and B
            ComponentBuildData::DCSource { .. } => vec![(0, -1), (0, 1)], // 2 Pins (Top, Bottom) -> Pos and Neg
            ComponentBuildData::ASource { .. } => vec![(0, -1), (0, 1)], // 2 Pins (Top, Bottom) -> Pos and Neg
            ComponentBuildData::AudioSource { .. } => vec![(0, -1), (0, 1)], // 2 Pins (Top, Bottom) -> Pos and Neg
            ComponentBuildData::Capacitor { .. } => vec![(-1, 0), (1, 0)],   // 2 Pins -> A and B
            ComponentBuildData::Inductor { .. } => vec![(-1, 0), (1, 0)],    // 2 Pins -> A and B
            ComponentBuildData::Diode { .. } => vec![(-1, 0), (1, 0)], // 2 Pins (Anode, Cathode) -> A and B
            ComponentBuildData::Bjt { model } => match model.polarity() {
                true => vec![(1, -1), (-1, 0), (1, 1)], // 3 Pins (Collector, Base, Emitter) -> C, B, E   | NPN
                false => vec![(1, 1), (-1, 0), (1, -1)], // 3 Pins (Collector, Base, Emitter) -> C, B, E  | PNP
            },
            ComponentBuildData::Triode { .. } => vec![(0, -1), (-1, 0), (0, 1)], // 3 Pins (Plate, Grid, Cathode) -> P, G, K
            ComponentBuildData::Pentode { .. } => vec![(0, -2), (-1, 0), (1, -1), (0, 1)], // 4 Pins (Plate, Control Grid, Screen Grid, Cathode) -> P, G1, G2, K
        };

        local_pins
            .iter()
            .map(|(lx, ly)| {
                // Apply Rotation (90 degree steps)
                let (rx, ry) = match self.rotation % 4 {
                    0 => (*lx, *ly),
                    1 => (-*ly, *lx),
                    2 => (-*lx, -*ly),
                    3 => (*ly, -*lx),
                    _ => unreachable!(),
                };

                GridPos {
                    x: self.pos.x + rx,
                    y: self.pos.y + ry,
                }
            })
            .collect()
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct VisualWire {
    pub start: GridPos,
    pub end: GridPos,
}

impl VisualWire {
    // Check if a point lies on the wire segment (inclusive)
    pub fn contains(&self, p: GridPos) -> bool {
        // Vertical wire
        if self.start.x == self.end.x {
            return p.x == self.start.x
                && p.y >= self.start.y.min(self.end.y)
                && p.y <= self.start.y.max(self.end.y);
        }
        // Horizontal wire
        if self.start.y == self.end.y {
            return p.y == self.start.y
                && p.x >= self.start.x.min(self.end.x)
                && p.x <= self.start.x.max(self.end.x);
        }

        false
    }
}

#[derive(Serialize, Deserialize, Clone, PartialEq)]
pub struct Schematic {
    pub components: Vec<VisualComponent>,
    pub wires: Vec<VisualWire>,
    pub next_component_id: usize,
}

impl Default for Schematic {
    fn default() -> Self {
        Schematic {
            components: Vec::new(),
            wires: Vec::new(),
            next_component_id: 1,
        }
    }
}

impl Schematic {
    fn generate_next_name(&self, prefix: &str) -> String {
        let mut indices: Vec<usize> = self
            .components
            .iter()
            .filter_map(|c| {
                if c.name.starts_with(prefix) && c.name.len() > prefix.len() {
                    // Check for standard format "{prefix}{number}"
                    let suffix = &c.name[prefix.len()..];
                    if let Ok(num) = suffix.parse::<usize>() {
                        Some(num)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect();

        indices.sort_unstable();

        // Find the lowest unused positive integer
        let mut next_idx = 1;
        for idx in indices {
            if idx == next_idx {
                next_idx += 1;
            } else if idx > next_idx {
                break;
            }
        }
        format!("{}{}", prefix, next_idx)
    }

    pub fn add_component(
        &mut self,
        data: ComponentBuildData,
        pos: GridPos,
        rotation: u8,
        size: GridPos,
        offset: (f32, f32),
    ) {
        let name = self.generate_next_name(data.prefix());
        self.components.push(VisualComponent {
            id: self.next_component_id,
            name,
            component: data,
            size,
            pos,
            rotation,
            offset,
        });
        self.next_component_id += 1;
    }

    pub fn add_component_with_name(
        &mut self,
        data: ComponentBuildData,
        pos: GridPos,
        rotation: u8,
        name: String,
        size: GridPos,
        offset: (f32, f32),
    ) {
        self.components.push(VisualComponent {
            id: self.next_component_id,
            name,
            component: data,
            pos,
            size,
            rotation,
            offset,
        });
        self.next_component_id += 1;
    }

    pub fn remove_component(&mut self, component_id: usize) {
        self.components.retain(|comp| comp.id != component_id);
    }

    pub fn recenter_schematic(&mut self) {
        if self.components.is_empty() && self.wires.is_empty() {
            return;
        }

        let mut min_x = i64::MAX;
        let mut max_x = i64::MIN;
        let mut min_y = i64::MAX;
        let mut max_y = i64::MIN;

        // Helper closure to expand the bounding box
        let mut include_point = |x: i64, y: i64| {
            if x < min_x {
                min_x = x;
            }
            if x > max_x {
                max_x = x;
            }
            if y < min_y {
                min_y = y;
            }
            if y > max_y {
                max_y = y;
            }
        };

        for comp in &self.components {
            // Include the top-left anchor
            include_point(comp.pos.x as i64, comp.pos.y as i64);

            // Include the bottom-right corner based on size
            include_point(
                (comp.pos.x + comp.size.x) as i64,
                (comp.pos.y + comp.size.y) as i64,
            );
        }

        for wire in &self.wires {
            include_point(wire.start.x as i64, wire.start.y as i64);
            include_point(wire.end.x as i64, wire.end.y as i64);
        }

        // Determine the center of that bounding box
        let center_x = (min_x + max_x) / 2;
        let center_y = (min_y + max_y) / 2;

        if center_x == 0 && center_y == 0 {
            return;
        }

        // Apply the offset to every element
        for comp in &mut self.components {
            comp.pos.x -= center_x as isize;
            comp.pos.y -= center_y as isize;
        }

        for wire in &mut self.wires {
            wire.start.x -= center_x as isize;
            wire.start.y -= center_y as isize;
            wire.end.x -= center_x as isize;
            wire.end.y -= center_y as isize;
        }
    }
}

pub enum SimCommand {
    LoadCircuit(Netlist),
    Pause,
    Resume,
    /// sets how long to simulate (in seconds)
    SetRunTime(f64),
    SetRealtime(bool),
    /// Update the value of a component in the simulation. The ComponentType must match the existing component type.
    UpdateValue {
        component_id: usize,
        updated: ComponentBuildData,
    },
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Netlist {
    /// A list of component ready to be built into the simulation
    pub instructions: Vec<ComponentDescriptor>,
    /// Maps Grid Position -> Node ID in the simulation
    pub node_map: HashMap<GridPos, NodeId>,
    /// Maps UI Component ID (usize) -> Simulation Component Index (usize)
    pub component_map: HashMap<usize, usize>,
}

pub struct SimState {
    pub history: Vec<SimStepData>,
    pub running: bool,
    pub current_sample: usize,
    pub metadata: Option<CircuitMetadata>,
    pub lookup_map: CircuitDataMap,
}

pub struct CircuitMetadata {
    pub components: Vec<ComponentMetadata>,
}

#[derive(Clone, Debug)]
pub struct ComponentMetadata {
    pub id: usize,
    pub probe_definitions: Vec<ComponentProbe>,
    pub num_terminals: usize, // Derived from ports().len()
}

struct ComponentDataLocation {
    current_start_idx: usize,
    current_count: usize,
    observable_start_idx: usize,
    observable_count: usize,
    probe_names: Vec<String>,
}

// Map internal Component index -> Location Info
type CircuitDataMap = HashMap<usize, ComponentDataLocation>;

fn handle_circuit_loaded(meta: &CircuitMetadata) -> CircuitDataMap {
    let mut map = HashMap::new();

    let mut current_offset = 0;
    let mut obs_offset = 0;

    for comp in &meta.components {
        let loc = ComponentDataLocation {
            current_start_idx: current_offset,
            current_count: comp.num_terminals,
            observable_start_idx: obs_offset,
            observable_count: comp.probe_definitions.len(),
            probe_names: comp
                .probe_definitions
                .iter()
                .map(|p| p.name.clone())
                .collect(),
        };

        map.insert(comp.id, loc);

        // Increment offsets for the next component
        current_offset += comp.num_terminals;
        obs_offset += comp.probe_definitions.len();
    }

    map
}

impl SimState {
    /// Helper to get the latest voltage for a specific node
    pub fn get_latest_voltage(&self, node_id: NodeId) -> Option<f64> {
        self.history
            .last()
            .and_then(|step| step.voltages.get(node_id.0).copied())
    }

    /// Helper to get the latest current for a specific component index
    pub fn get_latest_current(&self, comp_idx: usize) -> Option<f64> {
        self.history
            .last()
            .and_then(|step| step.currents.get(comp_idx).copied())
    }

    pub fn get_history_series(
        &self,
        comp_id: usize,
        data_type: CircuitSelection,
        local_index: usize,
    ) -> Vec<(f64, f64)> {
        let mut series = Vec::with_capacity(self.history.len());

        // Look up where this component's data lives in the flattened arrays
        // only if the component is not a voltage probe
        let loc: Option<&ComponentDataLocation> = self.lookup_map.get(&comp_id);

        // Iterate through history and grab the specific value
        for step in &self.history {
            let value = match data_type {
                CircuitSelection::Current => {
                    // Calculate absolute index in the flattened 'currents' vec
                    let loc_unwrapped = loc
                        .expect("Component ID not found in lookup map. Is this a voltage probe?");
                    let global_idx = loc_unwrapped.current_start_idx + local_index;
                    step.currents.get(global_idx).copied().unwrap_or(0.0)
                }
                CircuitSelection::Observable => {
                    // Calculate absolute index in the flattened 'observables' vec
                    let loc_unwrapped = loc
                        .expect("Component ID not found in lookup map. Is this a voltage probe?");
                    let global_idx = loc_unwrapped.observable_start_idx + local_index;
                    step.observables.get(global_idx).copied().unwrap_or(0.0)
                }
                CircuitSelection::Voltage => {
                    // For voltages, comp_id is actually the NodeID
                    step.voltages.get(comp_id).copied().unwrap_or(0.0)
                }
            };

            series.push((step.time, value));
        }

        series
    }
}

#[derive(Clone, Copy)]
pub enum CircuitSelection {
    Voltage,
    Current,
    Observable,
}

impl Default for SimState {
    fn default() -> Self {
        Self {
            history: Vec::new(),
            running: false,
            current_sample: 0,
            metadata: None,
            lookup_map: HashMap::new(),
        }
    }
}

fn lerp_color(a: Color32, b: Color32, t: f32) -> Color32 {
    let t = t.clamp(0.0, 1.0);
    Color32::from_rgb(
        (a.r() as f32 + (b.r() as f32 - a.r() as f32) * t) as u8,
        (a.g() as f32 + (b.g() as f32 - a.g() as f32) * t) as u8,
        (a.b() as f32 + (b.b() as f32 - a.b() as f32) * t) as u8,
    )
}
