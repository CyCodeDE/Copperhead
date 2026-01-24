pub mod app;
mod components;
mod drawing;
pub mod ui;

use crate::components::ComponentDescriptor;
use crate::model::{CircuitScalar, GridPos, NodeId};
use std::collections::HashMap;
use crate::components::diode::DiodeModel;

#[derive(Clone, Debug, PartialEq)]
pub enum ComponentBuildData {
    Resistor { resistance: f64 },
    Capacitor { capacitance: f64 },
    DCSource { voltage: f64 },
    ASource { amplitude: f64, frequency: f64 },
    Inductor { inductance: f64 },
    Diode { model: DiodeModel },
    Ground,
}

impl ComponentBuildData {
    pub fn prefix(&self) -> &'static str {
        match self {
            Self::Resistor { .. } => "R",
            Self::Capacitor { .. } => "C",
            Self::Inductor { .. } => "L",
            Self::DCSource { .. } | Self::ASource { .. } => "V",
            Self::Diode { .. } => "D",
            Self::Ground => "",
        }
    }
}

/// Represents a component on the grid
#[derive(Clone, Debug)]
pub struct VisualComponent {
    pub id: usize, // UID to map to the solver
    pub name: String,
    pub component: ComponentBuildData, // TODO: replace with a UI specific descriptor instead of the logical one
    pub pos: GridPos,
    pub rotation: u8, // 0, 1, 2, 3 (90 degree steps)
}

impl VisualComponent {
    /// Returns the Grid coordinates of the component's ports (A, B, Pos, Neg, Grid, Plate, etc.)
    /// based on position and rotation
    /// Order is important: [NodeA/Pos, NodeB/Neg, ...]
    pub fn get_pin_locations(&self) -> Vec<GridPos> {
        // Define local offsets for a generic 2-port component (e.g. width 2 units)
        // Left pin: (-1, 0), Right pin: (1, 0)
        let local_pins = match &self.component {
            ComponentBuildData::Ground => vec![(0, 0)], // 1 Pin
            ComponentBuildData::Resistor { .. } => vec![(-1, 0), (1, 0)], // 2 Pins
            ComponentBuildData::DCSource { .. } => vec![(0, -1), (0, 1)], // 2 Pins (Top, Bottom)
            ComponentBuildData::ASource { .. } => vec![(0, -1), (0, 1)], // 2 Pins (Top, Bottom)
            ComponentBuildData::Capacitor { .. } => vec![(-1, 0), (1, 0)], // 2 Pins
            ComponentBuildData::Inductor { .. } => vec![(-1, 0), (1, 0)], // 2 Pins
            ComponentBuildData::Diode { .. } => vec![(-1, 0), (1, 0)], // 2 Pins (Anode, Cathode)
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

#[derive(Clone, Debug)]
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

    pub fn add_component(&mut self, data: ComponentBuildData, pos: GridPos, rotation: u8) {
        let name = self.generate_next_name(data.prefix());
        self.components.push(VisualComponent {
            id: self.next_component_id,
            name,
            component: data,
            pos,
            rotation,
        });
        self.next_component_id += 1;
    }

    pub fn remove_component(&mut self, component_id: usize) {
        self.components.retain(|comp| comp.id != component_id);
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

#[derive(Clone, Debug)]
pub struct Netlist {
    /// A list of component ready to be built into the simulation
    pub instructions: Vec<ComponentDescriptor>,
    /// Maps Grid Position -> Node ID in the simulation
    pub node_map: HashMap<GridPos, NodeId>,
    /// Maps UI Component ID (usize) -> Simulation Component Index (usize)
    pub component_map: HashMap<usize, usize>,
}

/// Data chunk for a single simulation step
#[derive(Clone, Debug)]
pub struct SimStepData {
    pub time: f64,
    // Index corresponds to NodeId
    pub voltages: Vec<f64>,
    // Index corresponds to Simulation Component Index
    pub currents: Vec<f64>,
}

pub struct SimState {
    pub history: Vec<SimStepData>,
    pub running: bool,
    pub current_sample: usize,
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
}

impl Default for SimState {
    fn default() -> Self {
        Self {
            history: Vec::new(),
            running: false,
            current_sample: 0,
        }
    }
}
