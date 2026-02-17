use std::collections::HashMap;
use std::fmt::Display;
use egui::{Pos2, Vec2};
use faer::traits::ComplexField;
use faer::{Col, ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::ops::{Add, Sub};

/// The numerical trait.
/// For inference: T = f32
/// For high quality: T = f64
pub trait CircuitScalar:
num_traits::Float + std::fmt::Debug + Copy + Send + Sync + ComplexField + Display + 'static
{
    // possibly helper methods for fast approximations
}

impl CircuitScalar for f32 {}
impl CircuitScalar for f64 {}

/// A handle to a node in the circuit (like a wire junction)
/// In the matrix, this corresponds to a Row/Column index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

pub struct SimulationContext<T> {
    pub dt: T,
    pub time: T,
    pub step: usize,
    pub node_map: HashMap<NodeId, usize>,
}

impl<T> SimulationContext<T> {
    /// maps a global node id to the current solver matrix index
    pub fn map_index(&self, node: NodeId) -> Option<usize> {
        self.node_map.get(&node).copied()
    }
}

/// The interface a component must implement.
pub trait Component<T: CircuitScalar>: Send + Sync {
    /// Returns true if the component allows the matrix A to be pre-solved
    /// (e.g. Resistors, Capacitors, Inductors with fixed sample rate)
    /// Returns false for non-linear components (Diodes, Tubes, Transistors, etc.)
    fn linearity(&self) -> ComponentLinearity;

    /// Returns the nodes this component is connected to
    fn ports(&self) -> Vec<NodeId>;

    /// Called after partitioning, but before solving
    /// Gives the component the ability to cache the matrix indices for ports and auxiliary rows
    fn bake_indices(&mut self, ctx: &SimulationContext<T>) {}

    /// How many extra rows/cols does this component add to the matrix?
    /// For example Resistors = 0, Voltage Sources = 1, ...
    fn auxiliary_row_count(&self) -> usize {
        0
    }

    /// Called to inform the component where its rows live in the matrix
    /// `start_idx` is the row index in the matrix (already offset by num_nodes)
    fn set_auxiliary_index(&mut self, _start_idx: usize) {
        // Default: Do nothing
    }

    /// Called when the circuit is built
    /// Used for components that do not change over time
    /// (e.g. Resistors, discretized capacitors and inductors with fixed sample rate)
    /// Adds G (conductance) values to the A Matrix.
    fn stamp_static(&self, _matrix: &mut MatMut<T>) {}

    /// Time step preparation
    /// Called at the start of every audio sample
    /// Used to calculate the history current for capacitors and inductors based on previous voltages
    /// Modifies the Vector b
    fn stamp_dynamic(
        &mut self,
        prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {}

    /// Used for Time-Variant components (e.g. Potentiometer, LDR, Switch) that change their conductance G at runtime
    /// in the reduced matrix without re-inverting A_LL every time step.
    fn stamp_time_variant(
        &self,
        _matrix: &mut MatMut<T>,
        _ctx: &SimulationContext<T>,
    ) {}

    /// Non-Linear iteration (Newton-Raphson)
    /// Called multiple times per sample
    /// Only for Diodes, Tubes, Transistors, etc.
    /// Modifies Matrix A (Jacobian) and Vector b (Current correction)
    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        _matrix: &mut MatMut<T>,
        _rhs: &mut ColMut<T>,
        _ctx: &SimulationContext<T>,
        _l_size: usize,
    ) {}

    /// Post-Step update
    /// Called after the solver found the solution for the current frame
    /// Used to update internal state (e.g. capacitor charge)
    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {}

    fn is_converged(&self, _current_node_voltages: &ColRef<T>) -> bool {
        false
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![]
    }

    /// Calculates the values for the probes defined in `probe_definitions`
    /// TODO: pass a mutable slice of the probe values to avoid allocations in the audio thread.
    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
    ) -> Vec<T> {
        vec![]
    }

    /// Calculates the exact current flowing INTO every port defined in `ports()`
    /// For example for a BJT with ports [C, B, E], this returns [I_C, I_B, I_E]
    /// KCL defines that theoretically the sums should be zero
    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
    ) -> Vec<T>;

    /// Updates a parameter by name.
    /// Returns true if the static matrix needs to be rebuilt.
    /// For example if a Resistor value changes -> true
    /// If a Voltage source amplitude changes, return false since it only affects the b vector.
    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub struct ComponentProbe {
    pub name: String,
    pub unit: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ComponentLinearity {
    /// Conductance G is constant (e.g. Fixed Resistor)
    /// Goes into the Static Block (A_LL).
    LinearStatic,
    /// G is constant, b changes per sample (e.g. Capacitor, Inductor)
    /// Goes into the Static Block (A_LL), but updates b_L every step.
    LinearDynamic,
    /// G changes at runtime (e.g. Potentiometer, LDR, Switch)
    /// Must be in the Active Block (A_NN) to avoid re-inverting A_LL.
    TimeVariant,
    /// G changes during iteration (e.g. Diode, BJT, Tube)
    /// Must be in the Active Block (A_NN).
    NonLinear,
}