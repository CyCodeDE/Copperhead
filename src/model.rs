use egui::{Pos2, Vec2};
use faer::traits::ComplexField;
use faer::{Col, ColMut, ColRef, MatMut};
use std::ops::{Add, Sub};

/// The numerical trait.
/// For inference: T = f32
/// For high quality: T = f64
pub trait CircuitScalar:
    num_traits::Float + std::fmt::Debug + Copy + Send + Sync + ComplexField + 'static
{
    // possibly helper methods for fast approximations
}

impl CircuitScalar for f32 {}
impl CircuitScalar for f64 {}

/// A handle to a node in the circuit (like a wire junction)
/// In the matrix, this corresponds to a Row/Column index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
}

/// The interface a component must implement.
pub trait Component<T: CircuitScalar>: Send + Sync {
    /// Returns the nodes this component is connected to
    fn ports(&self) -> Vec<NodeId>;

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
    fn stamp_static(&self, matrix: &mut MatMut<T>);

    /// Time step preparation
    /// Called at the start of every audio sample
    /// Used to calculate the history current for capacitors and inductors based on previous voltages
    /// Modifies the Vector b
    fn stamp_dynamic(
        &mut self,
        prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    );

    /// Non-Linear iteration (Newton-Raphson)
    /// Called multiple times per sample
    /// Only for Diodes, Tubes, Transistors, etc.
    /// Returns true if the component is non-linear
    /// Modifies Matrix A (Jacobian) and Vector b (Current correction)
    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        _matrix: &mut MatMut<T>,
        _rhs: &mut ColMut<T>,
    ) -> bool {
        false
    }

    /// Post-Step update
    /// Called after the solver found the solution for the current frame
    /// Used to update internal state (e.g. capacitor charge)
    fn update_state(&mut self, current_node_voltages: &ColRef<T>);

    /// Calculates the current flowing through the component.
    /// Usually returns current flowing from Port 0 -> Port 1.
    fn calculate_current(&self, solution: &ColRef<T>) -> T {
        T::zero()
    }
}
