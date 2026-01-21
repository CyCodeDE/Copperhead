use crate::model::{CircuitScalar, Component, NodeId, SimulationContext};
use faer::{ColMut, ColRef, MatMut};

pub struct Capacitor<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,

    /// The physical capacitance in Farads
    pub capacitance: T,

    /// The discrete equivalent conductance: G = 2C / dt
    conductance: T,

    /// The equivalent current source value (history state)
    /// Represents I_eq in the Norton equivalent model
    eq_current: T,
}

impl<T: CircuitScalar> Capacitor<T> {
    /// Creates a new Capacitor.
    /// `dt` is the simulation time step (1.0 / sample_rate).
    pub fn new(a: NodeId, b: NodeId, capacitance: T, dt: T) -> Self {
        // Trapezoidal rule equivalent conductance: G = 2C / dt
        let conductance = (T::from(2.0).unwrap() * capacitance) / dt;

        Self {
            node_a: a,
            node_b: b,
            capacitance,
            conductance,
            eq_current: T::zero(), // Assumes capacitor is initially uncharged
        }
    }

    /// Helper to map NodeId to Matrix Index.
    fn get_matrix_idx(node: NodeId) -> Option<usize> {
        if node.0 == 0 { None } else { Some(node.0 - 1) }
    }

    /// Helper to get voltage across the component (Va - Vb)
    fn get_voltage_diff(&self, solution: &ColRef<T>) -> T {
        let v_a = if self.node_a.0 == 0 {
            T::zero()
        } else {
            solution[self.node_a.0 - 1]
        };
        let v_b = if self.node_b.0 == 0 {
            T::zero()
        } else {
            solution[self.node_b.0 - 1]
        };
        v_a - v_b
    }
}

impl<T: CircuitScalar> Component<T> for Capacitor<T> {
    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>) {
        // A discretized capacitor looks like a resistor of conductance G_eq in the A matrix
        let g = self.conductance;
        let idx_a = Self::get_matrix_idx(self.node_a);
        let idx_b = Self::get_matrix_idx(self.node_b);

        // Diagonal: Node A
        if let Some(i) = idx_a {
            matrix[(i, i)] = matrix[(i, i)] + g;
        }

        // Diagonal: Node B
        if let Some(j) = idx_b {
            matrix[(j, j)] = matrix[(j, j)] + g;
        }

        // Coupling
        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            matrix[(i, j)] = matrix[(i, j)] - g;
            matrix[(j, i)] = matrix[(j, i)] - g;
        }
    }

    fn stamp_dynamic(
        &mut self,
        _prev: &ColRef<T>,
        rhs: &mut ColMut<T>,
        _ctx: &SimulationContext<T>,
    ) {
        let i_eq = self.eq_current;

        if let Some(i) = Self::get_matrix_idx(self.node_a) {
            rhs[i] = rhs[i] - i_eq;
        }

        if let Some(j) = Self::get_matrix_idx(self.node_b) {
            rhs[j] = rhs[j] + i_eq;
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>) {
        let v_new = self.get_voltage_diff(current_node_voltages);

        let two = T::from(2.0).unwrap();
        self.eq_current = -self.eq_current - (two * self.conductance * v_new);
    }

    fn calculate_current(&self, solution: &ColRef<T>) -> T {
        // i(t) = G * v(t) + I_eq(t)
        let v = self.get_voltage_diff(solution);
        (v * self.conductance) + self.eq_current
    }
}
