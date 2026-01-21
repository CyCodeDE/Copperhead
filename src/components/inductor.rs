use crate::model::{CircuitScalar, Component, NodeId, SimulationContext};
use faer::{ColMut, ColRef, MatMut};

pub struct Inductor<T> {
    nodes: [NodeId; 2],
    inductance: T,

    // Cached geometric conductance g_eq = dt / (2L)
    g_eq: T,

    // History states
    prev_current: T,
    prev_voltage: T,
}

impl<T: CircuitScalar> Inductor<T> {
    pub fn new(n1: NodeId, n2: NodeId, l: T, dt: T) -> Self {
        let two = T::from(2.0).unwrap();
        // Guard against zero inductance to avoid division by zero
        let g_eq = if l.abs() < T::from(1e-12).unwrap() {
            T::from(1.0e12).unwrap() // effectively a short-circuit
        // TODO: If we ever implement variable time-step simulation, this needs to be recalculated
        } else {
            dt / (two * l)
        };

        Self {
            nodes: [n1, n2],
            inductance: l,
            g_eq,
            prev_current: T::zero(),
            prev_voltage: T::zero(),
        }
    }

    /// Helper to map NodeId to Matrix Index
    fn get_matrix_idx(node: NodeId) -> Option<usize> {
        if node.0 == 0 { None } else { Some(node.0 - 1) }
    }

    /// Helper to read voltage from solution vector.
    fn get_voltage_diff(&self, solution: &ColRef<T>) -> T {
        let v1 = if let Some(idx) = Self::get_matrix_idx(self.nodes[0]) {
            solution[idx]
        } else {
            T::zero()
        };

        let v2 = if let Some(idx) = Self::get_matrix_idx(self.nodes[1]) {
            solution[idx]
        } else {
            T::zero()
        };

        v1 - v2
    }
}

impl<T: CircuitScalar> Component<T> for Inductor<T> {
    fn ports(&self) -> Vec<NodeId> {
        self.nodes.to_vec()
    }

    fn auxiliary_row_count(&self) -> usize {
        0
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>) {
        let g = self.g_eq;

        let idx_a = Self::get_matrix_idx(self.nodes[0]);
        let idx_b = Self::get_matrix_idx(self.nodes[1]);

        // Diagonal A
        if let Some(i) = idx_a {
            matrix[(i, i)] = matrix[(i, i)] + g;
        }

        // Diagonal B
        if let Some(j) = idx_b {
            matrix[(j, j)] = matrix[(j, j)] + g;
        }

        // Off-Diagonals
        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            matrix[(i, j)] = matrix[(i, j)] - g;
            matrix[(j, i)] = matrix[(j, i)] - g;
        }
    }

    fn stamp_dynamic(
        &mut self,
        _prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        _ctx: &SimulationContext<T>,
    ) {
        // Calculate the "memory" current source equivalent
        // I_equiv = i[n-1] + G_eq * v[n-1]
        // KCL Equation at node: G_eq * v[n] = i[n] - I_equiv
        let i_source = self.prev_current + (self.g_eq * self.prev_voltage);

        if let Some(idx) = Self::get_matrix_idx(self.nodes[0]) {
            rhs[idx] = rhs[idx] - i_source;
        }
        if let Some(idx) = Self::get_matrix_idx(self.nodes[1]) {
            rhs[idx] = rhs[idx] + i_source;
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>) {
        let v_new = self.get_voltage_diff(current_node_voltages);

        // Calculate current using the discretized Trapezoidal equation
        // i[n] = G_eq * v[n] + (i[n-1] + G_eq * v[n-1])
        let i_history_term = self.prev_current + (self.g_eq * self.prev_voltage);
        let i_new = (self.g_eq * v_new) + i_history_term;

        self.prev_voltage = v_new;
        self.prev_current = i_new;
    }

    fn calculate_current(&self, solution: &ColRef<T>) -> T {
        // We need to return the instantaneous current i[n] based on the just-solved voltage.

        let v_new = self.get_voltage_diff(solution);
        let i_history_term = self.prev_current + (self.g_eq * self.prev_voltage);

        // i[n] = G_eq * v[n] + I_history
        (self.g_eq * v_new) + i_history_term
    }
}
