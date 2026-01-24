use crate::model::{CircuitScalar, Component, NodeId, SimulationContext};
use faer::{Col, ColRef};

pub struct Resistor<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,
    pub conductance: T,
}

impl<T: CircuitScalar> Resistor<T> {
    pub fn new(a: NodeId, b: NodeId, resistance: T) -> Self {
        // Guard against divide-by-zero
        let conductance = if resistance.abs() < T::from(1e-12).unwrap() {
            // TODO: Treat it as a node-merge instead
            T::from(1.0e12).unwrap()
        } else {
            T::one() / resistance
        };

        Self {
            node_a: a,
            node_b: b,
            conductance,
        }
    }

    /// Helper to map NodeId to Matrix Index.
    fn get_matrix_idx(node: NodeId) -> Option<usize> {
        if node.0 == 0 { None } else { Some(node.0 - 1) }
    }

    fn get_voltage(&self, node: NodeId, solution: &ColRef<T>) -> T {
        if node.0 == 0 {
            T::zero() // Ground
        } else {
            solution[node.0 - 1]
        }
    }
}

impl<T: CircuitScalar> Component<T> for Resistor<T> {
    fn get_parameters(&self) -> Option<Vec<T>> {
        // Resistance
        Some(vec![T::one() / self.conductance])
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn stamp_static(&self, matrix: &mut faer::MatMut<T>) {
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

        // Off-Diagonals: Coupling
        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            matrix[(i, j)] = matrix[(i, j)] - g;
            matrix[(j, i)] = matrix[(j, i)] - g;
        }
    }

    fn stamp_dynamic(
        &mut self,
        _prev: &faer::ColRef<T>,
        _rhs: &mut faer::ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
        // No dynamic behavior
    }

    fn update_state(&mut self, _current: &faer::ColRef<T>, _ctx: &SimulationContext<T>) {
        // No state
    }

    fn calculate_current(&self, solution: &ColRef<T>, _ctx: &SimulationContext<T>) -> T {
        let v_a = self.get_voltage(self.node_a, solution);
        let v_b = self.get_voltage(self.node_b, solution);
        (v_a - v_b) * self.conductance
    }
}
