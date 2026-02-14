use crate::model::{CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext};
use faer::{Col, ColRef};

pub struct Resistor<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,
    pub conductance: T,
    pub resistance: T,
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
            resistance,
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
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::LinearStatic
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

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        if name == "resistance" {
            self.resistance = value;

            // update derived conductance
            self.conductance = if value.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap()
            } else {
                T::one() / value
            };
            // Return TRUE: The conductance matrix (G) changed,
            // so the solver MUST rebuild Matrix A before the next step.
            return true;
        }
        false
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "V_delta".into(), unit: "V".into() },
            ComponentProbe { name: "Current".into(), unit: "A".into() },
            ComponentProbe { name: "Power".into(), unit: "W".into() },
        ]
    }

    fn calculate_observables(&self, node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) -> Vec<T> {
        let v_a = self.get_voltage(self.node_a, node_voltages);
        let v_b = self.get_voltage(self.node_b, node_voltages);

        let voltage_drop = v_a - v_b;
        let current = voltage_drop * self.conductance;
        let power = voltage_drop * current;

        vec![voltage_drop, current, power]
    }

    fn terminal_currents(&self, sol: &ColRef<T>, _ctx: &SimulationContext<T>) -> Vec<T> {
        let v_a = self.get_voltage(self.node_a, sol);
        let v_b = self.get_voltage(self.node_b, sol);

        // Current flows from A -> B
        let i_a = (v_a - v_b) * self.conductance;
        // Current flowing INTO Node B is negative of that
        let i_b = -i_a;

        vec![i_a, i_b]
    }
}
