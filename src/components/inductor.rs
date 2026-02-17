use crate::model::{CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext};
use faer::{ColMut, ColRef, MatMut};

pub struct Inductor<T> {
    node_a: NodeId,
    node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

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
            node_a: n1,
            node_b: n2,
            cached_idx_a: None,
            cached_idx_b: None,
            inductance: l,
            g_eq,
            prev_current: T::zero(),
            prev_voltage: T::zero(),
        }
    }

    /// Helper to read voltage from solution vector.
    fn get_voltage_diff(&self, solution: &ColRef<T>) -> T {
        let v1 = if let Some(idx) = self.cached_idx_a {
            solution[idx]
        } else {
            T::zero()
        };

        let v2 = if let Some(idx) = self.cached_idx_b {
            solution[idx]
        } else {
            T::zero()
        };

        v1 - v2
    }
}

impl<T: CircuitScalar> Component<T> for Inductor<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::LinearDynamic
    }

    fn bake_indices(&mut self, ctx: &SimulationContext<T>) {
        let a = ctx.map_index(self.node_a);
        let b = ctx.map_index(self.node_b);

        if a.is_none() { self.cached_idx_a = None; } else { self.cached_idx_a = a; }
        if b.is_none() { self.cached_idx_b = None; } else { self.cached_idx_b = b; }
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn auxiliary_row_count(&self) -> usize {
        0
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>) {
        let g = self.g_eq;

        let idx_a = self.cached_idx_a;
        let idx_b = self.cached_idx_b;

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

        if let Some(idx) = self.cached_idx_a {
            rhs[idx] = rhs[idx] - i_source;
        }
        if let Some(idx) = self.cached_idx_b {
            rhs[idx] = rhs[idx] + i_source;
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, _ctx: &SimulationContext<T>) {
        let v_new = self.get_voltage_diff(current_node_voltages);

        // Calculate current using the discretized Trapezoidal equation
        // i[n] = G_eq * v[n] + (i[n-1] + G_eq * v[n-1])
        let i_history_term = self.prev_current + (self.g_eq * self.prev_voltage);
        let i_new = (self.g_eq * v_new) + i_history_term;

        self.prev_voltage = v_new;
        self.prev_current = i_new;
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "Voltage".to_string(), unit: "V".to_string() },
            ComponentProbe { name: "Current".to_string(), unit: "A".to_string() },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
    ) -> Vec<T> {
        let v_new = self.get_voltage_diff(node_voltages);

        let i_history_term = self.prev_current + (self.g_eq * self.prev_voltage);
        let i_new = (self.g_eq * v_new) + i_history_term;

        vec![v_new, i_new]
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
    ) -> Vec<T> {
        let v_new = self.get_voltage_diff(node_voltages);

        let i_history_term = self.prev_current + (self.g_eq * self.prev_voltage);
        let i_flow = (self.g_eq * v_new) + i_history_term;

        vec![i_flow, -i_flow]
    }

    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        if name == "inductance" {
            self.inductance = value;

            // Recalculate G_eq = dt / (2L)
            if self.inductance.abs() > T::from(1e-15).unwrap() {
                self.g_eq = ctx.dt / (T::from(2.0).unwrap() * self.inductance);
            } else {
                self.g_eq = T::from(1.0e12).unwrap();
            }
            return true;
        }
        false
    }
}
