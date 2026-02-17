use crate::model::{CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext};
use faer::{ColMut, ColRef, MatMut};

pub struct Capacitor<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

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
    pub fn new(a: NodeId, b: NodeId, capacitance: T, dt: T) -> Self {
        // Trapezoidal rule equivalent conductance: G = 2C / dt
        let conductance = (T::from(2.0).unwrap() * capacitance) / dt;

        Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            capacitance,
            conductance,
            eq_current: T::zero(), // Assumes capacitor is initially uncharged
        }
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

    fn stamp_static(&self, matrix: &mut MatMut<T>) {
        // A discretized capacitor looks like a resistor of conductance G_eq in the A matrix
        let g = self.conductance;
        let idx_a = self.cached_idx_a;
        let idx_b = self.cached_idx_b;

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

        if let Some(i) = self.cached_idx_a {
            rhs[i] = rhs[i] - i_eq;
        }

        if let Some(j) = self.cached_idx_b {
            rhs[j] = rhs[j] + i_eq;
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, _ctx: &SimulationContext<T>) {
        let v_new = self.get_voltage_diff(current_node_voltages);

        let two = T::from(2.0).unwrap();
        self.eq_current = -self.eq_current - (two * self.conductance * v_new);
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
        let v = self.get_voltage_diff(node_voltages);
        let i = (v * self.conductance) + self.eq_current;
        vec![v, i]
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
    ) -> Vec<T> {
        let v = self.get_voltage_diff(node_voltages);
        let i_flow = (v * self.conductance) + self.eq_current;

        // Current entering Node A = i_flow
        // Current entering Node B = -i_flow
        vec![i_flow, -i_flow]
    }

    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        if name == "capacitance" {
            self.capacitance = value;
            // Recalculate conductance: G = 2C / dt
            self.conductance = (T::from(2.0).unwrap() * self.capacitance) / ctx.dt;
            // Return true because the matrix A (conductance) changed
            return true;
        }
        false
    }
}
