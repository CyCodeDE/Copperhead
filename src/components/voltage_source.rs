use crate::model::{CircuitScalar, Component, ComponentProbe, NodeId, SimulationContext};
use crate::signals::Signal;
use faer::{ColMut, ColRef, MatMut};

/// Provides a constant voltage that never changes.
pub struct VoltageSource<T: CircuitScalar> {
    pub pos: NodeId,
    pub neg: NodeId,
    pub signal: Box<dyn Signal<T>>,
    matrix_idx: Option<usize>,
}

impl<T: CircuitScalar> VoltageSource<T> {
    pub fn new(pos: NodeId, neg: NodeId, signal: Box<dyn Signal<T>>) -> Self {
        Self {
            pos,
            neg,
            signal,
            // unknown until build time
            matrix_idx: None,
        }
    }

    // Helper to map NodeId to Matrix Index.
    pub fn get_node_idx(&self, node: NodeId) -> Option<usize> {
        if node.0 == 0 { None } else { Some(node.0 - 1) }
    }
}

impl<T: CircuitScalar> Component<T> for VoltageSource<T> {
    fn ports(&self) -> Vec<NodeId> {
        vec![self.pos, self.neg]
    }

    // We need one extra row/col for MNA
    fn auxiliary_row_count(&self) -> usize {
        1
    }

    fn set_auxiliary_index(&mut self, start_idx: usize) {
        self.matrix_idx = Some(start_idx)
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>) {
        let src_idx = self.matrix_idx.expect("Circuit not built yet!");
        let one = T::one();

        if let Some(p) = self.get_node_idx(self.pos) {
            matrix[(p, src_idx)] = matrix[(p, src_idx)] + one;
            matrix[(src_idx, p)] = matrix[(src_idx, p)] + one;
        }

        if let Some(n) = self.get_node_idx(self.neg) {
            matrix[(n, src_idx)] = matrix[(n, src_idx)] - one;
            matrix[(src_idx, n)] = matrix[(src_idx, n)] - one;
        }
    }

    fn stamp_dynamic(
        &mut self,
        _prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
        let src_idx = self.matrix_idx.expect("Circuit not built yet!");

        let val = self.signal.get_voltage(ctx.time);

        rhs[src_idx] = val;
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "V_src".to_string(), unit: "V".to_string() },
            ComponentProbe { name: "I_src".to_string(), unit: "A".to_string() },
        ]
    }

    fn calculate_observables(&self, node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) -> Vec<T> {
        let voltage = self.signal.get_voltage(ctx.time);

        // In MNA, the current is the unknown variable at the auxiliary index
        let current = if let Some(idx) = self.matrix_idx {
            node_voltages[idx]
        } else {
            T::zero()
        };

        vec![voltage, current]
    }

    fn terminal_currents(&self, node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) -> Vec<T> {
        // The variable solved at 'matrix_idx' represents the current flowing
        // OUT of the positive node and INTO the negative node.
        let i_src = if let Some(idx) = self.matrix_idx {
            node_voltages[idx]
        } else {
            T::zero()
        };

        // We return current flowing INTO the ports [Pos, Neg]
        vec![
            -i_src, // Into Pos
            i_src   // Into Neg
        ]
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        self.signal.set_parameter(name, value);

        false
    }
}
