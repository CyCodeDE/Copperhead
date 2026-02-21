/*
 * Copyright (c) 2026-2026 CyCode and the Copperhead contributors
 *
 * This file is part of Copperhead.
 *
 * Copperhead is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Copperhead is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Copperhead. If not, see <https://www.gnu.org/licenses/>.
 */
use std::collections::HashMap;
use crate::model::{
    CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext,
};
use crate::signals::{Signal, SignalType};
use faer::{ColMut, ColRef, MatMut};

/// Provides a constant voltage that never changes.
pub struct VoltageSource<T: CircuitScalar> {
    pub pos: NodeId,
    pub neg: NodeId,

    cached_idx_pos: Option<usize>,
    cached_idx_neg: Option<usize>,

    pub signal: SignalType<T>,
    matrix_idx: Option<usize>,

    current_voltage: T,
}

impl<T: CircuitScalar> VoltageSource<T> {
    pub fn new(pos: NodeId, neg: NodeId, signal: SignalType<T>) -> Self {
        Self {
            pos,
            neg,
            cached_idx_pos: None,
            cached_idx_neg: None,
            signal,
            matrix_idx: None,
            current_voltage: T::zero(),
        }
    }
}

impl<T: CircuitScalar> Component<T> for VoltageSource<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::LinearDynamic
    }

    fn bake_indices(&mut self, ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        let pos = node_map.get(&self.pos).copied();
        let neg = node_map.get(&self.neg).copied();

        if pos.is_none() {
            self.cached_idx_pos = None;
        } else {
            self.cached_idx_pos = pos;
        }
        if neg.is_none() {
            self.cached_idx_neg = None;
        } else {
            self.cached_idx_neg = neg;
        }
    }

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

    fn stamp_static(&self, matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {
        let src_idx = self.matrix_idx.expect("Circuit not built yet!");
        let one = T::one();

        if let Some(p) = self.cached_idx_pos {
            matrix[(p, src_idx)] = matrix[(p, src_idx)] + one;
            matrix[(src_idx, p)] = matrix[(src_idx, p)] + one;
        }

        if let Some(n) = self.cached_idx_neg {
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

        let val = self.signal.get_voltage(ctx.time, ctx.is_dc_analysis);
        self.current_voltage = val;

        rhs[src_idx] = val;
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe {
                name: "V_src".to_string(),
                unit: "V".to_string(),
            },
            ComponentProbe {
                name: "I_src".to_string(),
                unit: "A".to_string(),
            },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_observables: &mut [T]
    ) {
        let voltage = self.current_voltage;

        let current = if let Some(idx) = self.matrix_idx {
            node_voltages[idx]
        } else {
            T::zero()
        };

        out_observables[0] = voltage;
        out_observables[1] = current;
    }

    fn terminal_currents(&self, node_voltages: &ColRef<T>, ctx: &SimulationContext<T>, out_currents: &mut [T]) {
        let i_src = if let Some(idx) = self.matrix_idx {
            node_voltages[idx]
        } else {
            T::zero()
        };

        // We return current flowing INTO the ports
        out_currents[0] = -i_src; // Into Pos
        out_currents[1] = i_src;  // Into Neg
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        self.signal.set_parameter(name, value);

        false
    }
}
