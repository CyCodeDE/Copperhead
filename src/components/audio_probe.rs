/*
 * Copyright (c) 2026 CyCode and the Copperhead contributors
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

use crate::model::{CircuitScalar, Component, ComponentLinearity, NodeId, SimulationContext};
use faer::ColRef;
use std::collections::HashMap;
use std::path::PathBuf;

pub struct AudioProbe<T: CircuitScalar> {
    a: NodeId,

    cached_idx_a: Option<usize>,

    /// The file this probe is associated with
    pub filepath: PathBuf,

    /// Buffer to store the voltage samples
    pub buffer: Vec<T>,
    pub write_cursor: usize,

    pub target_sample_rate: u32,
}

impl<T: CircuitScalar> AudioProbe<T> {
    pub fn new(
        a: NodeId,
        filepath: PathBuf,
        initial_capacity: usize,
        target_sample_rate: u32,
    ) -> Self {
        Self {
            a,
            cached_idx_a: None,
            filepath,
            buffer: vec![T::zero(); initial_capacity],
            write_cursor: 0,
            target_sample_rate,
        }
    }

    #[inline(always)]
    fn get_voltage(&self, solution: &ColRef<T>) -> T {
        let v_pos = self
            .cached_idx_a
            .map(|i| solution[i])
            .unwrap_or_else(T::zero);
        v_pos
    }
}

impl<T: CircuitScalar> Component<T> for AudioProbe<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::LinearStatic
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = if self.a.0 == 0 {
            None
        } else {
            node_map.get(&self.a).copied()
        };
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.a]
    }

    #[inline(always)]
    fn update_state(&mut self, current_node_voltages: &ColRef<T>, _ctx: &SimulationContext<T>) {
        // Extract the voltage and push it
        let voltage = self.get_voltage(current_node_voltages);
        if self.write_cursor < self.buffer.len() {
            self.buffer[self.write_cursor] = voltage;
            self.write_cursor += 1;
        }
    }

    fn terminal_currents(
        &self,
        _: &faer::col::generic::Col<faer::col::Ref<'_, T>>,
        _: &SimulationContext<T>,
        _: &mut [T],
    ) {
    }
}
