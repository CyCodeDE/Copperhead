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
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::signals::{AudioBufferSignal, ConstantSignal, Signal, SignalType, SineSignal};
use faer::{ColMut, ColRef, MatMut};
use std::collections::HashMap;
use std::path::PathBuf;
use num_traits::cast;
use portable_atomic::AtomicUsize;
use crate::audio::load_and_resample_audio;
use crate::circuit::Circuit;
use crate::descriptor::Instantiable;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct VoltageSourceDef {
    pub source_type: VoltageSourceType,
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum VoltageSourceType {
    DC { voltage: f64 },
    AC { amplitude: f64, frequency: f64, phase: f64 },
    AudioBuffer { file_path: PathBuf }
}

impl<T: CircuitScalar> Instantiable<T> for VoltageSourceDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        match self.source_type {
            VoltageSourceType::DC { voltage } => {
                let signal = SignalType::Constant(ConstantSignal { voltage: cast(voltage).expect("Failed to cast Voltage") });
                let comp = VoltageSource::new(nodes[0], nodes[1], signal);
                circuit.add_component(comp);
            }
            VoltageSourceType::AC { amplitude, frequency, phase } => {
                let freq = cast(frequency).expect("Failed to cast Frequency");
                let signal = SignalType::Sine(SineSignal {
                    amplitude: cast(amplitude).expect("Failed to cast Amplitude"),
                    frequency: freq,
                    phase: cast(phase).expect("Failed to cast Phase"),
                    omega: cast(T::from(2.0).unwrap() * T::from(std::f64::consts::PI).unwrap() * freq).expect("Failed to cast angular frequency")
                });
                let comp = VoltageSource::new(nodes[0], nodes[1], signal);
                circuit.add_component(comp);
            }
            VoltageSourceType::AudioBuffer { ref file_path } => {
                let target_sample_rate = (T::from(1.0).unwrap() / dt)
                    .round()
                    .to_u32()
                    .expect("Failed to convert target sample rate to u32");
                let samples: Vec<T> = load_and_resample_audio(&file_path, target_sample_rate);

                let signal = SignalType::AudioBuffer(AudioBufferSignal {
                    samples,
                    sample_rate: num_traits::cast(target_sample_rate)
                        .expect("Failed to cast sample rate"),
                    cursor: AtomicUsize::new(0),
                });

                let comp = VoltageSource::new(nodes[0], nodes[1], signal);
                circuit.add_component(comp);
            }
        }
    }
}

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

    /// Directly sets the current value of a `RealtimeInputSignal` without allocation
    /// or requiring a `SimulationContext`. Returns `true` if the signal was a `RealtimeInputSignal`.
    #[inline]
    pub fn set_realtime_value(&mut self, value: T) -> bool {
        if let SignalType::RealtimeInput(ref mut s) = self.signal {
            s.current_value = value;
            true
        } else {
            false
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
        out_observables: &mut [T],
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

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let i_src = if let Some(idx) = self.matrix_idx {
            node_voltages[idx]
        } else {
            T::zero()
        };

        // We return current flowing INTO the ports
        out_currents[0] = -i_src; // Into Pos
        out_currents[1] = i_src; // Into Neg
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        self.signal.set_parameter(name, value);

        false
    }
}
