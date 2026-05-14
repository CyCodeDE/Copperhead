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
use crate::audio::load_and_resample_audio;
use crate::circuit::Circuit;
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::parameter::{resolve_param_value, ComponentEvalCtx, ParamValue};
use crate::signals::{AudioBufferSignal, ConstantSignal, SignalType, SineSignal};
use crate::util::deserialize_number_or_string;
use faer::{ColMut, ColRef, MatMut};
use num_traits::cast;
use portable_atomic::AtomicUsize;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct VoltageSourceDef {
    pub source_type: VoltageSourceType,
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum VoltageSourceType {
    DC {
        #[serde(deserialize_with = "deserialize_number_or_string")]
        voltage: String,
    },
    AC {
        #[serde(deserialize_with = "deserialize_number_or_string")]
        amplitude: String,
        #[serde(deserialize_with = "deserialize_number_or_string")]
        frequency: String,
        #[serde(deserialize_with = "deserialize_number_or_string")]
        phase: String,
    },
    AudioBuffer {
        file_path: PathBuf,
    },
}

impl<T: CircuitScalar> Instantiable<T> for VoltageSourceDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let ps = circuit.param_system.as_ref();

        match &self.source_type {
            VoltageSourceType::DC { voltage } => {
                let pv = ps
                    .map(|ps| resolve_param_value(voltage, ps))
                    .unwrap_or_else(|| {
                        ParamValue::Constant(voltage.trim().parse::<f64>().unwrap_or(0.0))
                    });
                let initial_v: T = cast(pv.as_constant().unwrap_or(0.0)).unwrap();
                let signal = SignalType::Constant(ConstantSignal { voltage: initial_v });
                let comp = VoltageSource::new_dc(nodes[0], nodes[1], signal, pv);
                circuit.add_component(comp);
            }
            VoltageSourceType::AC { amplitude, frequency, phase } => {
                let amp_pv = ps
                    .map(|ps| resolve_param_value(amplitude, ps))
                    .unwrap_or_else(|| {
                        ParamValue::Constant(amplitude.trim().parse::<f64>().unwrap_or(0.0))
                    });
                let freq_pv = ps
                    .map(|ps| resolve_param_value(frequency, ps))
                    .unwrap_or_else(|| {
                        ParamValue::Constant(frequency.trim().parse::<f64>().unwrap_or(0.0))
                    });
                let phase_pv = ps
                    .map(|ps| resolve_param_value(phase, ps))
                    .unwrap_or_else(|| {
                        ParamValue::Constant(phase.trim().parse::<f64>().unwrap_or(0.0))
                    });

                let amp0: T = cast(amp_pv.as_constant().unwrap_or(0.0)).unwrap();
                let freq0: T = cast(freq_pv.as_constant().unwrap_or(0.0)).unwrap();
                let phase0: T = cast(phase_pv.as_constant().unwrap_or(0.0)).unwrap();
                let omega0 = T::from(2.0).unwrap() * T::from(std::f64::consts::PI).unwrap() * freq0;

                let signal = SignalType::Sine(SineSignal {
                    amplitude: amp0,
                    frequency: freq0,
                    phase: phase0,
                    omega: cast(omega0).unwrap(),
                });
                let comp = VoltageSource::new_ac(nodes[0], nodes[1], signal, amp_pv, freq_pv, phase_pv);
                circuit.add_component(comp);
            }
            VoltageSourceType::AudioBuffer { file_path } => {
                let target_sample_rate = (T::from(1.0).unwrap() / dt)
                    .round()
                    .to_u32()
                    .expect("Failed to convert target sample rate to u32");
                let samples: Vec<T> = load_and_resample_audio(file_path, target_sample_rate);
                let signal = SignalType::AudioBuffer(AudioBufferSignal {
                    samples,
                    sample_rate: cast(target_sample_rate).unwrap(),
                    cursor: AtomicUsize::new(0),
                });
                circuit.add_component(VoltageSource::new(nodes[0], nodes[1], signal));
            }
        }
    }
}

/// Provides a voltage source that supports both constant and formula-driven values.
pub struct VoltageSource<T: CircuitScalar> {
    pub pos: NodeId,
    pub neg: NodeId,

    cached_idx_pos: Option<usize>,
    cached_idx_neg: Option<usize>,

    pub signal: SignalType<T>,
    matrix_idx: Option<usize>,

    current_voltage: T,

    // Formula-driven parameters. None for AudioBuffer / RealtimeInput sources.
    param_voltage: Option<ParamValue>,
    param_amplitude: Option<ParamValue>,
    param_frequency: Option<ParamValue>,
    param_phase: Option<ParamValue>,
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
            param_voltage: None,
            param_amplitude: None,
            param_frequency: None,
            param_phase: None,
        }
    }

    fn new_dc(pos: NodeId, neg: NodeId, signal: SignalType<T>, voltage: ParamValue) -> Self {
        Self {
            param_voltage: Some(voltage),
            ..Self::new(pos, neg, signal)
        }
    }

    fn new_ac(
        pos: NodeId,
        neg: NodeId,
        signal: SignalType<T>,
        amplitude: ParamValue,
        frequency: ParamValue,
        phase: ParamValue,
    ) -> Self {
        Self {
            param_amplitude: Some(amplitude),
            param_frequency: Some(frequency),
            param_phase: Some(phase),
            ..Self::new(pos, neg, signal)
        }
    }

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

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_pos = node_map.get(&self.pos).copied();
        self.cached_idx_neg = node_map.get(&self.neg).copied();
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.pos, self.neg]
    }

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

    fn refresh_per_step(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if let Some(ref mut pv) = self.param_voltage {
            if pv.is_dynamic() {
                let v = T::from(pv.eval(eval)).unwrap();
                self.signal.set_parameter("voltage", v);
            }
        }
        if let Some(ref mut pv) = self.param_amplitude {
            if pv.is_dynamic() {
                let v = T::from(pv.eval(eval)).unwrap();
                self.signal.set_parameter("amplitude", v);
            }
        }
        if let Some(ref mut pv) = self.param_frequency {
            if pv.is_dynamic() {
                let v = T::from(pv.eval(eval)).unwrap();
                self.signal.set_parameter("frequency", v);
            }
        }
        if let Some(ref mut pv) = self.param_phase {
            if pv.is_dynamic() {
                let v = T::from(pv.eval(eval)).unwrap();
                self.signal.set_parameter("phase", v);
            }
        }
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "V_src".to_string(), unit: "V".to_string() },
            ComponentProbe { name: "I_src".to_string(), unit: "A".to_string() },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        out_observables[0] = self.current_voltage;
        out_observables[1] = self.matrix_idx.map(|i| node_voltages[i]).unwrap_or(T::zero());
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let i_src = self.matrix_idx.map(|i| node_voltages[i]).unwrap_or(T::zero());
        out_currents[0] = -i_src;
        out_currents[1] = i_src;
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        self.signal.set_parameter(name, value);
        false
    }
}
