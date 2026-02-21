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
use std::path::PathBuf;
use portable_atomic::AtomicUsize;
use crate::audio::load_and_resample_audio;
use crate::circuit::Circuit;
use crate::components::ComponentDescriptor::Bjt;
use crate::components::diode::Diode;
use crate::components::resistor::Resistor;
use crate::components::voltage_source::VoltageSource;
use crate::model::{CircuitScalar, Component, NodeId, SimulationContext};
use crate::signals::{AudioBufferSignal, ConstantSignal, SignalType, SineSignal};

pub mod capacitor;
pub mod diode;
pub mod inductor;
pub mod resistor;
pub mod transistor;
pub mod voltage_source;

// The values here are only "visual". They are constrained to T when the circuit is executed.
#[derive(Clone, Debug)]
pub enum ComponentDescriptor {
    Resistor {
        a: usize,
        b: usize,
        ohms: f64,
    },
    DCSource {
        pos: usize,
        neg: usize,
        volts: f64,
    },
    ASource {
        pos: usize,
        neg: usize,
        amp: f64,
        freq: f64,
    },
    AudioSource {
        pos: usize,
        neg: usize,
        file_path: PathBuf,
    },
    Capacitor {
        a: usize,
        b: usize,
        capacitance: f64,
        esr: f64,
    },
    Inductor {
        a: usize,
        b: usize,
        inductance: f64,
        series_resistance: f64,
    },
    Diode {
        a: usize,
        b: usize,
        /// Saturation current (Is)
        saturation_current: f64,
        /// Emission coefficient (N)
        emission_coefficient: f64,
        /// Series resistance (Rs)
        series_resistance: f64,
        /// Zero-bias junction capacitance (Cjo)
        cjo: f64,
        /// Grading coefficient (M)
        m: f64,
        /// Transit time (tt)
        transit_time: f64,
        // Breakdown voltage (BV)
        breakdown_voltage: f64,
        // Current at breakdown (IBV)
        breakdown_current: f64,
    },
    Bjt {
        c: usize,
        b: usize,
        e: usize,
        saturation_current: f64,
        beta_f: f64,
        beta_r: f64,
        vt: f64,
        vaf: f64,
        var: f64,
        rc: f64,
        rb: f64,
        re: f64,
        polarity: bool,
    },
}

impl ComponentDescriptor {
    pub fn add_to_circuit<T: CircuitScalar>(self, dt: T, circuit: &mut Circuit<T>) {
        match self {
            ComponentDescriptor::Resistor { a, b, ohms } => {
                let comp = Resistor::new(
                    NodeId(a),
                    NodeId(b),
                    num_traits::cast(ohms)
                        .expect("Failed to cast resistance to circuit scalar type"),
                );

                circuit.add_component(comp);
            }
            ComponentDescriptor::DCSource { pos, neg, volts } => {
                let signal = SignalType::Constant(ConstantSignal {
                    voltage: num_traits::cast(volts)
                        .expect("Failed to cast voltage to circuit scalar type"),
                });
                let comp = VoltageSource::new(NodeId(pos), NodeId(neg), signal);
                circuit.add_component(comp);
            }
            ComponentDescriptor::ASource {
                pos,
                neg,
                amp,
                freq,
            } => {
                let freq = num_traits::cast(freq)
                    .expect("Failed to cast frequency to circuit scalar type");

                let signal = SignalType::Sine(SineSignal {
                    amplitude: num_traits::cast(amp)
                        .expect("Failed to cast amplitude to circuit scalar type"),
                    frequency: freq,
                    phase: T::zero(),
                    omega: num_traits::cast(
                        T::from(2.0).unwrap() * T::from(std::f64::consts::PI).unwrap() * freq,
                    )
                    .expect("Failed to cast angular frequency to circuit scalar type"),
                });
                let comp = VoltageSource::new(NodeId(pos), NodeId(neg), signal);
                circuit.add_component(comp);
            }
            ComponentDescriptor::AudioSource {
                pos,
                neg,
                file_path
            } => {
                let target_sample_rate = (T::from(1.0).unwrap() / dt).round().to_u32().expect("Failed to convert target sample rate to u32");
                let samples: Vec<T> = load_and_resample_audio(&file_path, target_sample_rate);

                let signal = SignalType::AudioBuffer(AudioBufferSignal {
                    samples,
                    sample_rate: num_traits::cast(target_sample_rate)
                        .expect("Failed to cast sample rate"),
                    cursor: AtomicUsize::new(0),
                });

                let comp = VoltageSource::new(NodeId(pos), NodeId(neg), signal);
                circuit.add_component(comp);
            }
            ComponentDescriptor::Capacitor {
                a,
                b,
                capacitance,
                esr,
            } => {
                let comp = capacitor::Capacitor::new(
                    NodeId(a),
                    NodeId(b),
                    num_traits::cast(capacitance)
                        .expect("Failed to cast capacitance to circuit scalar type"),
                    num_traits::cast(esr).expect("Failed to cast ESR to circuit scalar type"),
                    dt,
                );
                circuit.add_component(comp);
            }
            ComponentDescriptor::Inductor {
                a,
                b,
                inductance,
                series_resistance,
            } => {
                let comp = inductor::Inductor::new(
                    NodeId(a),
                    NodeId(b),
                    num_traits::cast(inductance)
                        .expect("Failed to cast inductance to circuit scalar type"),
                    num_traits::cast(series_resistance)
                        .expect("Failed to cast series resistance to circuit scalar type"),
                    dt,
                );
                circuit.add_component(comp);
            }
            ComponentDescriptor::Diode {
                a,
                b,
                saturation_current,
                emission_coefficient,
                series_resistance,
                cjo,
                m,
                transit_time,
                breakdown_voltage,
                breakdown_current,
            } => {
                let comp = Diode::new(
                    NodeId(a),
                    NodeId(b),
                    num_traits::cast(saturation_current)
                        .expect("Failed to cast saturation current to circuit scalar type"),
                    num_traits::cast(emission_coefficient)
                        .expect("Failed to cast emission coefficient to circuit scalar type"),
                    num_traits::cast(series_resistance)
                        .expect("Failed to cast series resistance to circuit scalar type"),
                    num_traits::cast(cjo).expect(
                        "Failed to cast zero-bias junction capacitance to circuit scalar type",
                    ),
                    num_traits::cast(m)
                        .expect("Failed to cast grading coefficient to circuit scalar type"),
                    num_traits::cast(transit_time)
                        .expect("Failed to cast transit time to circuit scalar type"),
                    num_traits::cast(breakdown_voltage)
                        .expect("Failed to cast breakdown voltage to circuit scalar type"),
                    num_traits::cast(breakdown_current)
                        .expect("Failed to cast breakdown current to circuit scalar type"),
                );
                circuit.add_component(comp);
            }
            ComponentDescriptor::Bjt {
                c,
                b,
                e,
                saturation_current,
                beta_f,
                beta_r,
                vt,
                vaf,
                var,
                rc,
                rb,
                re,
                polarity,
            } => {
                let comp = transistor::bjt::Bjt::new(
                    NodeId(c),
                    NodeId(b),
                    NodeId(e),
                    num_traits::cast(saturation_current)
                        .expect("Failed to cast saturation current to circuit scalar type"),
                    num_traits::cast(beta_f)
                        .expect("Failed to cast forward beta to circuit scalar type"),
                    num_traits::cast(beta_r)
                        .expect("Failed to cast reverse beta to circuit scalar type"),
                    num_traits::cast(vt)
                        .expect("Failed to cast thermal voltage to circuit scalar type"),
                    num_traits::cast(vaf)
                        .expect("Failed to cast forward Early voltage to circuit scalar type"),
                    num_traits::cast(var)
                        .expect("Failed to cast reverse Early voltage to circuit scalar type"),
                    num_traits::cast(rc)
                        .expect("Failed to cast collector resistance to circuit scalar type"),
                    num_traits::cast(rb)
                        .expect("Failed to cast base resistance to circuit scalar type"),
                    num_traits::cast(re)
                        .expect("Failed to cast emitter resistance to circuit scalar type"),
                    polarity,
                );
                circuit.add_component(comp);
            }
        }
    }
}
