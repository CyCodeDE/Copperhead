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

use crate::model::CircuitScalar;
use portable_atomic::AtomicUsize;
use std::sync::atomic::Ordering;

/// Defines a value that changes over time.
pub trait Signal<T: CircuitScalar>: Send + Sync {
    fn get_voltage(&self, time: T, is_dc_analysis: bool) -> T;

    /// Set a parameter by name.
    /// CAREFUL: The names should be unique across signal types. Since we currently use a single string to identify parameters, there is a risk of name collisions.
    /// For example, if both a SineSignal and a ConstantSignal have a parameter named "amplitude", it could lead to confusion when setting parameters.
    fn set_parameter(&mut self, _name: &str, _value: T) {}
}

pub enum SignalType<T> {
    Constant(ConstantSignal<T>),
    Sine(SineSignal<T>),
    AudioBuffer(AudioBufferSignal<T>),
    RealtimeInput(RealtimeInputSignal<T>),
}

impl<T: CircuitScalar> SignalType<T> {
    #[inline]
    pub fn get_voltage(&self, time: T, is_dc_analysis: bool) -> T {
        match self {
            SignalType::Constant(s) => s.get_voltage(time, is_dc_analysis),
            SignalType::Sine(s) => s.get_voltage(time, is_dc_analysis),
            SignalType::AudioBuffer(s) => s.get_voltage(time, is_dc_analysis),
            SignalType::RealtimeInput(s) => s.get_voltage(time, is_dc_analysis),
        }
    }

    #[inline]
    pub fn set_parameter(&mut self, name: &str, value: T) {
        match self {
            SignalType::Constant(s) => s.set_parameter(name, value),
            SignalType::Sine(s) => s.set_parameter(name, value),
            SignalType::AudioBuffer(s) => s.set_parameter(name, value),
            SignalType::RealtimeInput(s) => s.set_parameter(name, value),
        }
    }
}

/// Standard DC source
pub struct ConstantSignal<T> {
    pub voltage: T,
}

impl<T: CircuitScalar> Signal<T> for ConstantSignal<T> {
    fn get_voltage(&self, time: T, _is_dc_analysis: bool) -> T {
        self.voltage
    }

    fn set_parameter(&mut self, name: &str, value: T) {
        if name == "voltage" {
            self.voltage = value;
        }
    }
}

pub struct SineSignal<T> {
    pub amplitude: T,
    pub frequency: T,
    pub phase: T,

    // Precalculated: 2 * pi * freq
    pub omega: T,
}

impl<T: CircuitScalar> Signal<T> for SineSignal<T> {
    fn get_voltage(&self, time: T, is_dc_analysis: bool) -> T {
        if is_dc_analysis {
            return T::zero();
        }

        // V = A * sin(2 * pi * f * t + phase)
        self.amplitude * (self.omega * time + self.phase).sin()
    }

    fn set_parameter(&mut self, name: &str, value: T) {
        match name {
            "amplitude" => self.amplitude = value,
            "frequency" => self.frequency = value,
            "phase" => {
                self.omega =
                    T::from(2.0).unwrap() * T::from(std::f64::consts::PI).unwrap() * self.frequency;
                self.phase = value
            }
            _ => {}
        }
    }
}

pub struct AudioBufferSignal<T> {
    pub samples: Vec<T>,
    pub sample_rate: T,
    pub cursor: AtomicUsize,
}
impl<T: CircuitScalar> Signal<T> for AudioBufferSignal<T> {
    fn get_voltage(&self, _time: T, is_dc_analysis: bool) -> T {
        if is_dc_analysis {
            return T::zero();
        }

        let current = self.cursor.load(Ordering::Relaxed);
        if current < self.samples.len() {
            let v = self.samples[current];
            self.cursor.store(current + 1, Ordering::Relaxed);
            v
        } else {
            T::zero()
        }
    }
}

pub struct RealtimeInputSignal<T> {
    pub current_value: T,
}

impl<T: CircuitScalar> Signal<T> for RealtimeInputSignal<T> {
    fn get_voltage(&self, _time: T, is_dc_analysis: bool) -> T {
        if is_dc_analysis {
            return T::zero();
        }
        self.current_value
    }

    fn set_parameter(&mut self, name: &str, value: T) {
        if name == "value" {
            self.current_value = value;
        }
    }
}
