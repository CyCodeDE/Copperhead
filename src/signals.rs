use std::cell::Cell;
use std::sync::atomic::Ordering;
use portable_atomic::AtomicUsize;
use crate::model::CircuitScalar;

/// Defines a value that changes over time.
pub trait Signal<T: CircuitScalar>: Send + Sync {
    fn get_voltage(&self, time: T) -> T;

    fn set_parameter(&mut self, _name: &str, _value: T) {}
}

/// Standard DC source
pub struct ConstantSignal<T> {
    pub voltage: T,
}

impl<T: CircuitScalar> Signal<T> for ConstantSignal<T> {
    fn get_voltage(&self, time: T) -> T {
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
}

impl<T: CircuitScalar> Signal<T> for SineSignal<T> {
    fn get_voltage(&self, time: T) -> T {
        let two = T::from(2.0).unwrap();
        let pi = T::from(std::f64::consts::PI).unwrap();
        // V = A * sin(2 * pi * f * t + phase)
        self.amplitude * (two * pi * self.frequency * time + self.phase).sin()
    }

    fn set_parameter(&mut self, name: &str, value: T) {
        match name {
            "amplitude" => self.amplitude = value,
            "frequency" => self.frequency = value,
            "phase" => self.phase = value,
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
    fn get_voltage(&self, _time: T) -> T {
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
