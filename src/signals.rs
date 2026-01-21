use crate::model::CircuitScalar;

/// Defines a value that changes over time.
pub trait Signal<T: CircuitScalar>: Send + Sync {
    fn get_voltage(&mut self, time: T) -> T;
}

/// Standard DC source
pub struct ConstantSignal<T> {
    pub voltage: T,
}

impl<T: CircuitScalar> Signal<T> for ConstantSignal<T> {
    fn get_voltage(&mut self, time: T) -> T {
        self.voltage
    }
}

pub struct SineSignal<T> {
    pub amplitude: T,
    pub frequency: T,
    pub phase: T,
}

impl<T: CircuitScalar> Signal<T> for SineSignal<T> {
    fn get_voltage(&mut self, time: T) -> T {
        let two = T::from(2.0).unwrap();
        let pi = T::from(std::f64::consts::PI).unwrap();
        // V = A * sin(2 * pi * f * t + phase)
        self.amplitude * (two * pi * self.frequency * time + self.phase).sin()
    }
}

pub struct AudioBufferSignal<T> {
    pub samples: Vec<T>,
    pub sample_rate: T,
    pub cursor: usize,
}
impl<T: CircuitScalar> Signal<T> for AudioBufferSignal<T> {
    fn get_voltage(&mut self, _time: T) -> T {
        if self.cursor < self.samples.len() {
            let v = self.samples[self.cursor];
            self.cursor += 1;
            v
        } else {
            T::zero()
        }
    }
}
