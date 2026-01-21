use crate::components::resistor::Resistor;
use crate::components::voltage_source::VoltageSource;
use crate::model::{CircuitScalar, Component, NodeId, SimulationContext};
use crate::signals::{ConstantSignal, SineSignal};

pub mod capacitor;
pub mod inductor;
pub mod resistor;
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
    Capacitor {
        a: usize,
        b: usize,
        capacitance: f64,
    },
    Inductor {
        a: usize,
        b: usize,
        inductance: f64,
    },
}

impl ComponentDescriptor {
    pub fn build<T: CircuitScalar>(self, dt: T) -> Box<dyn Component<T>> {
        match self {
            ComponentDescriptor::Resistor { a, b, ohms } => {
                Box::new(Resistor::new(
                    NodeId(a),
                    NodeId(b),
                    num_traits::cast(ohms)
                        .expect("Failed to cast resistance to circuit scalar type"),
                ))
            }
            ComponentDescriptor::DCSource { pos, neg, volts } => {
                let signal = Box::new(ConstantSignal {
                    voltage: num_traits::cast(volts)
                        .expect("Failed to cast voltage to circuit scalar type"),
                });
                Box::new(VoltageSource::new(NodeId(pos), NodeId(neg), signal))
            }
            ComponentDescriptor::ASource {
                pos,
                neg,
                amp,
                freq,
            } => {
                let signal = Box::new(SineSignal {
                    amplitude: num_traits::cast(amp)
                        .expect("Failed to cast amplitude to circuit scalar type"),
                    frequency: num_traits::cast(freq)
                        .expect("Failed to cast frequency to circuit scalar type"),
                    phase: T::zero(),
                });
                Box::new(VoltageSource::new(NodeId(pos), NodeId(neg), signal))
            }
            ComponentDescriptor::Capacitor { a, b, capacitance } => {
                Box::new(capacitor::Capacitor::new(
                    NodeId(a),
                    NodeId(b),
                    num_traits::cast(capacitance)
                        .expect("Failed to cast capacitance to circuit scalar type"),
                    dt,
                ))
            }
            ComponentDescriptor::Inductor { a, b, inductance } => {
                Box::new(inductor::Inductor::new(
                    NodeId(a),
                    NodeId(b),
                    num_traits::cast(inductance)
                        .expect("Failed to cast inductance to circuit scalar type"),
                    dt,
                ))
            }
        }
    }
}
