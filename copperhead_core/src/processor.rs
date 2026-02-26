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
use crate::circuit::Circuit;
use crate::components::ComponentId;
use crate::model::{CircuitScalar, SimulationContext};

pub struct CircuitProcessor<T: CircuitScalar> {
    circuit: Circuit<T>,
    sample_rate: f64,
    dt: T,
}

impl<T: CircuitScalar> CircuitProcessor<T> {
    pub fn new(mut circuit: Circuit<T>, sample_rate: f64, dt: T) -> Result<Self, String> {
        circuit
            .calculate_dc_operating_point(T::from(1e-6).unwrap(), 100, dt)
            .map_err(|e| format!("Initial state failed: {}", e))?;

        circuit.prepare(dt, false);

        Ok(Self {
            circuit,
            sample_rate,
            dt,
        })
    }

    #[inline]
    pub fn tick(&mut self) -> SimulationContext<T> {
        self.circuit.solve_step(self.dt)
    }

    pub fn get_circuit_ref(&self) -> &Circuit<T> {
        &self.circuit
    }

    pub fn get_circuit_mut(&mut self) -> &mut Circuit<T> {
        &mut self.circuit
    }

    /// Directly sets the voltage on a RealtimeInputSignal without allocation.
    /// The `input_id` must refer to a VoltageSource with a RealtimeInputSignal.
    #[inline]
    pub fn set_input_voltage(&mut self, input_id: ComponentId, voltage: T) {
        self.circuit
            .components
            .set_voltage_source_realtime_value(input_id, voltage);
    }
}
