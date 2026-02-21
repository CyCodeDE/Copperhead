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
use crate::model::{
    CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext,
};
use faer::{ColMut, ColRef, MatMut};
use std::collections::HashMap;

pub struct Capacitor<T: CircuitScalar> {
    node_a: NodeId,
    node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    /// The physical capacitance in Farads
    capacitance: T,

    /// Equivalent Series Resistance in Ohms
    esr: T,

    /// The discrete equivalent conductance: G = 2C / dt
    conductance: T,

    /// The equivalent current source value (history state)
    /// Represents I_eq in the Norton equivalent model
    eq_current: T,
}

impl<T: CircuitScalar> Capacitor<T> {
    /// Creates a new Capacitor.
    pub fn new(a: NodeId, b: NodeId, capacitance: T, esr: T, dt: T) -> Self {
        // Trapezoidal rule equivalent conductance: G = 2C / dt
        let two = T::from(2.0).unwrap();
        let r_step = dt / (two * capacitance);
        let conductance = T::one() / (esr + r_step);

        Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            capacitance,
            esr,
            conductance,
            eq_current: T::zero(), // Assumes capacitor is initially uncharged
        }
    }

    /// Helper to get voltage across the component (Va - Vb)
    fn get_voltage_diff(&self, solution: &ColRef<T>) -> T {
        let v_a = if let Some(idx) = self.cached_idx_a {
            solution[idx]
        } else {
            T::zero()
        };

        let v_b = if let Some(idx) = self.cached_idx_b {
            solution[idx]
        } else {
            T::zero()
        };

        v_a - v_b
    }

    fn update_conductance(&mut self, dt: T) {
        let two = T::from(2.0).unwrap();
        let r_step = dt / (two * self.capacitance);
        self.conductance = T::one() / (self.esr + r_step);
    }
}

impl<T: CircuitScalar> Component<T> for Capacitor<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::LinearDynamic
    }

    fn bake_indices(&mut self, ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = if self.node_a.0 == 0 {
            None
        } else {
            Some(node_map.get(&self.node_a).copied().unwrap())
        };
        self.cached_idx_b = if self.node_b.0 == 0 {
            None
        } else {
            Some(node_map.get(&self.node_b).copied().unwrap())
        };
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        if ctx.is_dc_analysis {
            return;
        }

        // A discretized capacitor looks like a resistor of conductance G_eq in the A matrix
        let g = self.conductance;
        let idx_a = self.cached_idx_a;
        let idx_b = self.cached_idx_b;

        // Diagonal: Node A
        if let Some(i) = idx_a {
            matrix[(i, i)] = matrix[(i, i)] + g;
        }

        // Diagonal: Node B
        if let Some(j) = idx_b {
            matrix[(j, j)] = matrix[(j, j)] + g;
        }

        // Coupling
        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            matrix[(i, j)] = matrix[(i, j)] - g;
            matrix[(j, i)] = matrix[(j, i)] - g;
        }
    }

    fn stamp_dynamic(
        &mut self,
        _prev: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
        if ctx.is_dc_analysis {
            return;
        }

        let i_eq = self.eq_current;

        if let Some(i) = self.cached_idx_a {
            rhs[i] = rhs[i] - i_eq;
        }

        if let Some(j) = self.cached_idx_b {
            rhs[j] = rhs[j] + i_eq;
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_terminal = self.get_voltage_diff(current_node_voltages);
        let i_through = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v_terminal * self.conductance) + self.eq_current
        };

        let two = T::from(2.0).unwrap();
        let r_step = ctx.dt / (two * self.capacitance);

        let term = v_terminal + (i_through * (r_step - self.esr));
        self.eq_current = -(term * self.conductance);
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe {
                name: "Voltage".to_string(),
                unit: "V".to_string(),
            },
            ComponentProbe {
                name: "Current".to_string(),
                unit: "A".to_string(),
            },
            ComponentProbe {
                name: "Power".to_string(),
                unit: "W".to_string(),
            },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        let v = self.get_voltage_diff(node_voltages);

        let i = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v * self.conductance) + self.eq_current
        };

        let p = v * i;

        out_observables[0] = v;
        out_observables[1] = i;
        out_observables[2] = p;
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v = self.get_voltage_diff(node_voltages);

        let i_flow = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v * self.conductance) + self.eq_current
        };

        out_currents[0] = i_flow; // Current flowing INTO Node A
        out_currents[1] = -i_flow; // Current flowing INTO Node B is
    }

    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        match name {
            "capacitance" => {
                self.capacitance = value;
                self.update_conductance(ctx.dt);
                true
            }
            "esr" => {
                self.esr = value;
                self.update_conductance(ctx.dt);
                true
            }
            _ => false,
        }
    }
}
