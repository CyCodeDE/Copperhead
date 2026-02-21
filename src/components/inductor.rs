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
use std::collections::HashMap;
use crate::model::{
    CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext,
};
use faer::{ColMut, ColRef, MatMut};

pub struct Inductor<T> {
    node_a: NodeId,
    node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    inductance: T,

    series_resistance: T,

    // Cached geometric conductance g_eq = dt / (2L)
    g_eq: T,

    // History states
    prev_current: T,
    prev_voltage: T,
}

impl<T: CircuitScalar> Inductor<T> {
    pub fn new(a: NodeId, b: NodeId, l: T, r: T, dt: T) -> Self {
        let mut inductor = Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            inductance: l,
            series_resistance: r,
            g_eq: T::zero(), // Will be calculated immediately below
            prev_current: T::zero(),
            prev_voltage: T::zero(),
        };

        inductor.update_conductance(dt);
        inductor
    }

    fn update_conductance(&mut self, dt: T) {
        let two = T::from(2.0).unwrap();

        let r_step = (two * self.inductance) / dt;

        let total_imp = self.series_resistance + r_step;

        // Guard against zero impedance
        self.g_eq = if total_imp.abs() < T::from(1e-12).unwrap() {
            T::from(1.0e12).unwrap()
        } else {
            T::one() / total_imp
        };
    }

    /// Helper to read voltage from solution vector.
    fn get_voltage_diff(&self, solution: &ColRef<T>) -> T {
        let v1 = if let Some(idx) = self.cached_idx_a {
            solution[idx]
        } else {
            T::zero()
        };

        let v2 = if let Some(idx) = self.cached_idx_b {
            solution[idx]
        } else {
            T::zero()
        };

        v1 - v2
    }

    fn calculate_history_current(&self) -> T {
        let two = T::from(2.0).unwrap();

        // The voltage drop due to the resistor in the previous step needs to be accounted for
        // in the history term derivation for RL branches.
        let resistive_drop_term = two * self.series_resistance * self.prev_current;

        self.prev_current + (self.g_eq * (self.prev_voltage - resistive_drop_term))
    }
}

impl<T: CircuitScalar> Component<T> for Inductor<T> {
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

    fn auxiliary_row_count(&self) -> usize {
        0
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        let g = if ctx.is_dc_analysis {
            if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap() // Finite short approximation
            } else {
                T::one() / self.series_resistance
            }
        } else {
            self.g_eq
        };

        let idx_a = self.cached_idx_a;
        let idx_b = self.cached_idx_b;

        // Diagonal A
        if let Some(i) = idx_a {
            matrix[(i, i)] = matrix[(i, i)] + g;
        }

        // Diagonal B
        if let Some(j) = idx_b {
            matrix[(j, j)] = matrix[(j, j)] + g;
        }

        // Off-Diagonals
        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            matrix[(i, j)] = matrix[(i, j)] - g;
            matrix[(j, i)] = matrix[(j, i)] - g;
        }
    }

    fn stamp_dynamic(
        &mut self,
        _prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
        if ctx.is_dc_analysis {
            return;
        }

        let i_source = self.calculate_history_current();

        if let Some(idx) = self.cached_idx_a {
            rhs[idx] = rhs[idx] - i_source;
        }
        if let Some(idx) = self.cached_idx_b {
            rhs[idx] = rhs[idx] + i_source;
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_new = self.get_voltage_diff(current_node_voltages);

        if ctx.is_dc_analysis {
            // Calculate DC Current: I = V / R_series
            let g_dc = if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap()
            } else {
                T::one() / self.series_resistance
            };

            let i_dc = v_new * g_dc;

            // Initialize history with DC Operating Point
            self.prev_voltage = v_new;
            self.prev_current = i_dc;
        } else {
            // Transient update
            let i_history_term = self.calculate_history_current();
            let i_new = (self.g_eq * v_new) + i_history_term;

            self.prev_voltage = v_new;
            self.prev_current = i_new;
        }
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
        out_observables: &mut [T]
    ) {
        let v_new = self.get_voltage_diff(node_voltages);

        let i_new = if ctx.is_dc_analysis {
            let g_dc = if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap()
            } else {
                T::one() / self.series_resistance
            };
            v_new * g_dc
        } else {
            let i_history_term = self.calculate_history_current();
            (self.g_eq * v_new) + i_history_term
        };

        let p_new = v_new * i_new;
        out_observables[0] = v_new;
        out_observables[1] = i_new;
        out_observables[2] = p_new;
    }

    fn terminal_currents(&self, node_voltages: &ColRef<T>, ctx: &SimulationContext<T>, out_currents: &mut [T]) {
        let v_new = self.get_voltage_diff(node_voltages);

        let i_flow = if ctx.is_dc_analysis {
            // DC Mode: Calculate current based on Series Resistance
            // (I = V / R)
            let g_dc = if self.series_resistance.abs() < T::from(1e-12).unwrap() {
                T::from(1.0e12).unwrap() // Finite short approximation
            } else {
                T::one() / self.series_resistance
            };
            v_new * g_dc
        } else {
            // Transient Mode: Calculate current based on discretized companion model
            // (I = G_eq * V + I_history)
            let i_history_term = self.calculate_history_current();
            (self.g_eq * v_new) + i_history_term
        };

        out_currents[0] = i_flow;
        out_currents[1] = -i_flow;
    }

    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        match name {
            "inductance" => {
                self.inductance = value;
                self.update_conductance(ctx.dt);
                true
            }
            "resistance" | "esr" => {
                self.series_resistance = value;
                self.update_conductance(ctx.dt);
                true
            }
            _ => false,
        }
    }
}
