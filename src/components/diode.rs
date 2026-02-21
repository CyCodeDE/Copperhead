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
use std::cell::Cell;
use std::collections::HashMap;
use crate::model::{
    CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext,
};
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::sync::Mutex;

/// Internal state used only during the Newton-Raphson iteration loop.
/// Grouping these reduces lock contention overhead.
#[derive(Clone, Copy)]
struct IterationState<T> {
    last_iter_voltage: T,
    last_iter_current: T,
    is_converged: bool,
}

/// Different types of diodes can be modeled by adjusting parameters.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum DiodeModel {
    // 1N4148
    D1N4148,
}

impl DiodeModel {
    pub fn parameters<T: CircuitScalar>(&self) -> (T, T, T, T, T, T, T, T) {
        match self {
            DiodeModel::D1N4148 => {
                let is = T::from(2.52e-9).unwrap(); // Saturation Current | typical 2.52nA
                let n = T::from(1.752).unwrap(); // Emission Coefficient | typical 1.752
                let rs = T::from(0.568).unwrap(); // Series Resistance | typical 0.568 Ohms
                let cjo = T::from(4e-12).unwrap(); // Zero-bias junction capacitance | typical 4pF
                let m = T::from(0.4).unwrap(); // Grading coefficient | typical 0.4
                let tt = T::from(20e-9).unwrap(); // Transit time | typical 20ns
                let bv = T::from(75.0).unwrap(); // breakdown voltage
                let ibv = T::from(5.0e-6).unwrap(); // breakdown current at BV | typical 5µA
                (is, n, rs, cjo, m, tt, bv, ibv)
            }
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            DiodeModel::D1N4148 => "1N4148",
        }
    }
}
pub struct Diode<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,

    // Cached matrix indices (populated in bake_indices)
    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    pub saturation_current: T,   // Is
    pub emission_coefficient: T, // N
    pub series_resistance: T,    // Rs
    pub cjo: T,                  // Zero-bias junction capacitance
    pub m: T,                    // Grading coefficient
    pub transit_time: T,         // Transit time (tt)
    pub vt: T,                   // Thermal voltage (kT/q)
    pub junction_potential: T,   // phi
    pub fc: T,                   // Forward-bias depletion capacitance coefficient (typ 0.5)
    pub bv: T,                   // Breakdown voltage
    pub ibv: T,                  // Current at Breakdown Voltage

    /// Voltage across the intrinsic diode at the end of the previous time step
    prev_voltage: T,
    /// Total charge (Q_jun + Q_diff) at the end of the previous time step
    prev_charge: T,
    /// Capacitor current at the end of the previous time step (for Trapezoidal integration)
    prev_cap_current: T,

    internal_node_idx: Option<usize>,

    g_min: T,
    /// Critical voltage for limiting algorithm (Pre-calculated)
    v_crit: T,
    // Critical voltage for breakdown limiting
    v_crit_bwd: T,

    iter_state: Cell<IterationState<T>>, // TODO: potential bottleneck, use atomic instead or let the solve loop handle state concurrency
}

impl<T: CircuitScalar> Diode<T> {
    pub fn new(
        node_a: NodeId,
        node_b: NodeId,
        is: T,
        n: T,
        rs: T,
        cjo: T,
        m: T,
        tt: T,
        bv: T,
        ibv: T,
    ) -> Self {
        let vt = T::from(0.02585).unwrap(); // ~300K thermal voltage at room temp (kT/q)
        // TODO: calculate the temperature dynamically based on the environment or allow user to set it, and calculate vt = kT/q accordingly

        // Pre-calculate V_crit for pnjlim
        // V_crit = N*Vt * ln( N*Vt / (Is * sqrt(2)) )
        let vt_n = n * vt;
        let sqrt_2 = T::from(2.0).unwrap().sqrt();
        let v_crit = vt_n * (vt_n / (is * sqrt_2)).ln();
        let v_crit_bwd = vt_n * (vt_n / (ibv * sqrt_2)).ln();

        Self {
            node_a,
            node_b,
            cached_idx_a: None,
            cached_idx_b: None,
            saturation_current: is,
            emission_coefficient: n,
            series_resistance: rs,
            cjo,
            m,
            transit_time: tt,
            vt,
            junction_potential: T::from(0.7).unwrap(),
            fc: T::from(0.5).unwrap(),
            bv,
            ibv,

            prev_voltage: T::zero(),
            prev_charge: T::zero(),
            prev_cap_current: T::zero(),

            internal_node_idx: None,
            g_min: T::from(1.0e-12).unwrap(), // 1pA/V leakage
            v_crit,
            v_crit_bwd,

            iter_state: Cell::new(IterationState {
                last_iter_voltage: T::zero(),
                last_iter_current: T::zero(),
                is_converged: false,
            }),
        }
    }

    /// Determines the index of the Anode of the *Intrinsic* diode.
    fn get_intrinsic_anode_idx(&self, offset: usize) -> (Option<usize>, usize) {
        if self.series_resistance > T::epsilon() {
            (self.internal_node_idx, offset)
        } else {
            (self.cached_idx_a, offset)
        }
    }

    /// Gets Vd (Anode - Cathode) based on the current solver vector
    fn get_intrinsic_voltage(&self, solution: &ColRef<T>) -> T {
        let idx_p = if self.series_resistance > T::epsilon() {
            self.internal_node_idx
        } else {
            self.cached_idx_a
        };

        let idx_k = self.cached_idx_b;

        // Read directly from solution using global indices
        let v_p = idx_p.map(|i| solution[i]).unwrap_or(T::zero());
        let v_k = idx_k.map(|i| solution[i]).unwrap_or(T::zero());

        v_p - v_k
    }

    /// Standard SPICE `pnjlim` implementation.
    /// Dampens voltage changes to prevent numerical overflow in exp().
    fn limit_voltage(&self, v_new: T, v_old: T) -> T {
        let vt_n = self.emission_coefficient * self.vt;
        let two = T::from(2.0).unwrap();

        // Checks if we are in (or entering) Breakdown
        // Treat the breakdown region as a "forward" diode by transforming coordinates:
        // V_equiv = -(V + BV)
        if v_new < -self.bv {
            let v_new_eq = -(v_new + self.bv);
            let v_old_eq = -(v_old + self.bv);
            let v_crit_bwd = self.v_crit_bwd;

            if v_new_eq > v_crit_bwd {
                let effective_v_old_eq = if v_old_eq < v_crit_bwd {
                    v_crit_bwd
                } else {
                    v_old_eq
                };

                let arg = (v_new_eq - effective_v_old_eq) / vt_n;

                if arg > two {
                    let limited_eq = effective_v_old_eq + vt_n * (two + (arg - two).ln());
                    // Transform back: V = -(V_eq + BV)
                    return -(limited_eq + self.bv);
                }
            }
            // If strictly inside breakdown but step is small, we accept the new voltage
            return v_new;
        }

        // Simple case: Voltage is low or reverse biased (but not breakdown)
        if v_new < self.v_crit {
            return v_new;
        }

        // Standard SPICE limiting for forward bias
        if v_new > v_old {
            let arg = (v_new - v_old) / vt_n;
            // CRITICAL FIX 1: Ensure arg > 2.0
            if arg > two {
                return v_old + vt_n * (two + (arg - two).ln());
            }
        } else {
            // Unwinding high voltage
            if v_new < self.v_crit {
                return v_new;
            }
        }

        v_new
    }

    /// Calculates: I_d, G_d, Q_tot, C_tot
    fn calculate_operating_point(&self, v_d: T) -> (T, T, T, T) {
        let one = T::one();
        let zero = T::zero();

        let vt_n = self.emission_coefficient * self.vt;

        // Clamp exponential argument for safety
        let max_exp_arg = T::from(80.0).unwrap();
        let exp_arg = (v_d / vt_n).min(max_exp_arg);

        // I = Is * (e^(V/nVt) - 1)
        let evd_minus_one = exp_arg.exp_m1();
        let evd = evd_minus_one + one;

        let mut i_dc = self.saturation_current * evd_minus_one + self.g_min * v_d;

        // G = Is/(nVt) * e^(V/nVt)
        let mut g_dc = (self.saturation_current / vt_n) * evd + self.g_min;

        let v_breakdown_onset =
            -self.bv + (self.emission_coefficient * self.vt * T::from(50.0).unwrap());

        // Handle breakdown
        // for performacne and floating point safety, only compute when necessary, as it is negligible otherwise
        if v_d < v_breakdown_onset {
            let v_bwd_arg = -(v_d + self.bv) / vt_n;

            // Clamp to avoid overflow if voltage goes insanely negative before limiting kicks in
            let v_bwd_arg_clamped = v_bwd_arg.min(max_exp_arg);

            let exp_bwd = v_bwd_arg_clamped.exp();

            // I_bwd flows cathode -> anode (negative sign)
            let i_bwd = -self.ibv * exp_bwd;

            // G_bwd is positive (slope is positive in IV curve)
            let g_bwd = (self.ibv / vt_n) * exp_bwd;

            i_dc = i_dc + i_bwd;
            g_dc = g_dc + g_bwd;
        }

        let q_diff = self.transit_time * i_dc;
        let c_diff = self.transit_time * g_dc;

        let phi = self.junction_potential;
        let fc_phi = self.fc * phi;

        let c_jun: T;
        let q_jun: T;

        if v_d < fc_phi {
            // Normal Reverse/Low Forward Bias
            let ratio = v_d / phi;
            let term = one - ratio;

            // Cj = Cjo / (1 - V/phi)^M
            // Qj = Cjo * phi * (1 - (1-V/phi)^(1-M)) / (1-M)

            if (self.m - T::from(0.5).unwrap()).abs() < T::epsilon() {
                let s = term.sqrt();
                c_jun = self.cjo / s;
                q_jun = self.cjo * phi * (one - s) * T::from(2.0).unwrap(); // 1/(1-0.5) = 2
            } else {
                let s = term.powf(-self.m);
                c_jun = self.cjo * s;

                let one_minus_m = one - self.m;
                let s_q = term.powf(one_minus_m);
                q_jun = (self.cjo * phi) * (one - s_q) / one_minus_m;
            }
        } else {
            let one_plus_m = one + self.m;
            let one_minus_fc = one - self.fc;

            // Constants at the transition point
            let term_fc = one_minus_fc.powf(-(one_plus_m)); // (1-FC)^-(1+M)
            let c_fc = self.cjo * one_minus_fc.powf(-self.m);

            // Slope of Capacitance
            // m_grad = Cjo * M / (phi * (1-FC)^(1+M))
            let c_gradient = (self.cjo / phi) * self.m * term_fc;

            // Delta Voltage from transition point
            let d_v = v_d - fc_phi;

            // C = C_fc + slope * dV
            c_jun = c_fc + c_gradient * d_v;

            // Q calculation (Area under C curve)
            // Q_fc = Charge accumulated up to FC*phi
            let one_minus_m = one - self.m;
            let q_fc = (self.cjo * phi) * (one - one_minus_fc.powf(one_minus_m)) / one_minus_m;

            // Q = Q_fc + C_fc*dV + 0.5*slope*dV^2
            let half = T::from(0.5).unwrap();
            q_jun = q_fc + (c_fc * d_v) + (half * c_gradient * d_v * d_v);
        }

        (i_dc, g_dc, q_diff + q_jun, c_diff + c_jun)
    }

    /// Stamp G into Matrix A
    fn stamp_conductance(matrix: &mut MatMut<T>, idx_a: Option<usize>, idx_b: Option<usize>, g: T) {
        if let Some(i) = idx_a {
            matrix[(i, i)] = matrix[(i, i)] + g;
        }
        if let Some(j) = idx_b {
            matrix[(j, j)] = matrix[(j, j)] + g;
        }

        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            matrix[(i, j)] = matrix[(i, j)] - g;
            matrix[(j, i)] = matrix[(j, i)] - g;
        }
    }

    /// Stamp G into Matrix A
    fn stamp_conductance_nonlinear(
        matrix: &mut MatMut<T>,
        idx_a: Option<usize>,
        idx_b: Option<usize>,
        g: T,
        offset: usize,
        a_offset: usize,
    ) {
        if let Some(i) = idx_a {
            let local_i = i - a_offset;
            matrix[(local_i, local_i)] = matrix[(local_i, local_i)] + g;
        }
        if let Some(j) = idx_b {
            let local_j = j - offset;
            matrix[(local_j, local_j)] = matrix[(local_j, local_j)] + g;
        }

        if let (Some(i), Some(j)) = (idx_a, idx_b) {
            let local_i = i - a_offset;
            let local_j = j - offset;
            matrix[(local_i, local_j)] = matrix[(local_i, local_j)] - g;
            matrix[(local_j, local_i)] = matrix[(local_j, local_i)] - g;
        }
    }

    /// Stamp Current J into RHS Vector b (J flows A -> B)
    fn stamp_current_source(
        rhs: &mut ColMut<T>,
        idx_a: Option<usize>,
        idx_b: Option<usize>,
        val: T,
        offset: usize,
        a_offset: usize,
    ) {
        if let Some(i) = idx_a {
            let local_i = i - a_offset;
            rhs[local_i] = rhs[local_i] - val;
        }
        if let Some(j) = idx_b {
            let local_j = j - offset;
            rhs[local_j] = rhs[local_j] + val;
        }
    }

    /// Helper to calculate the total current (DC + Transient) flowing through the diode
    /// based on the converged solution.
    fn compute_total_current(&self, solution: &ColRef<T>, dt: T) -> T {
        let v_d = self.get_intrinsic_voltage(solution);
        let (i_dc, _, q_tot, _) = self.calculate_operating_point(v_d);

        // I_cap = (2/dt) * (Q_new - Q_old) - I_cap_old
        let ag = T::from(2.0).unwrap() / dt;
        let i_cap = ag * (q_tot - self.prev_charge) - self.prev_cap_current;

        // Total Current = DC + Transient
        i_dc + i_cap
    }
}

impl<T: CircuitScalar> Component<T> for Diode<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::NonLinear
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn bake_indices(&mut self, ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        // Map global node indices to local matrix indices
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

    fn auxiliary_row_count(&self) -> usize {
        if self.series_resistance > T::epsilon() {
            1
        } else {
            0
        }
    }

    fn set_auxiliary_index(&mut self, start_idx: usize) {
        if self.series_resistance > T::epsilon() {
            self.internal_node_idx = Some(start_idx);
        } else {
            self.internal_node_idx = None;
        }
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        let idx_a = self.cached_idx_a;

        if let Some(r_idx) = self.internal_node_idx {
            let g_s = T::one() / self.series_resistance;
            Self::stamp_conductance(matrix, idx_a, Some(r_idx), g_s);
        }
    }

    fn stamp_nonlinear(
        &self,
        current_node_voltages: &ColRef<T>,
        matrix: &mut MatMut<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
        l_size: usize,
    ) {
        let mut state = self.iter_state.get();
        let v_old_iter = state.last_iter_voltage;

        let v_d_input = self.get_intrinsic_voltage(current_node_voltages);

        // pnjlim
        let v_d = self.limit_voltage(v_d_input, v_old_iter);

        state.last_iter_voltage = v_d;

        // Calculate Physics
        let (i_dc, g_dc, q_tot, c_tot) = self.calculate_operating_point(v_d);

        // I_cap = (2/dt)*(Q_n - Q_n-1) - I_cap_n-1
        // G_dyn = 2*C / dt
        let dt = ctx.dt;
        let ag = T::from(2.0).unwrap() / dt; // Alpha-G

        let g_dyn = ag * c_tot;
        let i_cap = ag * (q_tot - self.prev_charge) - self.prev_cap_current;

        let g_total = g_dc + g_dyn;
        let i_total_flowing = i_dc + i_cap;

        // Newton-Raphson Source: J = I_eq - G_eq * V_op
        let i_rhs = i_total_flowing - (g_total * v_d);

        let (idx_p, a_offset) = self.get_intrinsic_anode_idx(l_size);
        let idx_k = self.cached_idx_b;

        Self::stamp_conductance_nonlinear(matrix, idx_p, idx_k, g_total, l_size, a_offset);
        Self::stamp_current_source(rhs, idx_p, idx_k, i_rhs, l_size, a_offset);

        let current_diff = (i_total_flowing - state.last_iter_current).abs();

        // Release lock early
        let abs_tol = T::from(1e-12).unwrap();
        let rel_tol = T::from(1e-3).unwrap();

        let voltage_diff = (v_d - v_d_input).abs();
        let v_converged = voltage_diff < (self.vt * T::from(0.01).unwrap()); // Within a fraction of Vt
        let i_converged = current_diff < (abs_tol + rel_tol * i_total_flowing.abs());

        state.last_iter_current = i_total_flowing;
        state.is_converged = v_converged && i_converged;

        self.iter_state.set(state);
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        // Calculate the exact state at the converged solution
        let v_d = self.get_intrinsic_voltage(current_node_voltages);
        let (_, _, q_new, _) = self.calculate_operating_point(v_d);

        let dt = ctx.dt;
        let ag = T::from(2.0).unwrap() / dt;

        // Calculate capacitor current for history
        let i_cap_new = ag * (q_new - self.prev_charge) - self.prev_cap_current;

        // Update History
        self.prev_voltage = v_d;
        self.prev_charge = q_new;
        self.prev_cap_current = i_cap_new;

        let mut state = self.iter_state.get();
        state.last_iter_voltage = v_d;
        state.last_iter_current = T::zero();

        self.iter_state.set(state);
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

    fn calculate_observables(&self, solution: &ColRef<T>, ctx: &SimulationContext<T>, out_observables: &mut [T]) {
        // Calculate external voltage (Node A - Node B) using cached indices
        let v_a = self.cached_idx_a.map(|i| solution[i]).unwrap_or(T::zero());
        let v_b = self.cached_idx_b.map(|i| solution[i]).unwrap_or(T::zero());
        let v_total = v_a - v_b;

        let i_total = self.compute_total_current(solution, ctx.dt);

        out_observables[0] = v_total;
        out_observables[1] = i_total;
        out_observables[2] = v_total * i_total;
    }

    fn terminal_currents(&self, solution: &ColRef<T>, ctx: &SimulationContext<T>, out_currents: &mut [T]) {
        let i_flow = self.compute_total_current(solution, ctx.dt);
        // Current flows into Anode, out of Cathode
        out_currents[0] = i_flow;
        out_currents[1] = -i_flow;
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        match name {
            "is" | "saturation_current" => {
                self.saturation_current = value;
                false
            }
            "n" | "emission_coefficient" => {
                self.emission_coefficient = value;
                false
            }
            "rs" | "series_resistance" => {
                self.series_resistance = value;
                // Rs affects the static matrix (G = 1/Rs), so we must rebuild
                true
            }
            "bv" | "breakdown_voltage" => {
                self.bv = value;
                false
            }
            "ibv" => {
                self.ibv = value;
                false
            }
            "cjo" => {
                self.cjo = value;
                false
            }
            "tt" | "transit_time" => {
                self.transit_time = value;
                false
            }
            _ => false,
        }
    }

    fn is_converged(&self, _current_node_voltages: &ColRef<T>) -> bool {
        self.iter_state.get().is_converged
    }
}
