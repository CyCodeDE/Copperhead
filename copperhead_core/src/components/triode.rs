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
use crate::components::{Component, ComponentLinearity};
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::util::math::{exp_safe, exp_safe_deriv, pn_junction_limit, softplus_safe_deriv};
use crate::util::mna::{get_voltage, stamp_conductance, stamp_transconductance};
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::cell::Cell;
use std::collections::HashMap;
use std::f64::consts::SQRT_2;

/// Internal state for the Triode during Newton-Raphson iterations.
#[derive(Clone, Copy, Debug)]
struct TriodeIterationState<T: CircuitScalar> {
    pub prev_v_p: T,
    pub prev_v_g: T,
    pub prev_v_c: T,
    pub prev_v_x: T,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum TriodeModel {
    T12AX7,
}

impl TriodeModel {
    /// Returns MU, EX, KG1, KP, KVB, RGI, CCG, CGP, CCP, IS, VT
    pub fn parameters<T: CircuitScalar>(&self) -> (T, T, T, T, T, T, T, T, T, T, T) {
        let vt = T::from(0.02585).unwrap();
        let i_s = T::from(1e-9).unwrap();

        match self {
            TriodeModel::T12AX7 => {
                // Typical Norman Koren parameters for a 12AX7
                (
                    T::from(100.0).unwrap(),    // MU
                    T::from(1.4).unwrap(),      // EX
                    T::from(1060.0).unwrap(),   // KG1
                    T::from(600.0).unwrap(),    // KP
                    T::from(300.0).unwrap(),    // KVB
                    T::from(2000.0).unwrap(),   // RGI (2k Ohms)
                    T::from(2.3e-12).unwrap(),  // CCG (2.3 pF)
                    T::from(2.4e-12).unwrap(),  // CGP (2.4 pF)
                    T::from(0.9e-12).unwrap(),  // CCP (0.9 pF)
                    i_s,
                    vt,
                )
            }
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            TriodeModel::T12AX7 => "12AX7",
        }
    }
}

pub struct Triode<T: CircuitScalar> {
    // External Nodes
    pub node_p: NodeId, // Plate (Anode)
    pub node_g: NodeId, // Grid (Control)
    pub node_c: NodeId, // Cathode

    pub cached_idx_p: Option<usize>,
    pub cached_idx_g: Option<usize>,
    pub cached_idx_c: Option<usize>,

    // Internal Virtual Node (X)
    pub aux_start_index: Option<usize>,

    // Tube Parameters
    pub mu: T,
    pub ex: T,
    pub kg1: T,
    pub kp: T,
    pub kvb: T,
    pub rgi: T,
    pub ccg: T,
    pub cgp: T,
    pub ccp: T,
    pub i_s: T,
    pub vt: T,
    pub g_min: T,

    // Simulation State
    iter_state: Cell<TriodeIterationState<T>>,

    // BDF2 history states
    /// Internal capacitor voltage at t[n-1]
    v_ccg_m1: T,
    v_cgp_m1: T,
    v_ccp_m1: T,

    /// Internal capacitor voltage at t[n-2]
    v_ccg_m2: T,
    v_cgp_m2: T,
    v_ccp_m2: T,
}

impl<T: CircuitScalar> Triode<T> {
    pub fn new(
        node_p: NodeId,
        node_g: NodeId,
        node_c: NodeId,
        mu: T,
        ex: T,
        kg1: T,
        kp: T,
        kvb: T,
        rgi: T,
        ccg: T,
        cgp: T,
        ccp: T,
        i_s: T,
        vt: T,
    ) -> Self {
        Self {
            node_p,
            node_g,
            node_c,
            cached_idx_p: None,
            cached_idx_g: None,
            cached_idx_c: None,
            aux_start_index: None,
            mu,
            ex,
            kg1,
            kp,
            kvb,
            rgi,
            ccg,
            cgp,
            ccp,
            i_s,
            vt,
            g_min: T::from(1e-9).unwrap(),
            iter_state: Cell::new(TriodeIterationState {
                prev_v_p: T::zero(),
                prev_v_g: T::zero(),
                prev_v_c: T::zero(),
                prev_v_x: T::zero(),
            }),
            v_ccg_m1: T::zero(),
            v_cgp_m1: T::zero(),
            v_ccp_m1: T::zero(),
            v_ccg_m2: T::zero(),
            v_cgp_m2: T::zero(),
            v_ccp_m2: T::zero(),
        }
    }
}

impl<T: CircuitScalar> Component<T> for Triode<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::NonLinear
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_p, self.node_g, self.node_c]
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_p = if self.node_p.0 == 0 {
            None
        } else {
            Some(node_map[&self.node_p])
        };
        self.cached_idx_g = if self.node_g.0 == 0 {
            None
        } else {
            Some(node_map[&self.node_g])
        };
        self.cached_idx_c = if self.node_c.0 == 0 {
            None
        } else {
            Some(node_map[&self.node_c])
        };
    }

    fn auxiliary_row_count(&self) -> usize {
        1
    }

    fn set_auxiliary_index(&mut self, start_idx: usize) {
        self.aux_start_index = Some(start_idx);
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        if ctx.is_dc_analysis {
            return;
        }

        let idx_x = self.aux_start_index;

        // Grid Resistor (between G and X)
        let g_rgi = T::one() / self.rgi;
        stamp_conductance(matrix, self.cached_idx_g, idx_x, g_rgi, 0);

        // Convergence Resistor (between P and C)
        stamp_conductance(matrix, self.cached_idx_p, self.cached_idx_c, self.g_min, 0);
    }

    fn stamp_dynamic(
        &mut self,
        prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
        if ctx.is_dc_analysis {
            return;
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
        let idx_p = self.cached_idx_p;
        let idx_g = self.cached_idx_g;
        let idx_c = self.cached_idx_c;
        let idx_x = self.aux_start_index;

        let v_p = get_voltage(current_node_voltages, idx_p);
        let v_g = get_voltage(current_node_voltages, idx_g);
        let v_c = get_voltage(current_node_voltages, idx_c);
        let v_x = get_voltage(current_node_voltages, idx_x);

        let v_pk = v_p - v_c;
        let v_gk = v_g - v_c;
        let v_xc = v_x - v_c;

        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        // Stamp capacitors
        if !ctx.is_dc_analysis {
            let dt2 = two * ctx.dt;

            let g_eq_ccg = (three * self.ccg) / dt2;
            let g_eq_cgp = (three * self.cgp) / dt2;
            let g_eq_ccp = (three * self.ccp) / dt2;

            stamp_conductance(matrix, idx_g, idx_c, g_eq_ccg, l_size);
            stamp_conductance(matrix, idx_g, idx_p, g_eq_cgp, l_size);
            stamp_conductance(matrix, idx_p, idx_c, g_eq_ccp, l_size);

            // Current equivalents
            let i_eq_ccg = (self.ccg / dt2) * (four * self.v_ccg_m1 - self.v_ccg_m2);
            let i_eq_cgp = (self.cgp / dt2) * (four * self.v_cgp_m1 - self.v_cgp_m2);
            let i_eq_ccp = (self.ccp / dt2) * (four * self.v_ccp_m1 - self.v_ccp_m2);

            // Add parallel currents to RHS (Entering positive node, leaving negative)
            if let Some(i) = idx_g { rhs[i - l_size] = rhs[i - l_size] + i_eq_ccg + i_eq_cgp; }
            if let Some(i) = idx_c { rhs[i - l_size] = rhs[i - l_size] - i_eq_ccg - i_eq_ccp; }
            if let Some(i) = idx_p { rhs[i - l_size] = rhs[i - l_size] - i_eq_cgp + i_eq_ccp; }
        }

        // Shockley Diode (between Auxiliary and Cathode)
        let state = self.iter_state.get();
        let prev_v_xc = state.prev_v_x - state.prev_v_c;
        let v_crit = self.vt * (self.vt / (T::from(SQRT_2).unwrap() * self.i_s)).ln();
        let v_xc_limited = pn_junction_limit(v_xc, prev_v_xc, self.vt, v_crit);
        let (exp_val, _exp_deriv) = exp_safe_deriv(v_xc_limited / self.vt);
        let i_d = self.i_s * (exp_val - T::one());
        let g_d = (self.i_s / self.vt) * exp_val;
        let i_eq_d = i_d - g_d * v_xc_limited;

        stamp_conductance(matrix, idx_x, idx_c, g_d, l_size);
        if let Some(i) = idx_x { rhs[i - l_size] = rhs[i - l_size] - i_eq_d; }
        if let Some(i) = idx_c { rhs[i - l_size] = rhs[i - l_size] + i_eq_d; }

        // Koren Triode Plate Currents (between Plate and Cathode, controlled by G and Plate
        let v_pk_sq = v_pk * v_pk;
        let denom = (self.kvb + v_pk_sq).sqrt();
        let u = (T::one() / self.mu) + (v_gk / denom);

        let (ln_term, s) = softplus_safe_deriv(self.kp * u);
        let e1 = (v_pk / self.kp) * ln_term;

        let mut i_p = T::zero();
        let mut g_p = T::zero();
        let mut g_m = T::zero();

        if e1 > T::zero() {
            let e1_ex = e1.powf(self.ex);
            let e1_ex_minus_1 = e1.powf(self.ex - T::one());

            i_p = (two / self.kg1) * e1_ex;

            // Transconductance (gm = dIp / dVgk)
            let du_dvgk = T::one() / denom;
            let de1_dvgk = v_pk * s * du_dvgk;
            g_m = (two * self.ex / self.kg1) * e1_ex_minus_1 * de1_dvgk;

            // Plate Conductance (gp = dIp / dVpk)
            let du_dvpk = -v_gk * v_pk / denom.powf(three);
            let de1_dvpk = (T::one() / self.kp) * ln_term + v_pk * s * du_dvpk;
            g_p = (two * self.ex / self.kg1) * e1_ex_minus_1 * de1_dvpk;
        }

        let i_eq_p = i_p - g_p * v_pk - g_m * v_gk;

        stamp_conductance(matrix, idx_p, idx_c, g_p, l_size);
        stamp_transconductance(matrix, idx_p, idx_c, idx_g, idx_c, g_m, l_size);

        if let Some(i) = idx_p { rhs[i - l_size] = rhs[i - l_size] - i_eq_p; }
        if let Some(i) = idx_c { rhs[i - l_size] = rhs[i - l_size] + i_eq_p; }
    }

    /// Called after each newton raphson iteration to check if the solution has converged.
    fn is_converged(&self, current_node_voltages: &ColRef<T>) -> bool {
        let state = self.iter_state.get();
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g = get_voltage(current_node_voltages, self.cached_idx_g);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);
        let v_x = get_voltage(current_node_voltages, self.aux_start_index);

        let tol = T::from(1e-6).unwrap(); // Absolute tolerance for simplicity, adjust to your solver's standards
        (v_p - state.prev_v_p).abs() < tol &&
            (v_g - state.prev_v_g).abs() < tol &&
            (v_c - state.prev_v_c).abs() < tol &&
            (v_x - state.prev_v_x).abs() < tol
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g = get_voltage(current_node_voltages, self.cached_idx_g);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);
        let v_x = get_voltage(current_node_voltages, self.aux_start_index);

        // Advance BDF2 history states
        self.v_ccg_m2 = self.v_ccg_m1;
        self.v_cgp_m2 = self.v_cgp_m1;
        self.v_ccp_m2 = self.v_ccp_m1;

        self.v_ccg_m1 = v_g - v_c;
        self.v_cgp_m1 = v_g - v_p;
        self.v_ccp_m1 = v_p - v_c;

        self.iter_state.set(TriodeIterationState {
            prev_v_p: v_p,
            prev_v_g: v_g,
            prev_v_c: v_c,
            prev_v_x: v_x,
        });
    }
}
