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

use std::cell::Cell;
use std::collections::HashMap;
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use crate::components::{Component, ComponentLinearity};
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::util::math::{exp_safe, exp_safe_deriv, softplus_safe_deriv};
use crate::util::mna::{get_voltage, stamp_conductance, stamp_transconductance};

/// Internal state for the Pentode during Newton-Raphson iterations.
#[derive(Clone, Copy, Debug)]
struct PentodeIterationState<T: CircuitScalar> {
    pub prev_v_p: T,
    pub prev_v_g1: T,
    pub prev_v_g2: T,
    pub prev_v_c: T,
    pub prev_v_x: T,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum PentodeModel {
    P6L6GC,
}

impl PentodeModel {
    /// Returns MU, EX, KG1, KG2, KP, KVB, RGI, CG1P, CG1K, CPK, IS, VT
    pub fn parameters<T: CircuitScalar>(&self) -> (T, T, T, T, T, T, T, T, T, T, T, T) {
        let vt = T::from(0.02585).unwrap();
        let i_s = T::from(1e-9).unwrap();

        match self {
            PentodeModel::P6L6GC => {
                // Typical Norman Koren parameters for a 12AX7
                (
                    T::from(8.7).unwrap(),      // MU
                    T::from(1.35).unwrap(),     // EX
                    T::from(1460.0).unwrap(),   // KG1
                    T::from(4500.0).unwrap(),   // KG2
                    T::from(48.0).unwrap(),     // KP
                    T::from(12.0).unwrap(),     // KVB
                    T::from(1000.0).unwrap(),   // RGI (1k Ohms)
                    T::from(0.85e-12).unwrap(), // CG1P (CPG1: 0.85 pF)
                    T::from(14.0e-12).unwrap(), // CG1K (CCG: 14 pF)
                    T::from(12.0e-12).unwrap(), // CPK (CCP: 12 pF)
                    i_s,
                    vt,
                )
            }
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            PentodeModel::P6L6GC => "6L6GC",
        }
    }
}

pub struct Pentode<T: CircuitScalar> {
    // External Nodes
    pub node_p: NodeId, // Plate (Anode)
    pub node_g1: NodeId, // Control Grid
    pub node_g2: NodeId, // Screen Grid
    pub node_c: NodeId, // Cathode

    pub cached_idx_p: Option<usize>,
    pub cached_idx_g1: Option<usize>,
    pub cached_idx_g2: Option<usize>,
    pub cached_idx_c: Option<usize>,

    // Internal Virtual Node (X) for control grid current
    pub aux_start_index: Option<usize>,

    // Tube Parameters
    pub mu: T,
    pub ex: T,
    pub kg1: T,
    pub kg2: T,
    pub kp: T,
    pub kvb: T,
    pub rgi: T,
    pub c_g1p: T,
    pub c_g1k: T,
    pub c_pk: T,
    pub i_s: T,
    pub vt: T,
    pub g_min: T,

    // Simulation State
    iter_state: Cell<PentodeIterationState<T>>,
}

impl<T: CircuitScalar> Pentode<T> {
    pub fn new(
        node_p: NodeId,
        node_g1: NodeId,
        node_g2: NodeId,
        node_c: NodeId,
        mu: T,
        ex: T,
        kg1: T,
        kg2: T,
        kp: T,
        kvb: T,
        rgi: T,
        c_g1p: T,
        c_g1k: T,
        c_pk: T,
        i_s: T,
        vt: T,
    ) -> Self {
        Self {
            node_p,
            node_g1,
            node_g2,
            node_c,
            cached_idx_p: None,
            cached_idx_g1: None,
            cached_idx_g2: None,
            cached_idx_c: None,
            aux_start_index: None,
            mu,
            ex,
            kg1,
            kg2,
            kp,
            kvb,
            rgi,
            c_g1p,
            c_g1k,
            c_pk,
            i_s,
            vt,
            g_min: T::from(1e-9).unwrap(),
            iter_state: Cell::new(PentodeIterationState {
                prev_v_p: T::zero(),
                prev_v_g1: T::zero(),
                prev_v_g2: T::zero(),
                prev_v_c: T::zero(),
                prev_v_x: T::zero(),
            }),
        }
    }
}

impl<T: CircuitScalar> Component<T> for Pentode<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::NonLinear
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_p, self.node_g1, self.node_g2, self.node_c]
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        let get_idx = |n: NodeId| if n.0 == 0 { None } else { Some(node_map[&n]) };
        self.cached_idx_p = get_idx(self.node_p);
        self.cached_idx_g1 = get_idx(self.node_g1);
        self.cached_idx_g2 = get_idx(self.node_g2);
        self.cached_idx_c = get_idx(self.node_c);
    }

    fn auxiliary_row_count(&self) -> usize {
        1
    }

    fn set_auxiliary_index(&mut self, start_idx: usize) {
        self.aux_start_index = Some(start_idx);
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {
        let idx_x = self.aux_start_index;

        // Grid Resistor (between G1 and X)
        let g_rgi = T::one() / self.rgi;
        stamp_conductance(matrix, self.cached_idx_g1, idx_x, g_rgi, 0);

        // Convergence Resistors
        stamp_conductance(matrix, self.cached_idx_p, self.cached_idx_c, self.g_min, 0);
        stamp_conductance(matrix, self.cached_idx_g2, self.cached_idx_c, self.g_min, 0);
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

        let dt = ctx.dt;
        let prev_v_p = get_voltage(prev_node_voltages, self.cached_idx_p);
        let prev_v_g1 = get_voltage(prev_node_voltages, self.cached_idx_g1);
        let prev_v_c = get_voltage(prev_node_voltages, self.cached_idx_c);

        let inject_cap_history =
            |rhs: &mut ColMut<T>, n1: Option<usize>, n2: Option<usize>, c: T, v1: T, v2: T| {
                let i_hist = (c / dt) * (v1 - v2);
                if let Some(i) = n1 { rhs[i] += i_hist; }
                if let Some(j) = n2 { rhs[j] -= i_hist; }
            };

        inject_cap_history(rhs, self.cached_idx_g1, self.cached_idx_p, self.c_g1p, prev_v_g1, prev_v_p);
        inject_cap_history(rhs, self.cached_idx_g1, self.cached_idx_c, self.c_g1k, prev_v_g1, prev_v_c);
        inject_cap_history(rhs, self.cached_idx_p, self.cached_idx_c, self.c_pk, prev_v_p, prev_v_c);
    }

    fn stamp_nonlinear(
        &self,
        current_node_voltages: &ColRef<T>,
        matrix: &mut MatMut<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
        l_size: usize,
    ) {
        let dt = ctx.dt;
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g1 = get_voltage(current_node_voltages, self.cached_idx_g1);
        let v_g2 = get_voltage(current_node_voltages, self.cached_idx_g2);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);
        let v_x = get_voltage(current_node_voltages, self.aux_start_index);

        let v_pk = v_p - v_c;
        let v_g1k = v_g1 - v_c;
        let v_g2k = v_g2 - v_c;
        let v_xc = v_x - v_c;

        if !ctx.is_dc_analysis {
            stamp_conductance(matrix, self.cached_idx_g1, self.cached_idx_p, self.c_g1p / dt, l_size);
            stamp_conductance(matrix, self.cached_idx_g1, self.cached_idx_c, self.c_g1k / dt, l_size);
            stamp_conductance(matrix, self.cached_idx_p, self.cached_idx_c, self.c_pk / dt, l_size);
        }

        // Clamp V_G2K to prevent division by zero in u calculation
        let v_g2k_safe = v_g2k.max(T::from(1e-3).unwrap());

        // --- Plate Current Math ---
        let mut i_p = T::zero();
        let mut g_p = T::zero();
        let mut g_m1 = T::zero();
        let mut g_m2 = T::zero();

        let u = self.kp * (T::one() / self.mu + v_g1k / v_g2k_safe);
        let (ln_term, l_deriv) = softplus_safe_deriv(u);
        let e1 = (v_g2k_safe / self.kp) * ln_term;

        if e1 > T::zero() {
            let e1_pow_ex_minus_1 = e1.powf(self.ex - T::one());
            let atan_term = (v_pk / self.kvb).atan();

            i_p = (T::from(2.0).unwrap() / self.kg1) * e1.powf(self.ex) * atan_term;

            // Transconductance calculations
            let de1_dvg1k = l_deriv;
            let de1_dvg2k = (T::one() / self.kp) * ln_term - l_deriv * (v_g1k / v_g2k_safe);

            let g_base = (T::from(2.0).unwrap() * self.ex / self.kg1) * e1_pow_ex_minus_1 * atan_term;

            g_m1 = g_base * de1_dvg1k;
            // Smooth gradient cutoff for G2
            g_m2 = if v_g2k > T::from(1e-3).unwrap() { g_base * de1_dvg2k } else { T::zero() };

            // Plate Conductance calculation
            let d_atan_dvpk = self.kvb / (self.kvb * self.kvb + v_pk * v_pk);
            g_p = (T::from(2.0).unwrap() / self.kg1) * e1.powf(self.ex) * d_atan_dvpk;
        }

        // --- Screen Grid Current Math (I_G2) ---
        let mut i_g2 = T::zero();
        let mut g_g2_g2 = T::zero();
        let mut g_g2_g1 = T::zero();

        let arg_g2 = v_g2k / self.mu + v_g1k;
        if arg_g2 > T::zero() && v_g2k > T::zero() {
            let arg_g2_pow_ex_minus_1 = arg_g2.powf(self.ex - T::one());
            i_g2 = (T::one() / self.kg2) * arg_g2.powf(self.ex);

            let g_g2_base = (self.ex / self.kg2) * arg_g2_pow_ex_minus_1;
            g_g2_g1 = g_g2_base;
            g_g2_g2 = g_g2_base / self.mu;
        }

        // --- Grid Diode Math ---
        let (exp_val, exp_deriv) = exp_safe_deriv(v_xc / self.vt);
        let i_d = self.i_s * (exp_val - T::one());
        let g_d = (self.i_s / self.vt) * exp_deriv;

        // --- MNA Matrix Stamping ---

        // Plate Conductances
        stamp_conductance(matrix, self.cached_idx_p, self.cached_idx_c, g_p, l_size);
        stamp_transconductance(matrix, self.cached_idx_p, self.cached_idx_c, self.cached_idx_g1, self.cached_idx_c, g_m1, l_size);
        stamp_transconductance(matrix, self.cached_idx_p, self.cached_idx_c, self.cached_idx_g2, self.cached_idx_c, g_m2, l_size);

        // Screen Grid Conductances
        stamp_conductance(matrix, self.cached_idx_g2, self.cached_idx_c, g_g2_g2, l_size);
        stamp_transconductance(matrix, self.cached_idx_g2, self.cached_idx_c, self.cached_idx_g1, self.cached_idx_c, g_g2_g1, l_size);

        // Grid Diode Dynamic Conductance
        stamp_conductance(matrix, self.aux_start_index, self.cached_idx_c, g_d, l_size);

        // --- RHS Equivalent Currents ---
        let i_eq_p = i_p - (g_p * v_pk + g_m1 * v_g1k + g_m2 * v_g2k);
        let i_eq_g2 = i_g2 - (g_g2_g2 * v_g2k + g_g2_g1 * v_g1k);
        let i_eq_d = i_d - (g_d * v_xc);

        let t = |n: usize| n - l_size;

        if let Some(i) = self.cached_idx_p { rhs[t(i)] -= i_eq_p; }
        if let Some(i) = self.cached_idx_g2 { rhs[t(i)] -= i_eq_g2; }
        if let Some(i) = self.aux_start_index { rhs[t(i)] -= i_eq_d; }
        if let Some(i) = self.cached_idx_c { rhs[t(i)] += i_eq_p + i_eq_g2 + i_eq_d; }

        let state = self.iter_state.get();
        self.iter_state.set(PentodeIterationState {
            prev_v_p: v_p,
            prev_v_g1: v_g1,
            prev_v_g2: v_g2,
            prev_v_c: v_c,
            prev_v_x: v_x,
        });
    }

    fn is_converged(&self, current_node_voltages: &ColRef<T>) -> bool {
        let state = self.iter_state.get();

        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g1 = get_voltage(current_node_voltages, self.cached_idx_g1);
        let v_g2 = get_voltage(current_node_voltages, self.cached_idx_g2);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);
        let v_x = get_voltage(current_node_voltages, self.aux_start_index);

        let reltol = T::from(1e-3).unwrap();
        let vntol = T::from(1e-6).unwrap();

        let check_node = |v_new: T, v_old: T| -> bool {
            let diff = (v_new - v_old).abs();
            let max_abs = v_new.abs().max(v_old.abs());
            diff <= reltol * max_abs + vntol
        };

        let converged = check_node(v_p, state.prev_v_p)
            && check_node(v_g1, state.prev_v_g1)
            && check_node(v_g2, state.prev_v_g2)
            && check_node(v_c, state.prev_v_c)
            && check_node(v_x, state.prev_v_x);

        self.iter_state.set(PentodeIterationState {
            prev_v_p: v_p,
            prev_v_g1: v_g1,
            prev_v_g2: v_g2,
            prev_v_c: v_c,
            prev_v_x: v_x,
        });

        converged
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_p = get_voltage(node_voltages, self.cached_idx_p);
        let v_g1 = get_voltage(node_voltages, self.cached_idx_g1);
        let v_g2 = get_voltage(node_voltages, self.cached_idx_g2);
        let v_c = get_voltage(node_voltages, self.cached_idx_c);
        let v_x = get_voltage(node_voltages, self.aux_start_index);

        let v_pk = v_p - v_c;
        let v_g1k = v_g1 - v_c;
        let v_g2k = v_g2 - v_c;
        let v_xc = v_x - v_c;
        let v_g1x = v_g1 - v_x;

        let v_g2k_safe = v_g2k.max(T::from(1e-3).unwrap());

        // Plate Current
        let mut i_p_internal = T::zero();
        let u = self.kp * (T::one() / self.mu + v_g1k / v_g2k_safe);
        let e1 = (v_g2k_safe / self.kp) * (T::one() + exp_safe(u)).ln();

        if e1 > T::zero() {
            let e1_clamped = e1.max(T::from(1e-12).unwrap());
            i_p_internal = (T::from(2.0).unwrap() / self.kg1) * e1_clamped.powf(self.ex) * (v_pk / self.kvb).atan();
        }

        // Screen Current
        let mut i_g2_internal = T::zero();
        let arg_g2 = v_g2k / self.mu + v_g1k;
        if arg_g2 > T::zero() && v_g2k > T::zero() {
            i_g2_internal = (T::one() / self.kg2) * arg_g2.powf(self.ex);
        }

        // Resistors and Diodes
        let i_gmin_p = v_pk * self.g_min;
        let i_gmin_g2 = v_g2k * self.g_min;

        let total_i_p = i_p_internal + i_gmin_p;
        let total_i_g2 = i_g2_internal + i_gmin_g2;
        let total_i_g1 = v_g1x / self.rgi;

        let i_d = self.i_s * (exp_safe(v_xc / self.vt) - T::one());
        let total_i_c = -i_p_internal - i_gmin_p - i_g2_internal - i_gmin_g2 - i_d;

        out_currents[0] = total_i_p;
        out_currents[1] = total_i_g1;
        out_currents[2] = total_i_g2;
        out_currents[3] = total_i_c;
    }
}