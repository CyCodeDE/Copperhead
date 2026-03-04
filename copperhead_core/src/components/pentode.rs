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
use crate::components::{Component, ComponentLinearity};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::util::math::{exp_safe_deriv, pn_junction_limit, softplus_safe_deriv};
use crate::util::mna::{get_voltage, stamp_conductance, stamp_transconductance};
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct PentodeDef {
    pub model: PentodeModel,
}

impl<T: CircuitScalar> Instantiable<T> for PentodeDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let (mu, ex, kg1, kg2, kp, kvb, rgi, cg1p, cg1k, cpk, is, vt) = self.model.parameters();
        let comp = Pentode::new(
            nodes[0], nodes[1], nodes[2], nodes[3], mu, ex, kg1, kg2, kp, kvb, rgi, cg1p, cg1k,
            cpk, is, vt,
        );
        circuit.add_component(comp);
    }
}

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
    _6L6GC,
    EL34,
}

impl PentodeModel {
    /// Returns MU, EX, KG1, KG2, KP, KVB, RGI, CG1P, CG1K, CPK, IS, VT
    pub fn parameters<T: CircuitScalar>(&self) -> (T, T, T, T, T, T, T, T, T, T, T, T) {
        let vt = T::from(0.02585).unwrap();
        let i_s = T::from(1e-9).unwrap();

        match self {
            PentodeModel::_6L6GC => {
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
            PentodeModel::EL34 => {
                (
                    T::from(11.0).unwrap(),     // MU
                    T::from(1.35).unwrap(),     // EX
                    T::from(650.0).unwrap(),    // KG1
                    T::from(4200.0).unwrap(),   // KG2
                    T::from(60.0).unwrap(),     // KP
                    T::from(24.0).unwrap(),     // KVB
                    T::from(1000.0).unwrap(),   // RGI
                    T::from(1.0e-12).unwrap(),  // CG1P
                    T::from(15.0e-12).unwrap(), // CG1K
                    T::from(8.0e-12).unwrap(),  // CPK
                    i_s,
                    vt,
                )
            }
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            PentodeModel::_6L6GC => "6L6GC",
            PentodeModel::EL34 => "EL34",
        }
    }
}

pub struct Pentode<T: CircuitScalar> {
    // External Nodes
    pub node_p: NodeId,  // Plate (Anode)
    pub node_g1: NodeId, // Control Grid
    pub node_g2: NodeId, // Screen Grid
    pub node_c: NodeId,  // Cathode

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

    // BDF2 history states
    /// Internal capacitor voltage at t[n-1]
    v_cg1p_m1: T,
    v_cg1k_m1: T,
    v_cpk_m1: T,

    /// Internal capacitor voltage at t[n-2]
    v_cg1p_m2: T,
    v_cg1k_m2: T,
    v_cpk_m2: T,
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
            v_cg1p_m1: T::zero(),
            v_cg1k_m1: T::zero(),
            v_cpk_m1: T::zero(),
            v_cg1p_m2: T::zero(),
            v_cg1k_m2: T::zero(),
            v_cpk_m2: T::zero(),
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
        let idx_g1 = self.cached_idx_g1;
        let idx_g2 = self.cached_idx_g2;
        let idx_c = self.cached_idx_c;
        let idx_x = self.aux_start_index;

        let v_p = get_voltage(current_node_voltages, idx_p);
        let v_g1 = get_voltage(current_node_voltages, idx_g1);
        let v_g2 = get_voltage(current_node_voltages, idx_g2);
        let v_c = get_voltage(current_node_voltages, idx_c);
        let v_x = get_voltage(current_node_voltages, idx_x);

        let v_pk = v_p - v_c;
        let v_g1k = v_g1 - v_c;
        let v_g2k = v_g2 - v_c;
        let v_xc = v_x - v_c;

        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        let t = |n: usize| n - l_size;

        // Capacitors
        if !ctx.is_dc_analysis {
            let dt2 = two * ctx.dt;

            let g_eq_cg1k = (three * self.c_g1k) / dt2;
            let g_eq_cg1p = (three * self.c_g1p) / dt2;
            let g_eq_cpk = (three * self.c_pk) / dt2;

            stamp_conductance(matrix, idx_g1, idx_c, g_eq_cg1k, l_size);
            stamp_conductance(matrix, idx_g1, idx_p, g_eq_cg1p, l_size);
            stamp_conductance(matrix, idx_p, idx_c, g_eq_cpk, l_size);

            let i_eq_cg1k = (self.c_g1k / dt2) * (four * self.v_cg1k_m1 - self.v_cg1k_m2);
            let i_eq_cg1p = (self.c_g1p / dt2) * (four * self.v_cg1p_m1 - self.v_cg1p_m2);
            let i_eq_cpk = (self.c_pk / dt2) * (four * self.v_cpk_m1 - self.v_cpk_m2);

            if let Some(i) = idx_g1 {
                rhs[t(i)] = rhs[t(i)] + i_eq_cg1k + i_eq_cg1p;
            }
            if let Some(i) = idx_c {
                rhs[t(i)] = rhs[t(i)] - i_eq_cg1k - i_eq_cpk;
            }
            if let Some(i) = idx_p {
                rhs[t(i)] = rhs[t(i)] - i_eq_cg1p + i_eq_cpk;
            }
        }

        // Grid Diode
        let state = self.iter_state.get();
        let prev_v_xc = state.prev_v_x - state.prev_v_c;
        let v_crit =
            self.vt * (self.vt / (T::from(std::f64::consts::SQRT_2).unwrap() * self.i_s)).ln();
        let v_xc_limited = pn_junction_limit(v_xc, prev_v_xc, self.vt, v_crit);

        let (exp_val, exp_deriv) = exp_safe_deriv(v_xc_limited / self.vt);
        let i_d = self.i_s * (exp_val - T::one());
        let g_d = (self.i_s / self.vt) * exp_deriv;
        let i_eq_d = i_d - g_d * v_xc_limited;

        stamp_conductance(matrix, idx_x, idx_c, g_d, l_size);
        if let Some(i) = idx_x {
            rhs[t(i)] = rhs[t(i)] - i_eq_d;
        }
        if let Some(i) = idx_c {
            rhs[t(i)] = rhs[t(i)] + i_eq_d;
        }

        // Plate Current Math
        let v_g2k_safe = v_g2k.max(T::from(1e-3).unwrap());

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

            i_p = (two / self.kg1) * e1.powf(self.ex) * atan_term;

            let de1_dvg1k = l_deriv;
            let de1_dvg2k = (T::one() / self.kp) * ln_term - l_deriv * (v_g1k / v_g2k_safe);

            let g_base = (two * self.ex / self.kg1) * e1_pow_ex_minus_1 * atan_term;

            g_m1 = g_base * de1_dvg1k;
            g_m2 = if v_g2k > T::from(1e-3).unwrap() {
                g_base * de1_dvg2k
            } else {
                T::zero()
            };

            let d_atan_dvpk = self.kvb / (self.kvb * self.kvb + v_pk * v_pk);
            g_p = (two / self.kg1) * e1.powf(self.ex) * d_atan_dvpk;
        }

        // Screen Grid Current Math (I_G2)
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

        // Plate & Screen
        stamp_conductance(matrix, idx_p, idx_c, g_p, l_size);
        stamp_transconductance(matrix, idx_p, idx_c, idx_g1, idx_c, g_m1, l_size);
        stamp_transconductance(matrix, idx_p, idx_c, idx_g2, idx_c, g_m2, l_size);

        stamp_conductance(matrix, idx_g2, idx_c, g_g2_g2, l_size);
        stamp_transconductance(matrix, idx_g2, idx_c, idx_g1, idx_c, g_g2_g1, l_size);

        let i_eq_p = i_p - (g_p * v_pk + g_m1 * v_g1k + g_m2 * v_g2k);
        let i_eq_g2 = i_g2 - (g_g2_g2 * v_g2k + g_g2_g1 * v_g1k);

        if let Some(i) = idx_p {
            rhs[t(i)] = rhs[t(i)] - i_eq_p;
        }
        if let Some(i) = idx_g2 {
            rhs[t(i)] = rhs[t(i)] - i_eq_g2;
        }
        if let Some(i) = idx_c {
            rhs[t(i)] = rhs[t(i)] + i_eq_p + i_eq_g2;
        }
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
        ctx: &SimulationContext<T>,
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

        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        // Core DC Currents
        let mut i_p_dc = T::zero();
        let u = self.kp * (T::one() / self.mu + v_g1k / v_g2k_safe);
        let (ln_term, _) = softplus_safe_deriv(u);
        let e1 = (v_g2k_safe / self.kp) * ln_term;

        if e1 > T::zero() {
            i_p_dc = (two / self.kg1) * e1.powf(self.ex) * (v_pk / self.kvb).atan();
        }

        let mut i_g2_dc = T::zero();
        let arg_g2 = v_g2k / self.mu + v_g1k;
        if arg_g2 > T::zero() && v_g2k > T::zero() {
            i_g2_dc = (T::one() / self.kg2) * arg_g2.powf(self.ex);
        }

        // Grid Diode & Resistors
        let state = self.iter_state.get();
        let prev_v_xc = state.prev_v_x - state.prev_v_c;
        let v_crit =
            self.vt * (self.vt / (T::from(std::f64::consts::SQRT_2).unwrap() * self.i_s)).ln();
        let v_xc_limited = pn_junction_limit(v_xc, prev_v_xc, self.vt, v_crit);

        let (exp_val, _) = exp_safe_deriv(v_xc_limited / self.vt);
        let i_d = self.i_s * (exp_val - T::one());

        let i_rgi = v_g1x / self.rgi;
        let i_p_gmin = v_pk * self.g_min;
        let i_g2_gmin = v_g2k * self.g_min;

        // Capacitive Dynamic Currents
        let mut i_cap_cg1k = T::zero();
        let mut i_cap_cg1p = T::zero();
        let mut i_cap_cpk = T::zero();

        if !ctx.is_dc_analysis {
            let dt2 = two * ctx.dt;

            let g_eq_cg1k = (three * self.c_g1k) / dt2;
            let g_eq_cg1p = (three * self.c_g1p) / dt2;
            let g_eq_cpk = (three * self.c_pk) / dt2;

            let i_eq_cg1k = (self.c_g1k / dt2) * (four * self.v_cg1k_m1 - self.v_cg1k_m2);
            let i_eq_cg1p = (self.c_g1p / dt2) * (four * self.v_cg1p_m1 - self.v_cg1p_m2);
            let i_eq_cpk = (self.c_pk / dt2) * (four * self.v_cpk_m1 - self.v_cpk_m2);

            i_cap_cg1k = g_eq_cg1k * v_g1k - i_eq_cg1k;
            i_cap_cg1p = g_eq_cg1p * (v_g1 - v_p) - i_eq_cg1p;
            i_cap_cpk = g_eq_cpk * v_pk - i_eq_cpk;
        }

        let total_i_p = i_p_dc + i_p_gmin + i_cap_cpk - i_cap_cg1p;
        let total_i_g1 = i_rgi + i_cap_cg1k + i_cap_cg1p;
        let total_i_g2 = i_g2_dc + i_g2_gmin;
        let total_i_c = -(i_p_dc + i_p_gmin + i_g2_dc + i_g2_gmin + i_d + i_cap_cg1k + i_cap_cpk);

        out_currents[0] = total_i_p;
        out_currents[1] = total_i_g1;
        out_currents[2] = total_i_g2;
        out_currents[3] = total_i_c;
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g1 = get_voltage(current_node_voltages, self.cached_idx_g1);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);
        let v_x = get_voltage(current_node_voltages, self.aux_start_index);

        // Advance BDF2 history states
        self.v_cg1p_m2 = self.v_cg1p_m1;
        self.v_cg1k_m2 = self.v_cg1k_m1;
        self.v_cpk_m2 = self.v_cpk_m1;

        self.v_cg1p_m1 = v_g1 - v_p;
        self.v_cg1k_m1 = v_g1 - v_c;
        self.v_cpk_m1 = v_p - v_c;

        self.iter_state.set(PentodeIterationState {
            prev_v_p: v_p,
            prev_v_g1: v_g1,
            prev_v_g2: get_voltage(current_node_voltages, self.cached_idx_g2),
            prev_v_c: v_c,
            prev_v_x: v_x,
        });
    }
}
