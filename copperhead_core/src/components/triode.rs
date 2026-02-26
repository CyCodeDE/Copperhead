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
use crate::util::math::{exp_safe, exp_safe_deriv, softplus_safe_deriv};
use crate::util::mna::{get_voltage, stamp_conductance, stamp_transconductance};
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::cell::Cell;
use std::collections::HashMap;

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
    /// Returns MU, EX, KG1, KP, KVB, RGI, CGP, CGK, CPK, IS, VT
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
                    T::from(2.4e-12).unwrap(),  // CGP (1.7 pF)
                    T::from(2.3e-12).unwrap(),  // CGK (1.6 pF)
                    T::from(0.9e-12).unwrap(), // CPK (0.46 pF)
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
    pub cgp: T,
    pub cgk: T,
    pub cpk: T,
    pub i_s: T,
    pub vt: T,
    pub g_min: T,

    // Simulation State
    iter_state: Cell<TriodeIterationState<T>>,
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
        cgp: T,
        cgk: T,
        cpk: T,
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
            cgp,
            cgk,
            cpk,
            i_s,
            vt,
            g_min: T::from(1e-9).unwrap(),
            iter_state: Cell::new(TriodeIterationState {
                prev_v_p: T::zero(),
                prev_v_g: T::zero(),
                prev_v_c: T::zero(),
                prev_v_x: T::zero(),
            }),
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

    fn stamp_static(&self, matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {
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
        let dt = ctx.dt;
        let idx_p = self.cached_idx_p;
        let idx_g = self.cached_idx_g;
        let idx_c = self.cached_idx_c;

        let prev_v_p = get_voltage(prev_node_voltages, idx_p);
        let prev_v_g = get_voltage(prev_node_voltages, idx_g);
        let prev_v_c = get_voltage(prev_node_voltages, idx_c);

        // Dynamic Equivalent Currents (I_eq = G_c * V_prev)
        let inject_cap_history =
            |rhs: &mut ColMut<T>, n1: Option<usize>, n2: Option<usize>, c: T, v1: T, v2: T| {
                let g_c = c / dt;
                let i_hist = g_c * (v1 - v2);
                if let Some(i) = n1 {
                    rhs[i] += i_hist;
                }
                if let Some(j) = n2 {
                    rhs[j] -= i_hist;
                }
            };

        inject_cap_history(rhs, idx_g, idx_p, self.cgp, prev_v_g, prev_v_p);
        inject_cap_history(rhs, idx_g, idx_c, self.cgk, prev_v_g, prev_v_c);
        inject_cap_history(rhs, idx_p, idx_c, self.cpk, prev_v_p, prev_v_c);
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

        // Reactive Stamping (Capacitances)
        if !ctx.is_dc_analysis {
            stamp_conductance(matrix, idx_g, idx_p, self.cgp / dt, l_size);
            stamp_conductance(matrix, idx_g, idx_c, self.cgk / dt, l_size);
            stamp_conductance(matrix, idx_p, idx_c, self.cpk / dt, l_size);
        }

        // Plate Current Math (Part A)
        let mut i_p = T::zero();
        let mut g_p = T::zero();
        let mut g_m = T::zero();

        let s = (self.kvb + v_pk * v_pk).sqrt();
        let u = self.kp * (T::one() / self.mu + v_gk / s);

        let (ln_term, l_deriv) = softplus_safe_deriv(u);

        let e1 = (v_pk / self.kp) * ln_term;

        if e1 > T::zero() {
            let e1_pow_ex_minus_1 = e1.powf(self.ex - T::one());

            i_p = (T::from(2.0).unwrap() / self.kg1) * (e1 * e1_pow_ex_minus_1);

            let du_dvpk = -self.kp * v_gk * v_pk / (s * s * s);
            let du_dvgk = self.kp / s;

            let de1_dvpk = (T::one() / self.kp) * ln_term + (v_pk / self.kp) * l_deriv * du_dvpk;
            let de1_dvgk = (v_pk / self.kp) * l_deriv * du_dvgk;

            let g_base = (T::from(2.0).unwrap() * self.ex / self.kg1) * e1_pow_ex_minus_1;
            g_p = g_base * de1_dvpk;
            g_m = g_base * de1_dvgk;
        }

        // Grid Diode Math (Part B)
        let (exp_val, exp_deriv) = exp_safe_deriv(v_xc / self.vt);

        let i_d = self.i_s * (exp_val - T::one());
        let g_d = (self.i_s / self.vt) * exp_deriv;

        // Plate Output Conductance
        stamp_conductance(matrix, idx_p, idx_c, g_p, l_size);

        // Plate Mutual Conductance
        stamp_transconductance(matrix, idx_p, idx_c, idx_g, idx_c, g_m, l_size);

        // Grid Diode Dynamic Conductance
        stamp_conductance(matrix, idx_x, idx_c, g_d, l_size);

        // RHS Stamping (Equivalent Linear Currents)
        let i_eq_p = i_p - (g_p * v_pk + g_m * v_gk);
        let i_eq_d = i_d - (g_d * v_xc);

        let t = |n: usize| n - l_size;

        // Plate Current leaves P, enters C
        if let Some(i) = idx_p {
            rhs[t(i)] -= i_eq_p;
        }
        if let Some(i) = idx_c {
            rhs[t(i)] += i_eq_p;
        }

        // Diode Current leaves X, enters C
        if let Some(i) = idx_x {
            rhs[t(i)] -= i_eq_d;
        }
        if let Some(i) = idx_c {
            rhs[t(i)] += i_eq_d;
        }

        // Update convergence state tracking (you'll need to define your own tolerance)
        let state = self.iter_state.get();
        self.iter_state.set(TriodeIterationState {
            prev_v_p: v_p,
            prev_v_g: v_g,
            prev_v_c: v_c,
            prev_v_x: v_x,
            ..state
        });
    }

    fn is_converged(&self, current_node_voltages: &ColRef<T>) -> bool {
        let state = self.iter_state.get();

        // Fetch current voltages
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g = get_voltage(current_node_voltages, self.cached_idx_g);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);
        let v_x = get_voltage(current_node_voltages, self.aux_start_index);

        // Standard SPICE tolerances (these can be moved to SimulationContext later)
        let reltol = T::from(1e-3).unwrap(); // 0.1% relative tolerance
        let vntol = T::from(1e-6).unwrap(); // 1 uV absolute tolerance

        // Helper to check convergence for a single node
        let check_node = |v_new: T, v_old: T| -> bool {
            let diff = (v_new - v_old).abs();
            let abs_new = v_new.abs();
            let abs_old = v_old.abs();
            let max_abs = if abs_new > abs_old { abs_new } else { abs_old };

            let limit = reltol * max_abs + vntol;
            diff <= limit
        };

        // Check all internal and external nodes heavily involved in the non-linear math
        let converged = check_node(v_p, state.prev_v_p)
            && check_node(v_g, state.prev_v_g)
            && check_node(v_c, state.prev_v_c)
            && check_node(v_x, state.prev_v_x);

        // Update the state for the NEXT iteration check
        self.iter_state.set(TriodeIterationState {
            prev_v_p: v_p,
            prev_v_g: v_g,
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
        // 1. Fetch current node voltages
        let v_p = get_voltage(node_voltages, self.cached_idx_p);
        let v_g = get_voltage(node_voltages, self.cached_idx_g);
        let v_c = get_voltage(node_voltages, self.cached_idx_c);
        let v_x = get_voltage(node_voltages, self.aux_start_index);

        let v_pk = v_p - v_c;
        let v_gk = v_g - v_c;
        let v_xc = v_x - v_c;
        let v_gx = v_g - v_x; // Voltage across the grid resistor

        // Calculate Plate Current (I_P)
        let mut i_p_internal = T::zero();

        let s = (self.kvb + v_pk * v_pk).sqrt();
        let u = self.kp * (T::one() / self.mu + v_gk / s);

        let exp_u = exp_safe(u);
        let e1 = (v_pk / self.kp) * (T::one() + exp_u).ln();

        if e1 > T::zero() {
            let e1_clamped = e1.max(T::from(1e-12).unwrap());
            i_p_internal = (T::from(2.0).unwrap() / self.kg1) * e1_clamped.powf(self.ex);
        }

        // Add convergence resistor current (P to C)
        let i_gmin = v_pk * self.g_min;
        let total_i_p = i_p_internal + i_gmin;

        // Calculate Grid Current (I_G)
        // Current flows into G, through R_GI, to Virtual Node X
        let i_rgi = v_gx / self.rgi;
        let total_i_g = i_rgi;

        // Calculate Cathode Current (I_C)
        // Current flows from X to C through the grid diode
        let exp_vxc_vt = exp_safe(v_xc / self.vt);
        let i_d = self.i_s * (exp_vxc_vt - T::one());

        let total_i_c = -i_p_internal - i_gmin - i_d;

        out_currents[0] = total_i_p;
        out_currents[1] = total_i_g;
        out_currents[2] = total_i_c;
    }
}
