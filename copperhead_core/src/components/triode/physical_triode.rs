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
use crate::util::mna::{
    get_voltage, stamp_conductance, stamp_current_source, stamp_transconductance,
};
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct PhysicalTriodeDef {
    pub model: PhysicalTriodeModel,
}

impl<T: CircuitScalar> Instantiable<T> for PhysicalTriodeDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let (v_ct, m1_k, m1_ex, m2_k, m2_mu, m2_ex, p_k, p_mu, ik_k, ig_k, iak_k, c_ga, c_gk, c_pk) =
            self.model.parameters();
        let comp = PhysicalTriode::new(
            nodes[0], nodes[1], nodes[2], v_ct, m1_k, m1_ex, m2_k, m2_mu, m2_ex, p_k, p_mu, ik_k,
            ig_k, iak_k, c_ga, c_gk, c_pk,
        );
        circuit.add_component(comp);
    }
}

/// Internal state for the Triode during Newton-Raphson iterations.
#[derive(Clone, Copy, Debug)]
struct PhysicalTriodeIterState<T: CircuitScalar> {
    pub prev_v_p: T,
    pub prev_v_g: T,
    pub prev_v_c: T,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum PhysicalTriodeModel {
    _12AX7,
    _12AU7,
}

impl PhysicalTriodeModel {
    /// Returns v_ct, m1_k, m1_ex, m2_k, m2_mu, m2_ex, p_k, p_mu, ik_k, ig_k, iak_k, c_ga, c_gk, c_ak
    pub fn parameters<T: CircuitScalar>(&self) -> (T, T, T, T, T, T, T, T, T, T, T, T, T, T) {
        match self {
            PhysicalTriodeModel::_12AX7 => (
                T::from(0.59836683).unwrap(),    // v_ct
                T::from(0.0017172334).unwrap(),  // m1_k
                T::from(-0.2685074).unwrap(),    // m1_ex
                T::from(0.84817287).unwrap(),    // m2_k
                T::from(88.413802).unwrap(),     // m2_mu
                T::from(1.7685074).unwrap(),     // m2_ex
                T::from(0.001130216).unwrap(),   // p_k
                T::from(104.24031).unwrap(),     // p_mu
                T::from(0.00071211506).unwrap(), // ik_k
                T::from(0.000565108).unwrap(),   // ig_k
                T::from(0.00058141055).unwrap(), // iak_k
                T::from(1.7e-12).unwrap(),       // c_ga
                T::from(1.6e-12).unwrap(),       // c_gk
                T::from(0.5e-12).unwrap(),       // c_ak
            ),
            PhysicalTriodeModel::_12AU7 => (
                T::from(0.89005722).unwrap(),    // v_ct
                T::from(0.028826571).unwrap(),   // m1_k
                T::from(-0.90897681).unwrap(),   // m1_ex
                T::from(0.622671).unwrap(),      // m2_k
                T::from(13.089625).unwrap(),     // m2_mu
                T::from(2.4089768).unwrap(),     // m2_ex
                T::from(0.00087237591).unwrap(), // p_k
                T::from(21.021735).unwrap(),     // p_mu
                T::from(0.00055330711).unwrap(), // ik_k
                T::from(0.00043618795).unwrap(), // ig_k
                T::from(0.00049917061).unwrap(), // iak_k
                T::from(1.5e-12).unwrap(),       // c_ga
                T::from(1.6e-12).unwrap(),       // c_gk
                T::from(0.4e-12).unwrap(),       // c_ak
            ),
        }
    }
}

pub struct PhysicalTriode<T: CircuitScalar> {
    // External Nodes
    pub node_p: NodeId, // Plate (Anode)
    pub node_g: NodeId, // Grid (Control)
    pub node_c: NodeId, // Cathode

    pub cached_idx_p: Option<usize>,
    pub cached_idx_g: Option<usize>,
    pub cached_idx_c: Option<usize>,

    // Tube parameters
    pub v_ct: T,
    pub m1_k: T,
    pub m1_ex: T,
    pub m2_k: T,
    pub m2_mu: T,
    pub m2_ex: T,
    pub p_k: T,
    pub p_mu: T,
    pub ik_k: T,
    pub ig_k: T,
    pub iak_k: T,
    pub c_ga: T,
    pub c_gk: T,
    pub c_pk: T,

    g_min: T,

    // Simulation State
    iter_state: Cell<PhysicalTriodeIterState<T>>,

    // BDF2 history states
    /// Internal capacitor voltage at t[n-1]
    v_cgp_m1: T, // Grid to Plate
    v_cgc_m1: T, // Grid to Cathode
    v_cpc_m1: T, // Plate to Cathode

    /// Internal capacitor voltage at t[n-2]
    v_cgp_m2: T,
    v_cgc_m2: T,
    v_cpc_m2: T,
}

impl<T: CircuitScalar> PhysicalTriode<T> {
    pub fn new(
        node_p: NodeId,
        node_g: NodeId,
        node_c: NodeId,
        v_ct: T,
        m1_k: T,
        m1_ex: T,
        m2_k: T,
        m2_mu: T,
        m2_ex: T,
        p_k: T,
        p_mu: T,
        ik_k: T,
        ig_k: T,
        iak_k: T,
        c_ga: T,
        c_gk: T,
        c_pk: T,
    ) -> Self {
        Self {
            node_p,
            node_g,
            node_c,
            cached_idx_p: None,
            cached_idx_g: None,
            cached_idx_c: None,

            // Tube parameters
            v_ct,
            m1_k,
            m1_ex,
            m2_k,
            m2_mu,
            m2_ex,
            p_k,
            p_mu,
            ik_k,
            ig_k,
            iak_k,
            c_ga,
            c_gk,
            c_pk,

            g_min: T::from(1e-9).unwrap(),

            // Simulation State
            iter_state: Cell::new(PhysicalTriodeIterState {
                prev_v_p: T::zero(),
                prev_v_g: T::zero(),
                prev_v_c: T::zero(),
            }),

            // BDF2 history states
            v_cgp_m1: T::zero(),
            v_cgc_m1: T::zero(),
            v_cpc_m1: T::zero(),
            v_cgp_m2: T::zero(),
            v_cgc_m2: T::zero(),
            v_cpc_m2: T::zero(),
        }
    }
}

impl<T: CircuitScalar> Component<T> for PhysicalTriode<T> {
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
        0
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        if ctx.is_dc_analysis {
            return;
        }
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

        let v_plate = get_voltage(current_node_voltages, idx_p);
        let v_g = get_voltage(current_node_voltages, idx_g);
        let v_c = get_voltage(current_node_voltages, idx_c);

        let v_pk = v_plate - v_c;
        let v_gk = v_g - v_c;
        let v_gg = v_gk + self.v_ct;

        let zero = T::zero();
        let one = T::from(1.0).unwrap();
        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        // Stamp capacitors
        if !ctx.is_dc_analysis {
            let dt2 = two * ctx.dt;

            let g_eq_cgp = (three * self.c_ga) / dt2;
            let g_eq_cgc = (three * self.c_gk) / dt2;
            let g_eq_cpc = (three * self.c_pk) / dt2;

            stamp_conductance(matrix, idx_g, idx_p, g_eq_cgp, l_size); // Grid-Plate
            stamp_conductance(matrix, idx_g, idx_c, g_eq_cgc, l_size); // Grid-Cathode
            stamp_conductance(matrix, idx_p, idx_c, g_eq_cpc, l_size); // Plate-Cathode

            // Current equivalents
            let i_eq_cgp = (self.c_ga / dt2) * (four * self.v_cgp_m1 - self.v_cgp_m2);
            let i_eq_cgc = (self.c_gk / dt2) * (four * self.v_cgc_m1 - self.v_cgc_m2);
            let i_eq_cpc = (self.c_pk / dt2) * (four * self.v_cpc_m1 - self.v_cpc_m2);

            // Add parallel currents to RHS (Entering positive node, leaving negative)
            if let Some(i) = idx_g {
                rhs[i - l_size] = rhs[i - l_size] + i_eq_cgc + i_eq_cgp;
            }
            if let Some(i) = idx_c {
                rhs[i - l_size] = rhs[i - l_size] - i_eq_cgc - i_eq_cpc;
            }
            if let Some(i) = idx_p {
                rhs[i - l_size] = rhs[i - l_size] - i_eq_cgp + i_eq_cpc;
            }
        }

        // URAMP evaluations
        let u_vp = v_pk.max(zero);
        let u_vg = v_gk.max(zero);

        // URAMP derivatives (step functions)
        let is_vp_pos = if v_pk > zero { one } else { zero };
        let is_vg_pos = if v_gk > zero { one } else { zero };

        // --- Calculate M1 ---
        let m1_base = u_vp + T::from(1e-10).unwrap();
        let m1_inner = self.m1_k * m1_base;
        let v_m1 = m1_inner.powf(self.m1_ex);
        let dv_m1_dvp = (self.m1_ex * self.m1_k) * m1_inner.powf(self.m1_ex - one) * is_vp_pos;

        // --- Calculate M2 ---
        let m2_inner_val = v_gg + u_vp / self.m2_mu;
        let is_m2_inner_pos = if m2_inner_val > zero { one } else { zero };
        let m2_base = m2_inner_val.max(zero) + T::from(1e-10).unwrap();
        let m2_inner = self.m2_k * m2_base;

        let v_m2 = m2_inner.powf(self.m2_ex);
        let dv_m2_base =
            (self.m2_ex * self.m2_k) * m2_inner.powf(self.m2_ex - one) * is_m2_inner_pos;
        let dv_m2_dvg = dv_m2_base;
        let dv_m2_dvp = dv_m2_base * (is_vp_pos / self.m2_mu);

        // --- Calculate Node P ---
        let p_inner = v_gg + u_vp / self.p_mu;
        let is_p_inner_pos = if p_inner > zero { one } else { zero };
        let p_base = p_inner.max(zero) + T::from(1e-10).unwrap();

        let v_p = self.p_k * p_base.powf(T::from(1.5).unwrap());
        let dv_p_base = (self.p_k * T::from(1.5).unwrap())
            * p_base.powf(T::from(0.5).unwrap())
            * is_p_inner_pos;
        let dv_p_dvg = dv_p_base;
        let dv_p_dvp = dv_p_base * (is_vp_pos / self.p_mu);

        // --- Calculate IK ---
        let v_ik;
        let dv_ik_dvp;
        let dv_ik_dvg;

        if v_gg > zero {
            v_ik = v_p;
            dv_ik_dvp = dv_p_dvp;
            dv_ik_dvg = dv_p_dvg;
        } else {
            let coeff = self.ik_k;
            v_ik = coeff * v_m1 * v_m2;
            dv_ik_dvp = coeff * (dv_m1_dvp * v_m2 + v_m1 * dv_m2_dvp);
            dv_ik_dvg = coeff * (v_m1 * dv_m2_dvg);
        }

        // --- Calculate IG ---
        let denom_ig = u_vp + u_vg;
        let mut v_ig = zero;
        let mut dv_ig_dvp = zero;
        let mut dv_ig_dvg = zero;

        if denom_ig > T::from(1e-16).unwrap() {
            let f1 = self.ig_k * u_vg.powf(T::from(1.5).unwrap());
            let df1_dvg =
                (self.ig_k * T::from(1.5).unwrap()) * u_vg.powf(T::from(0.5).unwrap()) * is_vg_pos;

            let f2 = (u_vg / denom_ig) * T::from(1.2).unwrap() + T::from(0.4).unwrap();

            let df2_dvp = -T::from(1.2).unwrap() * u_vg / (denom_ig * denom_ig) * is_vp_pos;
            let df2_dvg = T::from(1.2).unwrap() * u_vp * is_vg_pos / (denom_ig * denom_ig);

            v_ig = f1 * f2;
            dv_ig_dvp = f1 * df2_dvp;
            dv_ig_dvg = df1_dvg * f2 + f1 * df2_dvg;
        }

        // --- Calculate IAK ---
        let ik_minus_ig = v_ik - v_ig;
        let is_ik_minus_ig_pos = if ik_minus_ig > zero { one } else { zero };
        let term1 = ik_minus_ig.max(zero);
        let d_term1_dvp = is_ik_minus_ig_pos * (dv_ik_dvp - dv_ig_dvp);
        let d_term1_dvg = is_ik_minus_ig_pos * (dv_ik_dvg - dv_ig_dvg);

        let vp_pow_15 = u_vp.powf(T::from(1.5).unwrap());
        let d_vp_pow_15_dvp = T::from(1.5).unwrap() * u_vp.powf(T::from(0.5).unwrap()) * is_vp_pos;

        let sub_term2 = ik_minus_ig - self.iak_k * vp_pow_15;
        let is_sub_term2_pos = if sub_term2 > zero { one } else { zero };
        let term2 = sub_term2.max(zero);
        let d_term2_dvp = is_sub_term2_pos * (dv_ik_dvp - dv_ig_dvp - self.iak_k * d_vp_pow_15_dvp);
        let d_term2_dvg = is_sub_term2_pos * (dv_ik_dvg - dv_ig_dvg);

        // Total currents & Jacobian entries
        let i_ak = term1 - term2 + T::from(1e-10).unwrap() * v_pk;
        let g_ak_p = d_term1_dvp - d_term2_dvp + T::from(1e-10).unwrap();
        let g_ak_g = d_term1_dvg - d_term2_dvg;

        let i_gk = v_ig;
        let g_gk_p = dv_ig_dvp;
        let g_gk_g = dv_ig_dvg;

        // --- Newton-Raphson Companion Model Equivalents ---
        let i_eq_ak = i_ak - (g_ak_p * v_pk) - (g_ak_g * v_gk);
        let i_eq_gk = i_gk - (g_gk_p * v_pk) - (g_gk_g * v_gk);

        // --- Stamping ---
        stamp_conductance(matrix, idx_p, idx_c, g_ak_p, l_size);
        stamp_transconductance(matrix, idx_p, idx_c, idx_g, idx_c, g_ak_g, l_size);
        stamp_current_source(rhs, idx_p, idx_c, i_eq_ak, l_size);

        stamp_conductance(matrix, idx_g, idx_c, g_gk_g, l_size);
        stamp_transconductance(matrix, idx_g, idx_c, idx_p, idx_c, g_gk_p, l_size);
        stamp_current_source(rhs, idx_g, idx_c, i_eq_gk, l_size);
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g = get_voltage(current_node_voltages, self.cached_idx_g);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);

        // Advance BDF2 history states
        self.v_cgc_m2 = self.v_cgc_m1;
        self.v_cgp_m2 = self.v_cgp_m1;
        self.v_cpc_m2 = self.v_cpc_m1;

        self.v_cgc_m1 = v_g - v_c;
        self.v_cgp_m1 = v_g - v_p;
        self.v_cpc_m1 = v_p - v_c;

        self.iter_state.set(PhysicalTriodeIterState {
            prev_v_p: v_p,
            prev_v_g: v_g,
            prev_v_c: v_c,
        });
    }

    /// Called after each newton raphson iteration to check if the solution has converged.
    fn is_converged(&self, current_node_voltages: &ColRef<T>) -> bool {
        let state = self.iter_state.get();
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g = get_voltage(current_node_voltages, self.cached_idx_g);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);

        let reltol = T::from(1e-3).unwrap(); // Relative tolerance 0.1%
        let vntol = T::from(1e-6).unwrap(); // 1uV absolute error floor

        let check_node = |v_new: T, v_old: T| -> bool {
            let diff = (v_new - v_old).abs();
            let max_val = v_new.abs().max(v_old.abs());
            let bound = vntol + reltol * max_val;
            diff < bound
        };

        check_node(v_p, state.prev_v_p)
            && check_node(v_g, state.prev_v_g)
            && check_node(v_c, state.prev_v_c)
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_p = get_voltage(node_voltages, self.cached_idx_p);
        let v_g = get_voltage(node_voltages, self.cached_idx_g);
        let v_c = get_voltage(node_voltages, self.cached_idx_c);

        let v_pk = v_p - v_c;
        let v_gk = v_g - v_c;
        let v_gg = v_gk + self.v_ct; // REPLACED

        let zero = T::zero();

        // --- Non-linear DC currents (stripped of derivatives) ---
        let u_vp = v_pk.max(zero);
        let u_vg = v_gk.max(zero);

        let m1_base = u_vp + T::from(1e-10).unwrap();
        let m1_inner = self.m1_k * m1_base;
        let v_m1 = m1_inner.powf(self.m1_ex);

        let m2_inner_val = v_gg + u_vp / self.m2_mu;
        let m2_base = m2_inner_val.max(zero) + T::from(1e-10).unwrap();
        let m2_inner = self.m2_k * m2_base;
        let v_m2 = m2_inner.powf(self.m2_ex);

        let p_inner = v_gg + u_vp / self.p_mu;
        let p_base = p_inner.max(zero) + T::from(1e-10).unwrap();
        let v_p_node = self.p_k * p_base.powf(T::from(1.5).unwrap());

        let v_ik = if v_gg > zero {
            v_p_node
        } else {
            self.ik_k * v_m1 * v_m2
        };

        let denom_ig = u_vp + u_vg;
        let v_ig = if denom_ig > T::from(1e-16).unwrap() {
            let f1 = self.ig_k * u_vg.powf(T::from(1.5).unwrap());
            let f2 = (u_vg / denom_ig) * T::from(1.2).unwrap() + T::from(0.4).unwrap();
            f1 * f2
        } else {
            zero
        };

        let ik_minus_ig = v_ik - v_ig;
        let term1 = ik_minus_ig.max(zero);

        let vp_pow_15 = u_vp.powf(T::from(1.5).unwrap());
        let sub_term2 = ik_minus_ig - self.iak_k * vp_pow_15;
        let term2 = sub_term2.max(zero);

        let i_ak = term1 - term2 + T::from(1e-10).unwrap() * v_pk;
        let i_gk = v_ig;

        // --- Transient Capacitor Currents (BDF2) ---
        let mut i_cgp_total = zero;
        let mut i_cgc_total = zero;
        let mut i_cpc_total = zero;

        if !ctx.is_dc_analysis {
            let two = T::from(2.0).unwrap();
            let three = T::from(3.0).unwrap();
            let four = T::from(4.0).unwrap();
            let dt2 = two * ctx.dt;

            let v_gp = v_g - v_p;

            let g_eq_cgp = (three * self.c_ga) / dt2;
            let i_eq_cgp = (self.c_ga / dt2) * (four * self.v_cgp_m1 - self.v_cgp_m2);
            i_cgp_total = g_eq_cgp * v_gp - i_eq_cgp;

            let g_eq_cgc = (three * self.c_gk) / dt2;
            let i_eq_cgc = (self.c_gk / dt2) * (four * self.v_cgc_m1 - self.v_cgc_m2);
            i_cgc_total = g_eq_cgc * v_gk - i_eq_cgc;

            let g_eq_cpc = (three * self.c_pk) / dt2;
            let i_eq_cpc = (self.c_pk / dt2) * (four * self.v_cpc_m1 - self.v_cpc_m2);
            i_cpc_total = g_eq_cpc * v_pk - i_eq_cpc;
        }

        out_currents[0] = i_ak + i_cpc_total - i_cgp_total;
        out_currents[1] = i_gk + i_cgc_total + i_cgp_total;
        out_currents[2] = -i_ak - i_gk - i_cpc_total - i_cgc_total;
    }
}
