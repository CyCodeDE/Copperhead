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
pub struct PhysicalPentodeDef {
    pub model: PhysicalPentodeModel,
}

impl<T: CircuitScalar> Instantiable<T> for PhysicalPentodeDef {
    fn instantiate(&self, nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let (
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
            ig2t_k,
            ik3_k,
            ip_k,
            c_ga,
            c_gk,
            c_g1g2,
            c_pk,
        ) = self.model.parameters();
        let comp = PhysicalPentode::new(
            nodes[0], nodes[2], nodes[1],
            nodes[3], // notice the order of g1 and g2 is swapped because I am too lazy to refactor the order
            v_ct, m1_k, m1_ex, m2_k, m2_mu, m2_ex, p_k, p_mu, ik_k, ig_k, ig2t_k, ik3_k, ip_k,
            c_ga, c_gk, c_g1g2, c_pk,
        );
        circuit.add_component(comp);
    }
}

/// Internal state for the Pentode during Newton-Raphson iterations.
#[derive(Clone, Copy, Debug)]
struct PhysicalPentodeIterState<T: CircuitScalar> {
    pub prev_v_p: T,
    pub prev_v_g2: T,
    pub prev_v_g1: T,
    pub prev_v_c: T,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum PhysicalPentodeModel {
    _6L6,
    EL34,
}

impl PhysicalPentodeModel {
    /// Returns: v_ct, m1_k, m1_ex, m2_k, m2_mu, m2_ex, p_k, p_mu, ik_k, ig_k, ig2t_k, ik3_k, ip_k, c_ga, c_gk, c_g1g2, c_pk
    pub fn parameters<T: CircuitScalar>(
        &self,
    ) -> (T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T) {
        match self {
            PhysicalPentodeModel::_6L6 => (
                T::from(0.91804059).unwrap(),    // v_ct
                T::from(0.10751078).unwrap(),    // m1_k
                T::from(-1.743575).unwrap(),     // m1_ex
                T::from(0.4624527).unwrap(),     // m2_k
                T::from(4.9999386).unwrap(),     // m2_mu
                T::from(3.243575).unwrap(),      // m2_ex
                T::from(0.0016883841).unwrap(),  // p_k
                T::from(10.811784).unwrap(),     // p_mu
                T::from(0.0021948901).unwrap(),  // ik_k
                T::from(0.0022135943).unwrap(),  // ig_k
                T::from(0.942171668).unwrap(),   // ig2t_k
                T::from(2180.0).unwrap(),        // ik3_k
                T::from(0.00056920996).unwrap(), // ip_k
                T::from(0.6e-12).unwrap(),       // c_ga
                T::from(5.7e-12).unwrap(),       // c_gk
                T::from(3.8e-12).unwrap(),       // c_g1g2
                T::from(5.9e-12).unwrap(),       // c_pk
            ),
            PhysicalPentodeModel::EL34 => (
                T::from(0.29360503).unwrap(),   // v_ct
                T::from(0.040003405).unwrap(),  // m1_k
                T::from(-0.73308055).unwrap(),  // m1_ex
                T::from(0.67171782).unwrap(),   // m2_k
                T::from(8.2063559).unwrap(),    // m2_mu
                T::from(2.2330806).unwrap(),    // m2_ex
                T::from(0.0033402929).unwrap(), // p_k
                T::from(12.216969).unwrap(),    // p_mu
                T::from(0.0019762451).unwrap(), // ik_k
                T::from(0.0016701465).unwrap(), // ig_k
                T::from(0.87617414).unwrap(),   // ig2t_k
                T::from(1250.0).unwrap(),       // ik3_k
                T::from(0.0020885491).unwrap(), // ip_k
                T::from(1.1e-12).unwrap(),      // c_ga
                T::from(9.1e-12).unwrap(),      // c_gk
                T::from(6.1e-12).unwrap(),      // c_g1g2
                T::from(8.4e-12).unwrap(),      // c_pk
            ),
        }
    }
}

pub struct PhysicalPentode<T: CircuitScalar> {
    // External Nodes
    pub node_p: NodeId,  // Plate (Anode)
    pub node_g2: NodeId, // Screen Grid
    pub node_g1: NodeId, // Control Grid
    pub node_c: NodeId,  // Cathode

    pub cached_idx_p: Option<usize>,
    pub cached_idx_g2: Option<usize>,
    pub cached_idx_g1: Option<usize>,
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
    pub ig2t_k: T,
    pub ik3_k: T,
    pub ip_k: T,
    pub c_ga: T,
    pub c_gk: T,
    pub c_g1g2: T,
    pub c_pk: T,

    g_min: T,

    // Simulation State
    iter_state: Cell<PhysicalPentodeIterState<T>>,

    // BDF2 history states
    /// Internal capacitor voltage at t[n-1]
    v_cga_m1: T,
    v_cgk_m1: T,
    v_cg1g2_m1: T,
    v_cpk_m1: T,

    /// Internal capacitor voltage at t[n-2]
    v_cga_m2: T,
    v_cgk_m2: T,
    v_cg1g2_m2: T,
    v_cpk_m2: T,
}

impl<T: CircuitScalar> PhysicalPentode<T> {
    pub fn new(
        node_p: NodeId,
        node_g2: NodeId,
        node_g1: NodeId,
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
        ig2t_k: T,
        ik3_k: T,
        ip_k: T,
        c_ga: T,
        c_gk: T,
        c_g1g2: T,
        c_pk: T,
    ) -> Self {
        Self {
            node_p,
            node_g2,
            node_g1,
            node_c,
            cached_idx_p: None,
            cached_idx_g2: None,
            cached_idx_g1: None,
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
            ig2t_k,
            ik3_k,
            ip_k,
            c_ga,
            c_gk,
            c_g1g2,
            c_pk,

            g_min: T::from(1e-9).unwrap(),

            // Simulation State
            iter_state: Cell::new(PhysicalPentodeIterState {
                prev_v_p: T::zero(),
                prev_v_g2: T::zero(),
                prev_v_g1: T::zero(),
                prev_v_c: T::zero(),
            }),

            // BDF2 history states
            v_cga_m1: T::zero(),
            v_cgk_m1: T::zero(),
            v_cg1g2_m1: T::zero(),
            v_cpk_m1: T::zero(),
            v_cga_m2: T::zero(),
            v_cgk_m2: T::zero(),
            v_cg1g2_m2: T::zero(),
            v_cpk_m2: T::zero(),
        }
    }
}

impl<T: CircuitScalar> Component<T> for PhysicalPentode<T> {
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
        let idx_g2 = self.cached_idx_g2;
        let idx_g1 = self.cached_idx_g1;
        let idx_c = self.cached_idx_c;

        let v_plate = get_voltage(current_node_voltages, idx_p);
        let v_g2 = get_voltage(current_node_voltages, idx_g2);
        let v_g1 = get_voltage(current_node_voltages, idx_g1);
        let v_c = get_voltage(current_node_voltages, idx_c);

        let v_pk = v_plate - v_c;
        let v_g2k = v_g2 - v_c;
        let v_g1k = v_g1 - v_c;
        let v_gg = v_g1k + self.v_ct;

        let zero = T::zero();
        let one = T::from(1.0).unwrap();
        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();
        let eps = T::from(1e-10).unwrap();

        // Stamp capacitors (BDF2)
        if !ctx.is_dc_analysis {
            let dt2 = two * ctx.dt;

            let g_eq_cga = (three * self.c_ga) / dt2;
            let g_eq_cgk = (three * self.c_gk) / dt2;
            let g_eq_cg1g2 = (three * self.c_g1g2) / dt2;
            let g_eq_cpk = (three * self.c_pk) / dt2;

            stamp_conductance(matrix, idx_g1, idx_p, g_eq_cga, l_size);
            stamp_conductance(matrix, idx_g1, idx_c, g_eq_cgk, l_size);
            stamp_conductance(matrix, idx_g1, idx_g2, g_eq_cg1g2, l_size);
            stamp_conductance(matrix, idx_p, idx_c, g_eq_cpk, l_size);

            let i_eq_cga = (self.c_ga / dt2) * (four * self.v_cga_m1 - self.v_cga_m2);
            let i_eq_cgk = (self.c_gk / dt2) * (four * self.v_cgk_m1 - self.v_cgk_m2);
            let i_eq_cg1g2 = (self.c_g1g2 / dt2) * (four * self.v_cg1g2_m1 - self.v_cg1g2_m2);
            let i_eq_cpk = (self.c_pk / dt2) * (four * self.v_cpk_m1 - self.v_cpk_m2);

            if let Some(i) = idx_g1 {
                rhs[i - l_size] = rhs[i - l_size] + i_eq_cga + i_eq_cgk + i_eq_cg1g2;
            }
            if let Some(i) = idx_g2 {
                rhs[i - l_size] = rhs[i - l_size] - i_eq_cg1g2;
            }
            if let Some(i) = idx_p {
                rhs[i - l_size] = rhs[i - l_size] - i_eq_cga + i_eq_cpk;
            }
            if let Some(i) = idx_c {
                rhs[i - l_size] = rhs[i - l_size] - i_eq_cgk - i_eq_cpk;
            }
        }

        // URAMP evaluations
        let u_vp = v_pk.max(zero);
        let u_vg2 = v_g2k.max(zero);
        let u_vg1 = v_g1k.max(zero);

        // URAMP derivatives
        let is_vp_pos = if v_pk > zero { one } else { zero };
        let is_vg2_pos = if v_g2k > zero { one } else { zero };
        let is_vg1_pos = if v_g1k > zero { one } else { zero };

        // Calculate M1
        let m1_base = u_vg2 + eps;
        let m1_inner = self.m1_k * m1_base;
        let v_m1 = m1_inner.powf(self.m1_ex);
        let dv_m1_dvg2 = (self.m1_ex * self.m1_k) * m1_inner.powf(self.m1_ex - one) * is_vg2_pos;

        // Calculate M2
        let m2_inner_val = v_gg + u_vg2 / self.m2_mu;
        let is_m2_inner_pos = if m2_inner_val > zero { one } else { zero };
        let m2_base = m2_inner_val.max(zero) + eps;
        let m2_inner = self.m2_k * m2_base;
        let v_m2 = m2_inner.powf(self.m2_ex);

        let dv_m2_base =
            (self.m2_ex * self.m2_k) * m2_inner.powf(self.m2_ex - one) * is_m2_inner_pos;
        let dv_m2_dvg1 = dv_m2_base;
        let dv_m2_dvg2 = dv_m2_base * (is_vg2_pos / self.m2_mu);

        // Calculate Node P
        let p_inner = v_gg + u_vg2 / self.p_mu;
        let is_p_inner_pos = if p_inner > zero { one } else { zero };
        let p_base = p_inner.max(zero) + eps;
        let v_p = self.p_k * p_base.powf(T::from(1.5).unwrap());

        let dv_p_base = (self.p_k * T::from(1.5).unwrap())
            * p_base.powf(T::from(0.5).unwrap())
            * is_p_inner_pos;
        let dv_p_dvg1 = dv_p_base;
        let dv_p_dvg2 = dv_p_base * (is_vg2_pos / self.p_mu);

        // Calculate IK
        let v_ik;
        let dv_ik_dvg2;
        let dv_ik_dvg1;

        if v_gg > zero {
            v_ik = v_p;
            dv_ik_dvg2 = dv_p_dvg2;
            dv_ik_dvg1 = dv_p_dvg1;
        } else {
            let coeff = self.ik_k;
            v_ik = coeff * v_m1 * v_m2;
            dv_ik_dvg2 = coeff * (dv_m1_dvg2 * v_m2 + v_m1 * dv_m2_dvg2);
            dv_ik_dvg1 = coeff * (v_m1 * dv_m2_dvg1);
        }

        // Calculate IG
        let denom_ig = u_vp + u_vg1;
        let mut v_ig = zero;
        let mut dv_ig_dvp = zero;
        let mut dv_ig_dvg1 = zero;

        if denom_ig > T::from(1e-16).unwrap() {
            let f1 = self.ig_k * u_vg1.powf(T::from(1.5).unwrap());
            let df1_dvg1 = (self.ig_k * T::from(1.5).unwrap())
                * u_vg1.powf(T::from(0.5).unwrap())
                * is_vg1_pos;
            let f2 = (u_vg1 / denom_ig) * T::from(1.2).unwrap() + T::from(0.4).unwrap();
            let df2_dvp = -T::from(1.2).unwrap() * u_vg1 / (denom_ig * denom_ig) * is_vp_pos;
            let df2_dvg1 = T::from(1.2).unwrap() * u_vp * is_vg1_pos / (denom_ig * denom_ig);

            v_ig = f1 * f2;
            dv_ig_dvp = f1 * df2_dvp;
            dv_ig_dvg1 = df1_dvg1 * f2 + f1 * df2_dvg1;
        }

        // Calculate IK2
        let ik_minus_ig = v_ik - v_ig;
        let dv_ik_minus_ig_dvp = -dv_ig_dvp;
        let dv_ik_minus_ig_dvg2 = dv_ik_dvg2;
        let dv_ik_minus_ig_dvg1 = dv_ik_dvg1 - dv_ig_dvg1;

        let exp_arg = -T::from(15.0).unwrap() * u_vp / (u_vg2 + eps);
        let f_ik2 = one - T::from(0.4).unwrap() * (exp_arg.exp() - (-T::from(15.0).unwrap()).exp());

        let d_exp_arg_dvp = -T::from(15.0).unwrap() / (u_vg2 + eps) * is_vp_pos;
        let d_exp_arg_dvg2 =
            T::from(15.0).unwrap() * u_vp / ((u_vg2 + eps) * (u_vg2 + eps)) * is_vg2_pos;

        let d_fik2_dvp = -T::from(0.4).unwrap() * exp_arg.exp() * d_exp_arg_dvp;
        let d_fik2_dvg2 = -T::from(0.4).unwrap() * exp_arg.exp() * d_exp_arg_dvg2;

        let v_ik2 = ik_minus_ig * f_ik2;
        let dv_ik2_dvp = dv_ik_minus_ig_dvp * f_ik2 + ik_minus_ig * d_fik2_dvp;
        let dv_ik2_dvg2 = dv_ik_minus_ig_dvg2 * f_ik2 + ik_minus_ig * d_fik2_dvg2;
        let dv_ik2_dvg1 = dv_ik_minus_ig_dvg1 * f_ik2;

        // Calculate IG2T
        let t_g2t_inner = u_vp + T::from(10.0).unwrap();
        let t_g2t_base = one - u_vp / t_g2t_inner;
        let d_tg2t_base_dvp = -T::from(10.0).unwrap() / (t_g2t_inner * t_g2t_inner) * is_vp_pos;

        let is_tg2t_pos = if t_g2t_base > zero { one } else { zero };
        let f_g2t =
            self.ig2t_k * t_g2t_base.max(zero).powf(T::from(1.5).unwrap()) + (one - self.ig2t_k);
        let d_fg2t_dvp = self.ig2t_k
            * T::from(1.5).unwrap()
            * t_g2t_base.max(zero).powf(T::from(0.5).unwrap())
            * d_tg2t_base_dvp
            * is_tg2t_pos;

        let v_ig2t = v_ik2 * f_g2t;
        let dv_ig2t_dvp = dv_ik2_dvp * f_g2t + v_ik2 * d_fg2t_dvp;
        let dv_ig2t_dvg2 = dv_ik2_dvg2 * f_g2t;
        let dv_ig2t_dvg1 = dv_ik2_dvg1 * f_g2t;

        // Calculate IK3
        let ik3_denom = u_vg2 + self.ik3_k;
        let f_ik3 = (u_vp + self.ik3_k) / ik3_denom;
        let d_fik3_dvp = one / ik3_denom * is_vp_pos;
        let d_fik3_dvg2 = -(u_vp + self.ik3_k) / (ik3_denom * ik3_denom) * is_vg2_pos;

        let v_ik3 = v_ik2 * f_ik3;
        let dv_ik3_dvp = dv_ik2_dvp * f_ik3 + v_ik2 * d_fik3_dvp;
        let dv_ik3_dvg2 = dv_ik2_dvg2 * f_ik3 + v_ik2 * d_fik3_dvg2;
        let dv_ik3_dvg1 = dv_ik2_dvg1 * f_ik3;

        // Calculate IK4
        let diff_vg2_vp = u_vg2 - u_vp;
        let is_vg2_gt_vp = if diff_vg2_vp > zero { one } else { zero };
        let v_eff = u_vp + diff_vg2_vp.max(zero);

        let d_veff_dvp = is_vp_pos * (one - is_vg2_gt_vp);
        let d_veff_dvg2 = is_vg2_gt_vp * is_vg2_pos;

        let i_lim4 = self.ip_k * v_eff.powf(T::from(1.5).unwrap());
        let d_ilim4_dvp =
            self.ip_k * T::from(1.5).unwrap() * v_eff.powf(T::from(0.5).unwrap()) * d_veff_dvp;
        let d_ilim4_dvg2 =
            self.ip_k * T::from(1.5).unwrap() * v_eff.powf(T::from(0.5).unwrap()) * d_veff_dvg2;

        let x4 = v_ik3 - i_lim4;
        let is_x4_pos = if x4 > zero { one } else { zero };
        let v_ik4 = v_ik3 - x4.max(zero);

        let dv_ik4_dvp = dv_ik3_dvp - is_x4_pos * (dv_ik3_dvp - d_ilim4_dvp);
        let dv_ik4_dvg2 = dv_ik3_dvg2 - is_x4_pos * (dv_ik3_dvg2 - d_ilim4_dvg2);
        let dv_ik4_dvg1 = dv_ik3_dvg1 - is_x4_pos * dv_ik3_dvg1;

        // Calculate IP
        let i_p0 = v_ik4 - v_ig2t;
        let d_ip0_dvp = dv_ik4_dvp - dv_ig2t_dvp;
        let d_ip0_dvg2 = dv_ik4_dvg2 - dv_ig2t_dvg2;
        let d_ip0_dvg1 = dv_ik4_dvg1 - dv_ig2t_dvg1;

        let i_plim = self.ip_k * u_vp.powf(T::from(1.5).unwrap());
        let d_iplim_dvp =
            self.ip_k * T::from(1.5).unwrap() * u_vp.powf(T::from(0.5).unwrap()) * is_vp_pos;

        let xp = i_p0 - i_plim;
        let is_xp_pos = if xp > zero { one } else { zero };
        let i_p_inner = i_p0 - xp.max(zero);
        let is_ip_inner_pos = if i_p_inner > zero { one } else { zero };
        let v_ip = i_p_inner.max(zero);

        let d_ipinner_dvp = d_ip0_dvp - is_xp_pos * (d_ip0_dvp - d_iplim_dvp);
        let d_ipinner_dvg2 = d_ip0_dvg2 - is_xp_pos * d_ip0_dvg2;
        let d_ipinner_dvg1 = d_ip0_dvg1 - is_xp_pos * d_ip0_dvg1;

        let dv_ip_dvp = is_ip_inner_pos * d_ipinner_dvp;
        let dv_ip_dvg2 = is_ip_inner_pos * d_ipinner_dvg2;
        let dv_ip_dvg1 = is_ip_inner_pos * d_ipinner_dvg1;

        // --- Total Currents & Jacobian Entries ---
        let i_ak = v_ip + eps * v_pk;
        let g_ak_p = dv_ip_dvp + eps;
        let g_ak_g2 = dv_ip_dvg2;
        let g_ak_g1 = dv_ip_dvg1;

        let i_g20 = v_ik4 - v_ip;
        let is_ig20_pos = if i_g20 > zero { one } else { zero };
        let i_g2 = i_g20.max(zero);
        let g_g2_p = is_ig20_pos * (dv_ik4_dvp - dv_ip_dvp);
        let g_g2_g2 = is_ig20_pos * (dv_ik4_dvg2 - dv_ip_dvg2);
        let g_g2_g1 = is_ig20_pos * (dv_ik4_dvg1 - dv_ip_dvg1);

        let i_g1 = v_ig;
        let g_g1_p = dv_ig_dvp;
        let g_g1_g1 = dv_ig_dvg1;

        let i_eq_ak = i_ak - (g_ak_p * v_pk) - (g_ak_g2 * v_g2k) - (g_ak_g1 * v_g1k);
        let i_eq_g2 = i_g2 - (g_g2_p * v_pk) - (g_g2_g2 * v_g2k) - (g_g2_g1 * v_g1k);
        let i_eq_g1 = i_g1 - (g_g1_p * v_pk) - (g_g1_g1 * v_g1k);

        stamp_conductance(matrix, idx_p, idx_c, g_ak_p, l_size);
        stamp_transconductance(matrix, idx_p, idx_c, idx_g2, idx_c, g_ak_g2, l_size);
        stamp_transconductance(matrix, idx_p, idx_c, idx_g1, idx_c, g_ak_g1, l_size);
        stamp_current_source(rhs, idx_p, idx_c, i_eq_ak, l_size);

        stamp_conductance(matrix, idx_g2, idx_c, g_g2_g2, l_size);
        stamp_transconductance(matrix, idx_g2, idx_c, idx_p, idx_c, g_g2_p, l_size);
        stamp_transconductance(matrix, idx_g2, idx_c, idx_g1, idx_c, g_g2_g1, l_size);
        stamp_current_source(rhs, idx_g2, idx_c, i_eq_g2, l_size);

        stamp_conductance(matrix, idx_g1, idx_c, g_g1_g1, l_size);
        stamp_transconductance(matrix, idx_g1, idx_c, idx_p, idx_c, g_g1_p, l_size);
        stamp_current_source(rhs, idx_g1, idx_c, i_eq_g1, l_size);
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g2 = get_voltage(current_node_voltages, self.cached_idx_g2);
        let v_g1 = get_voltage(current_node_voltages, self.cached_idx_g1);
        let v_c = get_voltage(current_node_voltages, self.cached_idx_c);

        // Advance BDF2 history states
        self.v_cga_m2 = self.v_cga_m1;
        self.v_cgk_m2 = self.v_cgk_m1;
        self.v_cg1g2_m2 = self.v_cg1g2_m1;
        self.v_cpk_m2 = self.v_cpk_m1;

        self.v_cga_m1 = v_g1 - v_p;
        self.v_cgk_m1 = v_g1 - v_c;
        self.v_cg1g2_m1 = v_g1 - v_g2;
        self.v_cpk_m1 = v_p - v_c;

        self.iter_state.set(PhysicalPentodeIterState {
            prev_v_p: v_p,
            prev_v_g2: v_g2,
            prev_v_g1: v_g1,
            prev_v_c: v_c,
        });
    }

    /// Called after each newton raphson iteration to check if the solution has converged.
    fn is_converged(&self, current_node_voltages: &ColRef<T>) -> bool {
        let state = self.iter_state.get();
        let v_p = get_voltage(current_node_voltages, self.cached_idx_p);
        let v_g2 = get_voltage(current_node_voltages, self.cached_idx_g2);
        let v_g1 = get_voltage(current_node_voltages, self.cached_idx_g1);
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
            && check_node(v_g2, state.prev_v_g2)
            && check_node(v_g1, state.prev_v_g1)
            && check_node(v_c, state.prev_v_c)
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_p = get_voltage(node_voltages, self.cached_idx_p);
        let v_g2 = get_voltage(node_voltages, self.cached_idx_g2);
        let v_g1 = get_voltage(node_voltages, self.cached_idx_g1);
        let v_c = get_voltage(node_voltages, self.cached_idx_c);

        let v_pk = v_p - v_c;
        let v_g2k = v_g2 - v_c;
        let v_g1k = v_g1 - v_c;
        let v_gg = v_g1k + self.v_ct;

        let zero = T::zero();
        let one = T::from(1.0).unwrap();
        let eps = T::from(1e-10).unwrap();

        // Non-linear DC currents
        let u_vp = v_pk.max(zero);
        let u_vg2 = v_g2k.max(zero);
        let u_vg1 = v_g1k.max(zero);

        let m1_base = u_vg2 + eps;
        let m1_inner = self.m1_k * m1_base;
        let v_m1 = m1_inner.powf(self.m1_ex);

        let m2_inner_val = v_gg + u_vg2 / self.m2_mu;
        let m2_base = m2_inner_val.max(zero) + eps;
        let m2_inner = self.m2_k * m2_base;
        let v_m2 = m2_inner.powf(self.m2_ex);

        let p_inner = v_gg + u_vg2 / self.p_mu;
        let p_base = p_inner.max(zero) + eps;
        let v_p_node = self.p_k * p_base.powf(T::from(1.5).unwrap());

        let v_ik = if v_gg > zero {
            v_p_node
        } else {
            self.ik_k * v_m1 * v_m2
        };

        let denom_ig = u_vp + u_vg1;
        let v_ig = if denom_ig > T::from(1e-16).unwrap() {
            let f1 = self.ig_k * u_vg1.powf(T::from(1.5).unwrap());
            let f2 = (u_vg1 / denom_ig) * T::from(1.2).unwrap() + T::from(0.4).unwrap();
            f1 * f2
        } else {
            zero
        };

        let ik_minus_ig = v_ik - v_ig;

        let exp_arg = -T::from(15.0).unwrap() * u_vp / (u_vg2 + eps);
        let f_ik2 = one - T::from(0.4).unwrap() * (exp_arg.exp() - (-T::from(15.0).unwrap()).exp());
        let v_ik2 = ik_minus_ig * f_ik2;

        let t_g2t_inner = u_vp + T::from(10.0).unwrap();
        let t_g2t_base = one - u_vp / t_g2t_inner;
        let f_g2t =
            self.ig2t_k * t_g2t_base.max(zero).powf(T::from(1.5).unwrap()) + (one - self.ig2t_k);
        let v_ig2t = v_ik2 * f_g2t;

        let ik3_denom = u_vg2 + self.ik3_k;
        let f_ik3 = (u_vp + self.ik3_k) / ik3_denom;
        let v_ik3 = v_ik2 * f_ik3;

        let diff_vg2_vp = u_vg2 - u_vp;
        let v_eff = u_vp + diff_vg2_vp.max(zero);
        let i_lim4 = self.ip_k * v_eff.powf(T::from(1.5).unwrap());

        let x4 = v_ik3 - i_lim4;
        let v_ik4 = v_ik3 - x4.max(zero);

        let i_p0 = v_ik4 - v_ig2t;
        let i_plim = self.ip_k * u_vp.powf(T::from(1.5).unwrap());

        let xp = i_p0 - i_plim;
        let i_p_inner = i_p0 - xp.max(zero);
        let v_ip = i_p_inner.max(zero);

        let i_ak = v_ip + eps * v_pk;
        let i_g20 = v_ik4 - v_ip;
        let i_g2_dc = i_g20.max(zero);
        let i_g1_dc = v_ig;

        // Capacitor currents (BDF2)
        let mut i_cga_total = zero;
        let mut i_cgk_total = zero;
        let mut i_cg1g2_total = zero;
        let mut i_cpk_total = zero;

        if !ctx.is_dc_analysis {
            let two = T::from(2.0).unwrap();
            let three = T::from(3.0).unwrap();
            let four = T::from(4.0).unwrap();
            let dt2 = two * ctx.dt;

            let v_ga = v_g1 - v_p;
            let v_g1g2 = v_g1 - v_g2;

            let g_eq_cga = (three * self.c_ga) / dt2;
            let i_eq_cga = (self.c_ga / dt2) * (four * self.v_cga_m1 - self.v_cga_m2);
            i_cga_total = g_eq_cga * v_ga - i_eq_cga;

            let g_eq_cgk = (three * self.c_gk) / dt2;
            let i_eq_cgk = (self.c_gk / dt2) * (four * self.v_cgk_m1 - self.v_cgk_m2);
            i_cgk_total = g_eq_cgk * v_g1k - i_eq_cgk;

            let g_eq_cg1g2 = (three * self.c_g1g2) / dt2;
            let i_eq_cg1g2 = (self.c_g1g2 / dt2) * (four * self.v_cg1g2_m1 - self.v_cg1g2_m2);
            i_cg1g2_total = g_eq_cg1g2 * v_g1g2 - i_eq_cg1g2;

            let g_eq_cpk = (three * self.c_pk) / dt2;
            let i_eq_cpk = (self.c_pk / dt2) * (four * self.v_cpk_m1 - self.v_cpk_m2);
            i_cpk_total = g_eq_cpk * v_pk - i_eq_cpk;
        }

        out_currents[0] = i_ak + i_cpk_total - i_cga_total; // Plate
        out_currents[1] = i_g1_dc + i_cgk_total + i_cga_total + i_cg1g2_total; // G1
        out_currents[2] = i_g2_dc - i_cg1g2_total; // G2
        out_currents[3] = -out_currents[0] - out_currents[1] - out_currents[2]; // Cathode
    }
}
