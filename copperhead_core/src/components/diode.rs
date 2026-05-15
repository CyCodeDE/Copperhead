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
use crate::circuit::Circuit;
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::parameter::{ComponentEvalCtx, ParamValue, RebuildKind};
use crate::util::math;
use crate::util::mna::{stamp_conductance, stamp_current_source};
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::cell::Cell;
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct DiodeDef {
    pub model: DiodeModel,
}

impl<T: CircuitScalar> Instantiable<T> for DiodeDef {
    fn instantiate(&self, nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let (is, n, rs, cjo, m, tt, bv, ibv) = self.model.parameters_f64();
        let comp = Diode::<T>::new(
            nodes[0],
            nodes[1],
            ParamValue::Constant(is),
            ParamValue::Constant(n),
            T::from(rs).unwrap(),
            ParamValue::Constant(cjo),
            ParamValue::Constant(m),
            ParamValue::Constant(tt),
            ParamValue::Constant(bv),
            ParamValue::Constant(ibv),
        );
        circuit.add_component(comp);
    }
}

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
    _1N4148,
    _1N4007,
}

impl DiodeModel {
    pub fn parameters<T: CircuitScalar>(&self) -> (T, T, T, T, T, T, T, T) {
        let (is, n, rs, cjo, m, tt, bv, ibv) = self.parameters_f64();
        (
            T::from(is).unwrap(),
            T::from(n).unwrap(),
            T::from(rs).unwrap(),
            T::from(cjo).unwrap(),
            T::from(m).unwrap(),
            T::from(tt).unwrap(),
            T::from(bv).unwrap(),
            T::from(ibv).unwrap(),
        )
    }

    pub fn parameters_f64(&self) -> (f64, f64, f64, f64, f64, f64, f64, f64) {
        match self {
            DiodeModel::_1N4148 => (
                2.52e-9, // Saturation Current | typical 2.52nA
                1.752,   // Emission Coefficient | typical 1.752
                0.568,   // Series Resistance | typical 0.568 Ohms
                4e-12,   // Zero-bias junction capacitance | typical 4pF
                0.4,     // Grading coefficient | typical 0.4
                20e-9,   // Transit time | typical 20ns
                75.0,    // breakdown voltage
                5.0e-6,  // breakdown current at BV | typical 5µA
            ),
            DiodeModel::_1N4007 => (
                4.352e-9,  // Saturation Current | typical 4.352nA
                1.906,     // Emission Coefficient | typical 1.906
                0.6458,    // Series Resistance | typical 0.6458 Ohms
                26.08e-12, // Zero-bias junction capacitance | typical 26.08pF
                0.3294,    // Grading coefficient | typical 0.3294
                5.7e-6,    // Transit time | typical 5.7µs
                1000.0,    // Breakdown voltage | 1000V rated
                5.0e-6,    // Breakdown current at BV | typical 5µA
            ),
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            DiodeModel::_1N4148 => "1N4148",
            DiodeModel::_1N4007 => "1N4007",
        }
    }
}
pub struct Diode<T: CircuitScalar> {
    pub node_a: NodeId,
    pub node_b: NodeId,

    // Cached matrix indices (populated in bake_indices)
    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    // User-facing parameters. May be constants or formulas.
    pub saturation_current: ParamValue,   // Is
    pub emission_coefficient: ParamValue, // N
    pub cjo: ParamValue,                  // Zero-bias junction capacitance
    pub m: ParamValue,                    // Grading coefficient
    pub transit_time: ParamValue,         // Transit time (tt)
    pub bv: ParamValue,                   // Breakdown voltage
    pub ibv: ParamValue,                  // Current at Breakdown Voltage

    /// Series resistance is kept as a plain T for now: making it formula-
    /// driven would let it toggle between zero (no aux row) and non-zero
    /// (an aux row in the matrix), changing the matrix shape on the fly.
    /// That's a re-partition trigger we'd rather model explicitly.
    pub series_resistance: T, // Rs

    // Cached T-typed copies of the ParamValues, refreshed by
    // refresh_per_step / refresh_per_iter. The hot path (stamp_nonlinear,
    // calculate_operating_point) reads from these, never from the
    // ParamValues directly.
    is_t: T,
    n_t: T,
    cjo_t: T,
    m_t: T,
    tt_t: T,
    bv_t: T,
    ibv_t: T,

    pub vt: T,                 // Thermal voltage (kT/q)
    pub junction_potential: T, // phi
    pub fc: T,                 // Forward-bias depletion capacitance coefficient (typ 0.5)

    /// Voltage across the intrinsic diode at the end of the previous time step
    prev_voltage: T,

    /// Total charge at t[n-1]
    q_m1: T,

    /// Total charge at t[n-2]
    q_m2: T,

    internal_node_idx: Option<usize>,

    g_min: T,
    /// Critical voltage for limiting algorithm. Recomputed by refresh
    /// whenever Is or N changes.
    v_crit: T,

    iter_state: Cell<IterationState<T>>,
}

impl<T: CircuitScalar> Diode<T> {
    pub fn new(
        node_a: NodeId,
        node_b: NodeId,
        is: ParamValue,
        n: ParamValue,
        rs: T,
        cjo: ParamValue,
        m: ParamValue,
        tt: ParamValue,
        bv: ParamValue,
        ibv: ParamValue,
    ) -> Self {
        let vt = T::from(0.02585).unwrap(); // ~300K thermal voltage at room temp (kT/q)
        // TODO: calculate the temperature dynamically based on the environment or allow user to set it, and calculate vt = kT/q accordingly

        // Initial cached scalar copies. Formula-driven values will be
        // overwritten by the first refresh_per_step / refresh_per_iter.
        let is_t = T::from(is.as_constant().unwrap_or(2.52e-9)).unwrap();
        let n_t = T::from(n.as_constant().unwrap_or(1.0)).unwrap();
        let cjo_t = T::from(cjo.as_constant().unwrap_or(0.0)).unwrap();
        let m_t = T::from(m.as_constant().unwrap_or(0.5)).unwrap();
        let tt_t = T::from(tt.as_constant().unwrap_or(0.0)).unwrap();
        let bv_t = T::from(bv.as_constant().unwrap_or(75.0)).unwrap();
        let ibv_t = T::from(ibv.as_constant().unwrap_or(5.0e-6)).unwrap();

        let v_crit = Self::compute_v_crit(is_t, n_t, vt);

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
            bv,
            ibv,

            is_t,
            n_t,
            cjo_t,
            m_t,
            tt_t,
            bv_t,
            ibv_t,

            vt,
            junction_potential: T::from(0.7).unwrap(),
            fc: T::from(0.5).unwrap(),

            prev_voltage: T::zero(),

            q_m1: T::zero(),
            q_m2: T::zero(),

            internal_node_idx: None,
            g_min: T::from(1.0e-12).unwrap(), // 1pA/V leakage
            v_crit,

            iter_state: Cell::new(IterationState {
                last_iter_voltage: T::zero(),
                last_iter_current: T::zero(),
                is_converged: false,
            }),
        }
    }

    fn compute_v_crit(is_t: T, n_t: T, vt: T) -> T {
        let vt_n = n_t * vt;
        let sqrt_2 = T::from(2.0).unwrap().sqrt();
        vt_n * (vt_n / (is_t * sqrt_2)).ln()
    }

    /// Re-evaluate the cached T-typed parameter copies from the current
    /// ParamValue states. Cheap (each formula eval is ~tens of ns).
    fn refresh_cached(&mut self, eval: &ComponentEvalCtx) {
        self.is_t = T::from(self.saturation_current.eval(eval)).unwrap();
        self.n_t = T::from(self.emission_coefficient.eval(eval)).unwrap();
        self.cjo_t = T::from(self.cjo.eval(eval)).unwrap();
        self.m_t = T::from(self.m.eval(eval)).unwrap();
        self.tt_t = T::from(self.transit_time.eval(eval)).unwrap();
        self.bv_t = T::from(self.bv.eval(eval)).unwrap();
        self.ibv_t = T::from(self.ibv.eval(eval)).unwrap();
        self.v_crit = Self::compute_v_crit(self.is_t, self.n_t, self.vt);
    }

    fn any_param_dynamic(&self) -> bool {
        self.saturation_current.is_dynamic()
            || self.emission_coefficient.is_dynamic()
            || self.cjo.is_dynamic()
            || self.m.is_dynamic()
            || self.transit_time.is_dynamic()
            || self.bv.is_dynamic()
            || self.ibv.is_dynamic()
    }

    fn any_param_voltage(&self) -> bool {
        self.saturation_current.depends_on_voltage()
            || self.emission_coefficient.depends_on_voltage()
            || self.cjo.depends_on_voltage()
            || self.m.depends_on_voltage()
            || self.transit_time.depends_on_voltage()
            || self.bv.depends_on_voltage()
            || self.ibv.depends_on_voltage()
    }

    /// Determines the index of the Anode of the *Intrinsic* diode
    fn get_intrinsic_anode_idx(&self, _offset: usize) -> Option<usize> {
        if self.series_resistance > T::epsilon() {
            self.internal_node_idx
        } else {
            self.cached_idx_a
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
        let vt_n = self.n_t * self.vt;

        math::pn_junction_limit(v_new, v_old, vt_n, self.v_crit)
    }

    /// Calculates: I_d, G_d, Q_tot, C_tot
    fn calculate_operating_point(&self, v_d: T) -> (T, T, T, T) {
        let one = T::one();

        let vt_n = self.n_t * self.vt;

        let max_exp_arg = T::from(80.0).unwrap();
        let exp_arg = (v_d / vt_n).min(max_exp_arg);

        let evd_minus_one = exp_arg.exp_m1();
        let evd = evd_minus_one + one;

        let i_fw = self.is_t * evd_minus_one;
        let g_fw = (self.is_t / vt_n) * evd;

        let q_diff = self.tt_t * i_fw;
        let c_diff = self.tt_t * g_fw;

        let mut i_dc = i_fw + self.g_min * v_d;
        let mut g_dc = g_fw + self.g_min;

        let v_breakdown_onset = -self.bv_t + (self.n_t * self.vt * T::from(50.0).unwrap());

        // Handle breakdown
        // for performance and floating point safety, only compute when necessary, as it is negligible otherwise
        if v_d < v_breakdown_onset {
            let v_bwd_arg = -(v_d + self.bv_t) / vt_n;

            let v_bwd_arg_clamped = v_bwd_arg.min(max_exp_arg);

            let exp_bwd = v_bwd_arg_clamped.exp();

            let i_bwd = -self.ibv_t * exp_bwd;

            let g_bwd = (self.ibv_t / vt_n) * exp_bwd;

            i_dc = i_dc + i_bwd;
            g_dc = g_dc + g_bwd;
        }

        let phi = self.junction_potential;
        let fc_phi = self.fc * phi;

        let c_jun: T;
        let q_jun: T;

        if v_d < fc_phi {
            // Normal Reverse/Low Forward Bias
            let ratio = v_d / phi;
            let term = one - ratio;

            if (self.m_t - T::from(0.5).unwrap()).abs() < T::epsilon() {
                let s = term.sqrt();
                c_jun = self.cjo_t / s;
                q_jun = self.cjo_t * phi * (one - s) * T::from(2.0).unwrap();
            } else {
                let s = term.powf(-self.m_t);
                c_jun = self.cjo_t * s;

                let one_minus_m = one - self.m_t;
                let s_q = term.powf(one_minus_m);
                q_jun = (self.cjo_t * phi) * (one - s_q) / one_minus_m;
            }
        } else {
            let one_plus_m = one + self.m_t;
            let one_minus_fc = one - self.fc;

            let term_fc = one_minus_fc.powf(-(one_plus_m));
            let c_fc = self.cjo_t * one_minus_fc.powf(-self.m_t);

            let c_gradient = (self.cjo_t / phi) * self.m_t * term_fc;

            let d_v = v_d - fc_phi;

            c_jun = c_fc + c_gradient * d_v;

            let one_minus_m = one - self.m_t;
            let q_fc = (self.cjo_t * phi) * (one - one_minus_fc.powf(one_minus_m)) / one_minus_m;

            let half = T::from(0.5).unwrap();
            q_jun = q_fc + (c_fc * d_v) + (half * c_gradient * d_v * d_v);
        }

        (i_dc, g_dc, q_diff + q_jun, c_diff + c_jun)
    }

    /// Helper to calculate the total current (DC + Transient) flowing through the diode
    /// based on the converged solution.
    fn compute_total_current(&self, solution: &ColRef<T>, dt: T) -> T {
        let v_d = self.get_intrinsic_voltage(solution);
        let (i_dc, _, q_tot, _) = self.calculate_operating_point(v_d);

        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        let ag = three / (two * dt);

        let q_history = (four * self.q_m1) - self.q_m2;
        let i_eq = -(q_history / (two * dt));

        let i_cap = (ag * q_tot) + i_eq;

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

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
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

    fn stamp_static(&self, matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {
        let idx_a = self.cached_idx_a;

        if let Some(r_idx) = self.internal_node_idx {
            let g_s = T::one() / self.series_resistance;
            stamp_conductance(matrix, idx_a, Some(r_idx), g_s, 0);
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

        let v_d = self.limit_voltage(v_d_input, v_old_iter);

        state.last_iter_voltage = v_d;

        let (i_dc, g_dc, q_tot, c_tot) = self.calculate_operating_point(v_d);

        let (g_total, i_total_flowing);

        if ctx.is_dc_analysis {
            g_total = g_dc;
            i_total_flowing = i_dc;
        } else {
            let dt = ctx.dt;
            let two = T::from(2.0).unwrap();
            let three = T::from(3.0).unwrap();
            let four = T::from(4.0).unwrap();

            let ag = three / (two * dt);
            let g_dyn = ag * c_tot;

            let q_history = (four * self.q_m1) - self.q_m2;
            let i_eq = -(q_history / (two * dt));

            let i_cap = (ag * q_tot) + i_eq;

            g_total = g_dc + g_dyn;
            i_total_flowing = i_dc + i_cap;
        }

        let i_rhs = i_total_flowing - (g_total * v_d);

        let idx_p = self.get_intrinsic_anode_idx(l_size);
        let idx_k = self.cached_idx_b;

        stamp_conductance(matrix, idx_p, idx_k, g_total, l_size);
        stamp_current_source(rhs, idx_p, idx_k, i_rhs, l_size);

        let current_diff = (i_total_flowing - state.last_iter_current).abs();

        let abs_tol = T::from(1e-12).unwrap();
        let rel_tol = T::from(1e-3).unwrap();

        let voltage_diff = (v_d - v_d_input).abs();
        let v_converged = voltage_diff < (self.vt * T::from(0.01).unwrap());
        let i_converged = current_diff < (abs_tol + rel_tol * i_total_flowing.abs());

        state.last_iter_current = i_total_flowing;
        state.is_converged = v_converged && i_converged;

        self.iter_state.set(state);
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_d = self.get_intrinsic_voltage(current_node_voltages);
        let (_, _, q_new, _) = self.calculate_operating_point(v_d);

        self.prev_voltage = v_d;

        if ctx.is_dc_analysis {
            self.q_m1 = q_new;
            self.q_m2 = q_new;
        } else {
            // Shift BDF2 history
            self.q_m2 = self.q_m1;
            self.q_m1 = q_new;
        }

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

    fn calculate_observables(
        &self,
        solution: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        // Calculate external voltage (Node A - Node B) using cached indices
        let v_a = self.cached_idx_a.map(|i| solution[i]).unwrap_or(T::zero());
        let v_b = self.cached_idx_b.map(|i| solution[i]).unwrap_or(T::zero());
        let v_total = v_a - v_b;

        let i_total = self.compute_total_current(solution, ctx.dt);

        out_observables[0] = v_total;
        out_observables[1] = i_total;
        out_observables[2] = v_total * i_total;
    }

    fn terminal_currents(
        &self,
        solution: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let i_flow = self.compute_total_current(solution, ctx.dt);
        // Current flows into Anode, out of Cathode
        out_currents[0] = i_flow;
        out_currents[1] = -i_flow;
    }

    fn refresh_per_step(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        // Refresh whenever any parameter is formula-driven and none of them
        // depend on a node voltage. Voltage-dependent ones get refreshed
        // per iteration instead.
        if self.any_param_dynamic() && !self.any_param_voltage() {
            self.refresh_cached(eval);
        }
    }

    fn refresh_per_iter(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if self.any_param_voltage() {
            self.refresh_cached(eval);
        }
    }

    fn set_param_value(&mut self, name: &str, pv: ParamValue) -> RebuildKind {
        // Each arm: swap the ParamValue, then refresh the cached T-typed
        // copy if the new value is a plain constant. Formula-driven values
        // get picked up automatically by refresh_per_step / refresh_per_iter.
        //
        // The diode's linearity is always NonLinear, so a parameter change
        // never re-partitions the matrix. The refresh phase (per_step vs
        // per_iter) does change when a formula's voltage-dependence flips,
        // but no matrix-shape change is required for that.
        match name {
            "is" | "saturation_current" => {
                self.saturation_current = pv;
                if let Some(v) = self.saturation_current.as_constant() {
                    self.is_t = T::from(v).unwrap();
                    self.v_crit = Self::compute_v_crit(self.is_t, self.n_t, self.vt);
                }
            }
            "n" | "emission_coefficient" => {
                self.emission_coefficient = pv;
                if let Some(v) = self.emission_coefficient.as_constant() {
                    self.n_t = T::from(v).unwrap();
                    self.v_crit = Self::compute_v_crit(self.is_t, self.n_t, self.vt);
                }
            }
            "bv" | "breakdown_voltage" => {
                self.bv = pv;
                if let Some(v) = self.bv.as_constant() {
                    self.bv_t = T::from(v).unwrap();
                }
            }
            "ibv" => {
                self.ibv = pv;
                if let Some(v) = self.ibv.as_constant() {
                    self.ibv_t = T::from(v).unwrap();
                }
            }
            "cjo" => {
                self.cjo = pv;
                if let Some(v) = self.cjo.as_constant() {
                    self.cjo_t = T::from(v).unwrap();
                }
            }
            "m" => {
                self.m = pv;
                if let Some(v) = self.m.as_constant() {
                    self.m_t = T::from(v).unwrap();
                }
            }
            "tt" | "transit_time" => {
                self.transit_time = pv;
                if let Some(v) = self.transit_time.as_constant() {
                    self.tt_t = T::from(v).unwrap();
                }
            }
            _ => return RebuildKind::None,
        }

        RebuildKind::None
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        // Rs is special: still a plain T (not a ParamValue) because it
        // changes the matrix shape (auxiliary row appears/disappears at
        // the zero crossing). Treat it as a static-matrix rebuild trigger.
        if matches!(name, "rs" | "series_resistance") {
            self.series_resistance = value;
            return true;
        }
        // Everything else routes through set_param_value with a constant.
        let v = value.to_f64().unwrap_or(0.0);
        !matches!(
            self.set_param_value(name, ParamValue::Constant(v)),
            RebuildKind::None
        )
    }

    fn is_converged(&self, _current_node_voltages: &ColRef<T>) -> bool {
        self.iter_state.get().is_converged
    }
}
