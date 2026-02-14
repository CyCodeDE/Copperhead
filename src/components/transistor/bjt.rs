use crate::model::{CircuitScalar, Component, ComponentLinearity, ComponentProbe, NodeId, SimulationContext};
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::sync::Mutex;
use log::info;

/// Internal state for the BJT during Newton-Raphson iterations.
#[derive(Clone, Copy, Debug)]
struct BjtIterationState<T> {
    v_be_limited: T,
    v_bc_limited: T,
    is_converged: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum BjtModel {
    GenericNPN,
    GenericPNP,
}

impl BjtModel {
    /// Returns (Is, Bf, Br, Vt, Vaf, Var, Rc, Rb, Re, Polarity where Polarity is true for NPN, false for PNP)
    pub fn parameters<T: CircuitScalar>(&self) -> (T, T, T, T, T, T, T, T, T, bool) {
        let vt = T::from(0.02585).unwrap();
        let no_early = T::from(1e30).unwrap(); // Effectively infinite
        let zero_r = T::zero();

        match self {
            BjtModel::GenericNPN => {
                let is = T::from(1e-14).unwrap();
                let bf = T::from(200.0).unwrap();
                let br = T::from(2.0).unwrap();
                let vaf = T::from(74.0).unwrap(); // Typical 2N2222
                (is, bf, br, vt, vaf, no_early, zero_r, zero_r, zero_r, true)
            }
            BjtModel::GenericPNP => {
                let is = T::from(1e-14).unwrap();
                let bf = T::from(200.0).unwrap();
                let br = T::from(2.0).unwrap();
                let vaf = T::from(74.0).unwrap();
                (is, bf, br, vt, vaf, no_early, zero_r, zero_r, zero_r, false)
            }
        }
    }

    pub fn format_name(&self) -> &'static str {
        match self {
            BjtModel::GenericNPN => "Generic NPN",
            BjtModel::GenericPNP => "Generic PNP",
        }
    }

    pub fn polarity(&self) -> bool {
        match self {
            BjtModel::GenericNPN => true,
            BjtModel::GenericPNP => false,
        }
    }
}

pub struct Bjt<T: CircuitScalar> {
    // External Nodes
    pub node_c: NodeId,
    pub node_b: NodeId,
    pub node_e: NodeId,

    // Static Parameters
    pub saturation_current: T,
    pub beta_f: T,
    pub beta_r: T,
    pub vt: T,
    pub polarity: T,

    pub v_af: T, // Forward Early Voltage
    pub v_ar: T, // Reverse Early Voltage
    pub rc: T,   // Collector Resistance
    pub rb: T,   // Base Resistance
    pub re: T,   // Emitter Resistance

    // Convergence Parameters
    g_min: T,
    v_crit_be: T,
    v_crit_bc: T,

    // Simulation State
    iter_state: Mutex<BjtIterationState<T>>,
    aux_start_index: Option<usize>, // Assigned by the system
}

impl<T: CircuitScalar> Bjt<T> {
    pub fn new(node_c: NodeId, node_b: NodeId, node_e: NodeId, is: T, bf: T, br: T, vt: T, vaf: T, var: T, rc: T, rb: T, re: T, polarity: bool) -> Self {
        let sqrt_2 = T::from(2.0).unwrap().sqrt();
        let is_be = is / bf;
        let v_crit_be = vt * T::from(vt / (is_be * sqrt_2)).unwrap().ln();
        let is_bc = is / br;
        let v_crit_bc = vt * T::from(vt / (is_bc * sqrt_2)).unwrap().ln();

        // polarity true means 1 (NPN), false means -1 (PNP)
        let polarity = if polarity { T::one() } else { -T::one() };

        Self {
            node_c, node_b, node_e,
            saturation_current: is,
            beta_f: bf,
            beta_r: br,
            vt,
            polarity,
            v_af: vaf,
            v_ar: var,
            rc, rb, re,
            g_min: T::from(1.0e-12).unwrap(),
            v_crit_be,
            v_crit_bc,
            iter_state: Mutex::new(BjtIterationState {
                is_converged: false,
                v_bc_limited: T::zero(),
                v_be_limited: T::zero(),
            }),
            aux_start_index: None,
        }
    }

    fn limit_junction_voltage(v_new: T, v_old: T, vt: T, v_crit: T) -> T {
        if v_new > v_crit && (v_new - v_old).abs() > vt + vt {
            if v_old > T::zero() {
                let arg = (v_new - v_old) / vt;
                if arg > T::zero() {
                    v_old + vt * (T::one() + arg).ln()
                } else {
                    v_new
                }
            } else {
                vt * (v_new / vt).ln()
            }
        } else {
            v_new
        }
    }

    fn get_matrix_idx(node: NodeId) -> Option<usize> {
        if node.0 == 0 { None } else { Some(node.0 - 1) }
    }

    fn exp_safe(x: T) -> T {
        let max_arg = T::from(80.).unwrap();
        if x > max_arg {
            let exp_max = max_arg.exp();
            exp_max * (T::one() + (x - max_arg))
        } else {
            x.exp()
        }
    }

    fn exp_safe_deriv(x: T) -> (T, T) { // Returns (Value, Derivative)
        let max_arg = T::from(80.).unwrap();
        if x > max_arg {
            let exp_max = max_arg.exp();
            (exp_max * (T::one() + (x - max_arg)), exp_max)
        } else {
            let ex = x.exp();
            (ex, ex)
        }
    }
}

impl<T: CircuitScalar> Component<T> for Bjt<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::NonLinear
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_c, self.node_b, self.node_e]
    }

    fn auxiliary_row_count(&self) -> usize {
        // We need an auxiliary row for each terminal that has non-zero parasitic resistance
        let mut count = 0;
        if self.rc > T::zero() { count += 1; }
        if self.rb > T::zero() { count += 1; }
        if self.re > T::zero() { count += 1; }
        count
    }

    fn set_auxiliary_index(&mut self, start_idx: usize) {
        self.aux_start_index = Some(start_idx);
    }

    fn stamp_nonlinear(
        &self,
        current_node_voltages: &ColRef<T>,
        matrix: &mut MatMut<T>,
        rhs: &mut ColMut<T>,
        _ctx: &SimulationContext<T>,
    ) {
        let mut state_guard = self.iter_state.lock().unwrap();
        let idx_c_ext = Self::get_matrix_idx(self.node_c);
        let idx_b_ext = Self::get_matrix_idx(self.node_b);
        let idx_e_ext = Self::get_matrix_idx(self.node_e);

        let mut current_aux = self.aux_start_index.unwrap_or(0);
        let mut resolve_internal = |idx_ext: Option<usize>, r_val: T| -> (Option<usize>, T) {
            if r_val > T::from(1e-9).unwrap() {
                // Parasitic exists: Internal node is the next aux index
                let idx_int = Some(current_aux);
                current_aux += 1;
                (idx_int, r_val)
            } else {
                // No parasitic: Internal node == External node
                (idx_ext, T::zero())
            }
        };

        let (idx_c_int, rc) = resolve_internal(idx_c_ext, self.rc);
        let (idx_b_int, rb) = resolve_internal(idx_b_ext, self.rb);
        let (idx_e_int, re) = resolve_internal(idx_e_ext, self.re);

        // Helper for Matrix Stamping
        let mut stamp = |r: Option<usize>, c: Option<usize>, val: T| {
            if let (Some(row), Some(col)) = (r, c) {
                matrix[(row, col)] = matrix[(row,col)] + val;
            }
        };

        let mut stamp_rhs = |r: Option<usize>, val: T| {
            if let Some(row) = r {
                rhs[row] = rhs[row] + val;
            }
        };

        // Stamp Parasitic Resistors
        let mut apply_resistor = |idx_ext: Option<usize>, idx_int: Option<usize>, r: T| {
            if r > T::zero() {
                let g = T::one() / r;
                // Stamp G between Ext and Int
                stamp(idx_ext, idx_ext, g);
                stamp(idx_ext, idx_int, -g);
                stamp(idx_int, idx_ext, -g);
                stamp(idx_int, idx_int, g);
            }
        };

        apply_resistor(idx_c_ext, idx_c_int, rc);
        apply_resistor(idx_b_ext, idx_b_int, rb);
        apply_resistor(idx_e_ext, idx_e_int, re);

        // Recover Internal Node Voltages
        let get_v = |idx: Option<usize>| -> T {
            match idx {
                Some(i) => current_node_voltages[i],
                None => T::zero(),
            }
        };

        let vc = get_v(idx_c_int);
        let vb = get_v(idx_b_int);
        let ve = get_v(idx_e_int);

        let v_be_raw = self.polarity * (vb - ve);
        let v_bc_raw = self.polarity * (vb - vc);

        // Limiting and Convergence
        let v_be = Self::limit_junction_voltage(v_be_raw, state_guard.v_be_limited, self.vt, self.v_crit_be);
        let v_bc = Self::limit_junction_voltage(v_bc_raw, state_guard.v_bc_limited, self.vt, self.v_crit_bc);

        // Ebers-Moll with Early Effect
        let vt = self.vt;
        let is = self.saturation_current;

        // Base Exponentials
        let (evbe, d_evbe) = Self::exp_safe_deriv(v_be / vt);
        let evbc = Self::exp_safe(v_bc / vt);

        let early_denom = T::one() + (v_bc / self.v_af) + (v_be / self.v_ar);
        let early_denom = if early_denom < T::from(0.01).unwrap() { T::from(0.01).unwrap() } else { early_denom };
        let q1 = T::one() / early_denom;

        // Derivatives of q1
        let dq1_dvbc = (q1 * q1) / self.v_af;
        let dq1_dvbe = (q1 * q1) / self.v_ar;

        let i_trans_ideal = is * (evbe - evbc);
        let i_transport = i_trans_ideal * q1;
        let i_be = (is / self.beta_f) * (evbe - T::one());
        let i_bc = (is / self.beta_r) * (evbc - T::one());

        let g_ideal_fwd = (is * evbe) / vt;
        let g_ideal_rev = -(is * evbc) / vt;

        let g_trans_vbe = (i_trans_ideal * dq1_dvbe) + (q1 * g_ideal_fwd);
        let g_trans_vbc = (i_trans_ideal * dq1_dvbc) + (q1 * g_ideal_rev);

        // Base Conductances
        let g_pi = (is / self.beta_f) * evbe / vt;
        let g_mu = (is / self.beta_r) * evbc / vt;

        // Assemble Jacobian Elements (Unscaled NPN logic)
        let g_cc = -(g_trans_vbc - g_mu);
        let g_ce = -g_trans_vbe;
        let g_cb = -g_cc - g_ce;

        let g_bb = g_pi + g_mu;
        let g_bc = -g_mu;
        let g_be = -g_pi;

        // Emitter Row (Internal)
        let g_ec = g_trans_vbc;
        let g_ee = g_trans_vbe + g_pi;
        let g_eb = -g_ec - g_ee;

        // Calculate total currents (unscaled NPN logic)
        let i_c_total_mag = i_transport - i_bc;
        let i_b_total_mag = i_be + i_bc;
        let i_e_total_mag = -i_transport - i_be;

        let p = self.polarity;

        // Apply polarity to Currents
        let i_c_final = i_c_total_mag * p;
        let i_b_final = i_b_total_mag * p;
        let i_e_final = i_e_total_mag * p;

        // Stamp Jacobian
        stamp(idx_c_int, idx_c_int, g_cc + self.g_min);
        stamp(idx_c_int, idx_b_int, g_cb);
        stamp(idx_c_int, idx_e_int, g_ce);

        stamp(idx_b_int, idx_c_int, g_bc);
        stamp(idx_b_int, idx_b_int, g_bb + self.g_min);
        stamp(idx_b_int, idx_e_int, g_be);

        stamp(idx_e_int, idx_c_int, g_ec);
        stamp(idx_e_int, idx_b_int, g_eb);
        stamp(idx_e_int, idx_e_int, g_ee + self.g_min);

        let p_inv = T::one() / self.polarity;

        // Row C: g_cc*Vc + g_cb*Vb + g_ce*Ve = -g_cc(Vb-Vc) - g_ce(Vb-Ve)
        let g_dot_v_c = (-g_cc * v_bc * p_inv) + (-g_ce * v_be * p_inv);

        // Row B: g_bc*Vc + g_bb*Vb + g_be*Ve = -g_bc(Vb-Vc) - g_be(Vb-Ve)
        let g_dot_v_b = (-g_bc * v_bc * p_inv) + (-g_be * v_be * p_inv);

        // Row E: g_ec*Vc + g_eb*Vb + g_ee*Ve = -g_ec(Vb-Vc) - g_ee(Vb-Ve)
        let g_dot_v_e = (-g_ec * v_bc * p_inv) + (-g_ee * v_be * p_inv);

        stamp_rhs(idx_c_int, g_dot_v_c - i_c_final);
        stamp_rhs(idx_b_int, g_dot_v_b - i_b_final);
        stamp_rhs(idx_e_int, g_dot_v_e - i_e_final);

        // Update State
        state_guard.v_be_limited = v_be;
        state_guard.v_bc_limited = v_bc;

        // Check convergence on the INPUT to the limiter vs previous OUTPUT
        let tol = T::from(1e-6).unwrap();
        state_guard.is_converged = (v_be - v_be_raw).abs() < tol && (v_bc - v_bc_raw).abs() < tol;
    }

    fn is_converged(&self, current_node_voltages: &ColRef<T>) -> bool {
        let state = self.iter_state.lock().unwrap();

        let idx_c_ext = Self::get_matrix_idx(self.node_c);
        let idx_b_ext = Self::get_matrix_idx(self.node_b);
        let idx_e_ext = Self::get_matrix_idx(self.node_e);

        let mut current_aux = self.aux_start_index.unwrap_or(0);
        let mut resolve_int = |idx_ext: Option<usize>, r: T| {
            if r > T::from(1e-9).unwrap() {
                let i = Some(current_aux);
                current_aux += 1;
                i
            } else {
                idx_ext
            }
        };

        let idx_c_int = resolve_int(idx_c_ext, self.rc);
        let idx_b_int = resolve_int(idx_b_ext, self.rb);
        let idx_e_int = resolve_int(idx_e_ext, self.re);

        let get_v = |i: Option<usize>| if let Some(x) = i { current_node_voltages[x] } else { T::zero() };
        let vc = get_v(idx_c_int);
        let vb = get_v(idx_b_int);
        let ve = get_v(idx_e_int);

        // Check Convergence
        let v_be_new = self.polarity * (vb - ve);
        let v_bc_new = self.polarity * (vb - vc);

        let tol = T::from(1e-6).unwrap();
        (v_be_new - state.v_be_limited).abs() < tol && (v_bc_new - state.v_bc_limited).abs() < tol
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "V_be".into(), unit: "V".into() },
            ComponentProbe { name: "V_ce".into(), unit: "V".into() },
            ComponentProbe { name: "I_c".into(), unit: "A".into() },
            ComponentProbe { name: "I_b".into(), unit: "A".into() },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
    ) -> Vec<T> {
        let currents = self.terminal_currents(node_voltages, _ctx);
        let i_c = currents[0];
        let i_b = currents[1];

        // Re-calculate voltages logic locally for V_be/V_ce
        let idx_c_ext = Self::get_matrix_idx(self.node_c);
        let idx_b_ext = Self::get_matrix_idx(self.node_b);
        let idx_e_ext = Self::get_matrix_idx(self.node_e);

        let get_v = |i: Option<usize>| if let Some(x) = i { node_voltages[x] } else { T::zero() };

        let vc_ext = get_v(idx_c_ext);
        let vb_ext = get_v(idx_b_ext);
        let ve_ext = get_v(idx_e_ext);

        let v_be = self.polarity * (vb_ext - ve_ext);
        let v_ce = self.polarity * (vc_ext - ve_ext);

        vec![v_be, v_ce, i_c, i_b]
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
    ) -> Vec<T> {
        // 1. Resolve Internal Nodes
        let idx_c_ext = Self::get_matrix_idx(self.node_c);
        let idx_b_ext = Self::get_matrix_idx(self.node_b);
        let idx_e_ext = Self::get_matrix_idx(self.node_e);

        let mut current_aux = self.aux_start_index.unwrap_or(0);
        let mut resolve_int = |idx_ext: Option<usize>, r: T| {
            if r > T::from(1e-9).unwrap() {
                let i = Some(current_aux);
                current_aux += 1;
                i
            } else {
                idx_ext
            }
        };

        let idx_c_int = resolve_int(idx_c_ext, self.rc);
        let idx_b_int = resolve_int(idx_b_ext, self.rb);
        let idx_e_int = resolve_int(idx_e_ext, self.re);

        let get_v = |i: Option<usize>| if let Some(x) = i { node_voltages[x] } else { T::zero() };
        let vc = get_v(idx_c_int);
        let vb = get_v(idx_b_int);
        let ve = get_v(idx_e_int);

        let v_be = self.polarity * (vb - ve);
        let v_bc = self.polarity * (vb - vc);

        let vt = self.vt;
        let is = self.saturation_current;

        let evbe = Self::exp_safe(v_be / vt);
        let evbc = Self::exp_safe(v_bc / vt);

        // Early Effect
        let early_denom = T::one() + (v_bc / self.v_af) + (v_be / self.v_ar);
        let early_denom = if early_denom < T::from(0.01).unwrap() { T::from(0.01).unwrap() } else { early_denom };
        let q1 = T::one() / early_denom;

        let i_trans_ideal = is * (evbe - evbc);
        let i_transport = i_trans_ideal * q1;
        let i_be = (is / self.beta_f) * (evbe - T::one());
        let i_bc = (is / self.beta_r) * (evbc - T::one());

        // Unscaled NPN currents
        let i_c_mag = i_transport - i_bc;
        let i_b_mag = i_be + i_bc;
        let i_e_mag = -i_transport - i_be;

        // Apply Polarity
        let p = self.polarity;

        vec![i_c_mag * p, i_b_mag * p, i_e_mag * p]
    }

    // NEW
    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        match name {
            "is" => self.saturation_current = value,
            "bf" => self.beta_f = value,
            "br" => self.beta_r = value,
            "vaf" => self.v_af = value,
            "var" => self.v_ar = value,
            "rc" => self.rc = value,
            "rb" => self.rb = value,
            "re" => self.re = value,
            _ => return false,
        }
        false
    }
}