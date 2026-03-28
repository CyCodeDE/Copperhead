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
use crate::components::transformer::{Coupling, Winding};
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::util::mna::{get_voltage, stamp_matrix_element, stamp_vector_element};
use faer::{ColMut, ColRef, MatMut};
use num_traits::cast;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AccurateTransformerDef {
    pub windings: Vec<Winding>,
    pub couplings: Vec<Coupling>,
    /// Saturation flux in Webers (Wb). Determines the maximum magnetic flux.
    pub phi_sat: f64,
    /// Magnetizing current in Amperes (A) at which the core begins to saturate.
    pub i_sat: f64,
}

impl<T: CircuitScalar> Instantiable<T> for AccurateTransformerDef {
    fn instantiate(&self, _nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let n = self.windings.len();
        let mut n_turns = vec![T::zero(); n];
        let mut l_leak = vec![T::zero(); n];

        for i in 0..n {
            let l_nom = cast::<f64, T>(self.windings[i].inductance).unwrap();

            // Effective turns ratio N = sqrt(L_nom)
            n_turns[i] = l_nom.sqrt();

            // Find the maximum coupling coefficient for this winding to estimate leakage
            let mut max_k = 0.0;
            for c in &self.couplings {
                if c.winding_1 == i || c.winding_2 == i {
                    if c.k > max_k {
                        max_k = c.k;
                    }
                }
            }

            // Estimate leakage inductance. If k=0, it acts as an independent inductor.
            let k_factor = cast::<f64, T>(max_k).unwrap();
            l_leak[i] = l_nom * (T::one() - k_factor);
        }

        let comp = AccurateTransformer::new(
            self.windings.clone(),
            n_turns,
            l_leak,
            T::from(self.phi_sat).unwrap(),
            T::from(self.i_sat).unwrap(),
        );
        circuit.add_component(comp);
    }
}

pub struct AccurateTransformer<T> {
    windings: Vec<Winding>,
    cached_node_indices: Vec<(Option<usize>, Option<usize>)>,
    aux_indices: Vec<usize>,

    // Extracted / Derived nonlinear core parameters
    n_turns: Vec<T>,
    l_leak: Vec<T>,
    phi_sat: T,
    i_sat: T,

    // BDF2 history states for Flux Linkage
    lambda_m1: Vec<T>,
    lambda_m2: Vec<T>,

    // Convergence tracking
    i_prev_iter: Vec<T>,
}

impl<T: CircuitScalar> AccurateTransformer<T> {
    pub fn new(
        windings: Vec<Winding>,
        n_turns: Vec<T>,
        l_leak: Vec<T>,
        phi_sat: T,
        i_sat: T,
    ) -> Self {
        let n = windings.len();
        Self {
            windings,
            cached_node_indices: vec![(None, None); n],
            aux_indices: Vec::new(),
            n_turns,
            l_leak,
            phi_sat,
            i_sat,
            lambda_m1: vec![T::zero(); n],
            lambda_m2: vec![T::zero(); n],
            i_prev_iter: vec![T::zero(); n],
        }
    }
}

impl<T: CircuitScalar> Component<T> for AccurateTransformer<T> {
    fn linearity(&self) -> ComponentLinearity {
        ComponentLinearity::NonLinear
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        for (i, w) in self.windings.iter().enumerate() {
            let idx_a = if w.node_a.0 == 0 {
                None
            } else {
                Some(*node_map.get(&w.node_a).unwrap())
            };
            let idx_b = if w.node_b.0 == 0 {
                None
            } else {
                Some(*node_map.get(&w.node_b).unwrap())
            };
            self.cached_node_indices[i] = (idx_a, idx_b);

            println!(
                "Node IDs: {:?}, {:?} for winding {}",
                w.node_a,
                w.node_b,
                i + 1
            );
        }
    }

    fn ports(&self) -> Vec<NodeId> {
        let mut p = Vec::with_capacity(self.windings.len() * 2);
        for w in &self.windings {
            p.push(w.node_a);
            p.push(w.node_b);
        }
        p
    }

    fn auxiliary_row_count(&self) -> usize {
        self.windings.len()
    }

    fn set_auxiliary_index(&mut self, start_idx: usize) {
        self.aux_indices = (start_idx..(start_idx + self.windings.len())).collect();
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {
        let n = self.windings.len();

        for i in 0..n {
            let (idx_a, idx_b) = self.cached_node_indices[i];
            let aux_i = Some(self.aux_indices[i]);

            // Stamp voltage differences into the aux
            stamp_matrix_element(matrix, aux_i, idx_a, T::one(), 0);
            stamp_matrix_element(matrix, aux_i, idx_b, -T::one(), 0);

            // Stamp branch current back
            stamp_matrix_element(matrix, idx_a, aux_i, T::one(), 0);
            stamp_matrix_element(matrix, idx_b, aux_i, -T::one(), 0);

            // Stamp series resistance
            let rs = T::from(self.windings[i].series_resistance).unwrap();
            stamp_matrix_element(matrix, aux_i, aux_i, -rs, 0);
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
        if ctx.is_dc_analysis {
            return;
        }

        let n = self.windings.len();
        let dt = cast::<T, f64>(ctx.dt).unwrap();

        let mut i_curr = vec![0.0; n];
        let mut i_mag = 0.0;

        for j in 0..n {
            let aux_idx = Some(self.aux_indices[j]);
            i_curr[j] = cast::<T, f64>(crate::util::mna::get_voltage(
                current_node_voltages,
                aux_idx,
            ))
            .unwrap();
            i_mag += cast::<T, f64>(self.n_turns[j]).unwrap() * i_curr[j];
        }

        let i_sat = cast::<T, f64>(self.i_sat).unwrap();
        let phi_sat = cast::<T, f64>(self.phi_sat).unwrap();

        let tanh_val = (i_mag / i_sat).tanh();
        let sech2_val = 1.0 - (tanh_val * tanh_val);
        let phi = phi_sat * tanh_val;

        let dphi_dimag = (phi_sat / i_sat) * sech2_val;

        let mut lambda = vec![0.0; n];
        let mut l_diff = vec![vec![0.0; n]; n];

        for j in 0..n {
            let nj = cast::<T, f64>(self.n_turns[j]).unwrap();
            let lleak_j = cast::<T, f64>(self.l_leak[j]).unwrap();

            lambda[j] = nj * phi + lleak_j * i_curr[j];

            for k in 0..n {
                let nk = cast::<T, f64>(self.n_turns[k]).unwrap();
                let mut l_jk = nj * nk * dphi_dimag;
                if j == k {
                    l_jk += lleak_j;
                }
                l_diff[j][k] = l_jk;
            }
        }

        let factor = 3.0 / (2.0 * dt);

        for j in 0..n {
            let aux_j = Some(self.aux_indices[j]);

            let lam_j = lambda[j];
            let lam_m1 = cast::<T, f64>(self.lambda_m1[j]).unwrap();
            let lam_m2 = cast::<T, f64>(self.lambda_m2[j]).unwrap();

            let v_ind_j = (3.0 * lam_j - 4.0 * lam_m1 + lam_m2) / (2.0 * dt);
            let mut sum_g_eq_i = 0.0;

            for k in 0..n {
                let aux_k = Some(self.aux_indices[k]);
                let g_eq_jk = l_diff[j][k] * factor;

                // equivalent conductance
                stamp_matrix_element(
                    matrix,
                    aux_j,
                    aux_k,
                    cast::<f64, T>(-g_eq_jk).unwrap(),
                    l_size,
                );

                sum_g_eq_i += g_eq_jk * i_curr[k];
            }

            // equivalent RHS source for the nonlinear inductor
            let v_eq_j = v_ind_j - sum_g_eq_i;

            stamp_vector_element(rhs, aux_j, cast::<f64, T>(v_eq_j).unwrap(), l_size);
        }
    }

    fn is_converged(&self, current_node_voltages: &ColRef<T>) -> bool {
        let tol = cast::<f64, T>(1e-6).unwrap(); // Or derive from ctx
        for i in 0..self.windings.len() {
            let aux_idx = Some(self.aux_indices[i]);
            let current = crate::util::mna::get_voltage(current_node_voltages, aux_idx);
            let diff = current - self.i_prev_iter[i];

            if diff.abs() > tol {
                return false;
            }
        }
        true
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        if ctx.is_dc_analysis {
            return;
        }

        let n = self.windings.len();
        let mut i_curr = vec![0.0; n];
        let mut i_mag = 0.0;

        for j in 0..n {
            let aux_idx = Some(self.aux_indices[j]);
            let current = get_voltage(current_node_voltages, aux_idx);
            self.i_prev_iter[j] = current;
            i_curr[j] = cast::<T, f64>(current).unwrap();
            i_mag += cast::<T, f64>(self.n_turns[j]).unwrap() * i_curr[j];
        }

        // Calculate and shift the final flux linkage for this time step
        let i_sat = cast::<T, f64>(self.i_sat).unwrap();
        let phi_sat = cast::<T, f64>(self.phi_sat).unwrap();
        let phi = phi_sat * (i_mag / i_sat).tanh();

        for j in 0..n {
            let nj = cast::<T, f64>(self.n_turns[j]).unwrap();
            let lleak_j = cast::<T, f64>(self.l_leak[j]).unwrap();

            let lambda_new = nj * phi + lleak_j * i_curr[j];
            let lambda_new_t = cast::<f64, T>(lambda_new).unwrap();

            self.lambda_m2[j] = self.lambda_m1[j];
            self.lambda_m1[j] = lambda_new_t;
        }
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        let mut probes = Vec::with_capacity(self.windings.len() * 3);
        for i in 0..self.windings.len() {
            probes.push(ComponentProbe {
                name: format!("Winding {} Voltage", i + 1),
                unit: "V".to_string(),
            });
            probes.push(ComponentProbe {
                name: format!("Winding {} Current", i + 1),
                unit: "A".to_string(),
            });
            probes.push(ComponentProbe {
                name: format!("Winding {} Power", i + 1),
                unit: "W".to_string(),
            });
        }
        probes
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        for i in 0..self.windings.len() {
            let (idx_a, idx_b) = self.cached_node_indices[i];
            let va = idx_a.map_or(T::zero(), |idx| node_voltages[idx]);
            let vb = idx_b.map_or(T::zero(), |idx| node_voltages[idx]);
            let v_diff = va - vb;

            let current = node_voltages[self.aux_indices[i]];
            let power = v_diff * current;

            let base_idx = i * 3;
            out_observables[base_idx] = v_diff;
            out_observables[base_idx + 1] = current;
            out_observables[base_idx + 2] = power;
        }
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        for i in 0..self.windings.len() {
            let current = node_voltages[self.aux_indices[i]];

            let base_idx = i * 2;
            out_currents[base_idx] = current; // Current entering node_a
            out_currents[base_idx + 1] = -current; // Current leaving node_b
        }
    }

    fn set_parameter(&mut self, _name: &str, _value: T, _ctx: &SimulationContext<T>) -> bool {
        false
    }
}
