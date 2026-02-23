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

use crate::components::capacitor::Capacitor;
use crate::components::resistor::Resistor;
use crate::model::CircuitComponents;
use crate::model::{
    CircuitScalar, Component, ComponentId, ComponentLinearity, InsertIntoSoA, NodeId,
    SimulationContext,
};
use crate::ui::{SimBatchData, SimStepData};
use faer::dyn_stack::{MemBuffer, MemStack};
use faer::linalg::lu::full_pivoting::factor::lu_in_place;
use faer::linalg::solvers::PartialPivLu;
use faer::matrix_free::LinOp;
use faer::perm::{Perm, PermRef};
use faer::prelude::Solve;
use faer::{Accum, Col, Conj, Mat, Par, Spec};
use log::{debug, error, info, trace};
use petgraph::graph::NodeIndex;
use petgraph::prelude::{EdgeRef, UnGraph};
use std::collections::{HashMap, HashSet};

const GMIN: f64 = 1e-12;
const MAX_NR_ITERATIONS: usize = 50;
const NR_TOLERANCE: f64 = 1e-6;

pub enum CircuitElement {
    Net(NodeId),
    Device(ComponentId),
}

pub type Terminal = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodePartition {
    Eliminated,
    Retained,
}

pub struct PartitionedCircuit {
    /// Maps Global NodeId -> Matrix Index
    node_map: HashMap<NodeId, usize>,

    /// How many rows are in the linear (eliminated) block
    num_l_nodes: usize,
    /// Count of linear auxiliary rows
    num_l_aux: usize,

    /// How many rows are in the non-linear (retained) block
    num_n_nodes: usize,
    /// Count of non-linear auxiliary rows
    num_n_aux: usize,
}

impl PartitionedCircuit {
    /// Total size of the Linear Block (Rows 0 to size_l-1)
    /// This block is Static and will be pre-inverted.
    pub fn size_l(&self) -> usize {
        self.num_l_nodes + self.num_l_aux
    }

    /// Total size of the Non-Linear Block (Rows size_l to end)
    /// This block is Active and solved via Newton-Raphson.
    pub fn size_n(&self) -> usize {
        self.num_n_nodes + self.num_n_aux
    }

    /// Total Matrix Size
    pub fn total_size(&self) -> usize {
        self.size_l() + self.size_n()
    }
}

pub struct Circuit<T: CircuitScalar> {
    pub components: CircuitComponents<T>,
    /// Holds components and nets
    pub graph: UnGraph<CircuitElement, Terminal>,
    /// Fast lookup to find existing nodes in the graph
    pub net_lookup: HashMap<NodeId, NodeIndex>,
    pub partition: Option<PartitionedCircuit>,
    /// Important for the UI to know the order of components for data collection
    pub component_order: Vec<NodeIndex>,
    pub num_nodes: usize,
    pub solver_state: Option<SolverState<T>>,

    pub batch_buffer: SimBatchData,
    previous_solution: Col<T>,
    prev_prev_solution: Col<T>,
    current_solution: Col<T>,

    pub time: T,
    step_count: usize,

    pub total_terminals: usize,
    pub total_observables: usize,
}

pub struct SolverState<T: CircuitScalar> {
    pub workspace: SolverWorkspace<T>,

    /// Pre-factorized LU for the L-Block (A_LL)
    /// Used for solving for linear node voltages given known non-linear variables
    pub l_lu: PartialPivLu<T>,

    /// Coupling matrix A_LN (Linear rows, Non-linear columns)
    /// Represents how Non-Linear voltages affect Linear nodes
    pub a_ln: Mat<T>,

    /// Coupling matrix A_NL (Non-linear rows, Linear columns)
    /// Represents how Linear voltages affect Non-Linear nodes
    pub a_nl: Mat<T>,

    /// Statically condensed matrix: A_NN - (A_NL * A_LL^-1 * A_LN)
    /// "Starting point" for the Jacobian in the Newton-Raphson loop
    pub reduced_base_matrix: Mat<T>,

    pub t_mat: Mat<T>,

    pub ctx: SimulationContext<T>,

    /// Size of the linear block
    pub l_size: usize,
    /// Size of the non-linear block
    pub n_size: usize,
}

pub struct SolverWorkspace<T: CircuitScalar> {
    pub b_full: Col<T>,
    pub v_temp: Col<T>,
    pub coupling_effect: Col<T>,
    pub b_reduced_base: Col<T>,
    pub x_n: Col<T>,
    pub initial_guess_n: Col<T>,
    pub iter_matrix: Mat<T>,
    pub iter_rhs: Col<T>,
    pub next_x_n: Col<T>,
    pub diff: Col<T>,
    pub adjustment: Col<T>,
    /// Buffer for querying terminal currents and observables
    pub component_buffer: Vec<T>,

    pub n_lu_mat: Mat<T>,
    pub row_perm_fwd: Vec<usize>,
    pub row_perm_inv: Vec<usize>,
    pub lu_workspace_memory: MemBuffer,
}

impl<T: CircuitScalar> Circuit<T> {
    pub fn new() -> Self {
        Self {
            graph: UnGraph::<CircuitElement, Terminal>::default(),
            components: CircuitComponents::new(),
            net_lookup: HashMap::new(),
            component_order: Vec::new(),
            batch_buffer: SimBatchData::default(),
            partition: None,
            num_nodes: 0,
            solver_state: None,
            previous_solution: Col::<T>::zeros(0),
            prev_prev_solution: Col::<T>::zeros(0),
            current_solution: Col::<T>::zeros(0),
            time: T::zero(),
            step_count: 0,
            total_terminals: 0,
            total_observables: 0,
        }
    }

    pub fn add_component<C>(&mut self, component: C)
    where
        C: Component<T> + InsertIntoSoA<T>,
    {
        let ports = component.ports();

        self.total_observables += component.probe_definitions().len();
        self.total_terminals += ports.len();

        let comp_id = component.insert_into(&mut self.components);

        let comp_node_idx = self.graph.add_node(CircuitElement::Device(comp_id));

        for (port_idx, node_id) in ports.into_iter().enumerate() {
            let net_node_idx = *self.net_lookup.entry(node_id).or_insert_with(|| {
                self.num_nodes = self.num_nodes.max(node_id.0);
                self.graph.add_node(CircuitElement::Net(node_id))
            });

            self.graph.add_edge(comp_node_idx, net_node_idx, port_idx);
        }
    }

    /// Prepares the MNA system
    pub fn prepare(&mut self, dt: T, is_dc: bool) {
        let partition = self.partition();

        let ctx = SimulationContext {
            dt,
            time: self.time,
            step: self.step_count,
            is_dc_analysis: is_dc,
        };

        let l_aux_start = partition.num_l_nodes;
        let n_block_start = partition.num_l_nodes + partition.num_l_aux;
        let n_aux_start = n_block_start + partition.num_n_nodes;

        let mut node_status = HashMap::new();
        self.components.find_retained_nodes(&mut node_status);

        self.components.bake_all_indices(&ctx, &partition.node_map);
        self.components.assign_auxiliary_rows(
            &partition.node_map,
            n_block_start,
            l_aux_start,
            n_aux_start,
        );

        let l_size = partition.num_l_nodes + partition.num_l_aux;
        let n_size = partition.num_n_nodes + partition.num_n_aux;
        let total_size = l_size + n_size;

        let mut matrix_a = Mat::<T>::zeros(total_size, total_size);

        self.components
            .stamp_all_static(&mut matrix_a.as_mut(), &ctx);

        if is_dc {
            let gmin = T::from(GMIN).unwrap();
            // Only apply to actual VOLTAGE nodes, not auxiliary current rows
            for i in 0..partition.num_l_nodes {
                matrix_a[(i, i)] = matrix_a[(i, i)] + gmin;
            }
        }

        // Slice the matrix into quadrants
        let a_ll = matrix_a.subrows(0, l_size).subcols(0, l_size);
        let a_ln = matrix_a.subrows(0, l_size).subcols(l_size, n_size);
        let a_nl = matrix_a.subrows(l_size, n_size).subcols(0, l_size);
        let a_nn = matrix_a.subrows(l_size, n_size).subcols(l_size, n_size);

        let l_block_lu = a_ll.partial_piv_lu();
        let t_mat = l_block_lu.solve(a_ln);
        let schur_gain = a_nl * &t_mat;
        let reduced_base_matrix = a_nn - &schur_gain;

        // Important for UI/Data collection
        self.component_order.clear();
        for node_idx in self.graph.node_indices() {
            if let CircuitElement::Device(_) = &self.graph[node_idx] {
                self.component_order.push(node_idx);
            }
        }

        self.previous_solution = Col::<T>::zeros(total_size);
        self.prev_prev_solution = Col::<T>::zeros(total_size);
        self.current_solution = Col::<T>::zeros(total_size);
        self.partition = Some(partition);

        let max_buffer_needed = self.total_terminals.max(self.total_observables);

        let lu_memory = faer::linalg::lu::partial_pivoting::factor::lu_in_place_scratch::<usize, T>(
            n_size,
            n_size,
            Par::Seq,
            Default::default(),
        );
        let solve_memory = faer::linalg::lu::partial_pivoting::solve::solve_in_place_scratch::<
            usize,
            T,
        >(n_size, n_size, Par::Seq);

        let combined_req = lu_memory.or(solve_memory);
        let lu_workspace_memory = MemBuffer::new(combined_req);

        self.solver_state = Some(SolverState {
            workspace: SolverWorkspace {
                b_full: Col::<T>::zeros(total_size),
                v_temp: Col::<T>::zeros(l_size),
                coupling_effect: Col::<T>::zeros(n_size),
                b_reduced_base: Col::<T>::zeros(n_size),
                x_n: Col::<T>::zeros(n_size),
                initial_guess_n: Col::<T>::zeros(n_size),
                iter_matrix: Mat::<T>::zeros(n_size, n_size),
                iter_rhs: Col::<T>::zeros(n_size),
                next_x_n: Col::<T>::zeros(n_size),
                diff: Col::<T>::zeros(n_size),
                adjustment: Col::<T>::zeros(l_size),
                component_buffer: vec![T::zero(); max_buffer_needed],

                n_lu_mat: Mat::<T>::zeros(n_size, n_size),
                row_perm_fwd: vec![0usize; n_size],
                row_perm_inv: vec![0usize; n_size],
                lu_workspace_memory,
            },
            l_lu: l_block_lu,
            a_ln: a_ln.to_owned(),
            a_nl: a_nl.to_owned(),
            reduced_base_matrix,
            t_mat: t_mat.to_owned(),
            ctx,
            l_size,
            n_size,
        });

        self.batch_buffer.nodes_per_step = self.num_nodes + 1;
        self.batch_buffer.terminals_per_step = self.total_terminals;
        self.batch_buffer.observables_per_step = self.total_observables;
    }

    pub fn partition(&self) -> PartitionedCircuit {
        let mut node_status = HashMap::new();

        self.components.find_retained_nodes(&mut node_status);

        let (num_l_aux, num_n_aux) = self.components.count_auxiliary_rows(&node_status);

        let mut l_nodes = Vec::new();
        let mut n_nodes = Vec::new();

        for node_idx in self.graph.node_indices() {
            if let CircuitElement::Net(node_id) = self.graph[node_idx] {
                if node_id == NodeId(0) {
                    continue; // Skip Ground
                }

                match node_status
                    .get(&node_id)
                    .unwrap_or(&NodePartition::Eliminated)
                {
                    NodePartition::Eliminated => l_nodes.push(node_id),
                    NodePartition::Retained => n_nodes.push(node_id),
                }
            }
        }

        let mut map = HashMap::new();
        let mut idx_counter = 0;

        // Linear Nodes
        for node_id in l_nodes {
            map.insert(node_id, idx_counter);
            idx_counter += 1;
        }
        let num_l_nodes = idx_counter;

        idx_counter += num_l_aux;

        // Non-Linear Nodes
        let n_start_offset = idx_counter;
        for node_id in n_nodes {
            map.insert(node_id, idx_counter);
            idx_counter += 1;
        }
        let num_n_nodes = idx_counter - n_start_offset;

        PartitionedCircuit {
            node_map: map,
            num_l_nodes,
            num_l_aux,
            num_n_nodes,
            num_n_aux,
        }
    }

    pub fn calculate_dc_operating_point(
        &mut self,
        tolerance: T,
        max_iters: usize,
        dt: T,
    ) -> Result<(), String> {
        let dc_ctx = SimulationContext {
            dt, // Technically completely irrelevant for DC
            time: T::zero(),
            step: 0,
            is_dc_analysis: true,
        };

        self.time = T::zero();
        self.prepare(dc_ctx.dt, true);

        let state = self.solver_state.as_ref().unwrap();
        let total_size = state.l_size + state.n_size;

        let mut x = if self.previous_solution.nrows() == total_size {
            self.previous_solution.clone()
        } else {
            Col::<T>::zeros(total_size)
        };

        let mut b_full = Col::<T>::zeros(total_size);
        let empty_prev = Col::<T>::zeros(total_size);

        self.components
            .stamp_all_dynamic(&empty_prev.as_ref(), &mut b_full.as_mut(), &dc_ctx);

        let b_l = b_full.subrows(0, state.l_size);
        let b_n = b_full.subrows(state.l_size, state.n_size);

        let v_linear_sources = state.l_lu.solve(&b_l);

        let constant_coupling = &state.a_nl * &v_linear_sources;

        let mut converged = false;
        let mut damping_factor = 1.0f64;

        for _iter in 0..max_iters {
            let mut iter_matrix = state.reduced_base_matrix.clone();

            // GMIN insertion (Crucial for DC stability of floating nodes)
            let gmin = T::from(GMIN).unwrap();
            for i in 0..state.n_size {
                iter_matrix[(i, i)] = iter_matrix[(i, i)] + gmin;
            }

            let mut iter_rhs = b_n - &constant_coupling;

            // Stamp Non-Linear Jacobian and RHS corrections
            self.components.stamp_all_nonlinear(
                &x.as_ref(),
                &mut iter_matrix.as_mut(),
                &mut iter_rhs.as_mut(),
                &state.ctx,
                state.l_size,
            );

            let lu = iter_matrix.partial_piv_lu();
            let target_x_n = lu.solve(&iter_rhs);

            let current_x_n = x.subrows(state.l_size, state.n_size);
            let diff = &target_x_n - &current_x_n;
            let error = diff.norm_max();

            let damped_update = &current_x_n + &(&diff * damping_factor);
            x.subrows_mut(state.l_size, state.n_size)
                .copy_from(&damped_update);

            let adjustment = &state.t_mat * &x.subrows(state.l_size, state.n_size);
            let x_l = &v_linear_sources - &adjustment;
            x.subrows_mut(0, state.l_size).copy_from(&x_l);

            if error < T::real_part_impl(&T::from(tolerance).unwrap()) {
                converged = true;
                break;
            }

            // Simple Damping Control
            if _iter > 10 && error > T::real_part_impl(&T::from(1.0).unwrap()) {
                damping_factor = 0.5; // Slow down if oscillating high
            }
        }

        if !converged {
            // Fallback: Try Source Stepping here?
            return Err("DC Operating Point failed to converge".to_string());
        }

        self.current_solution = x.clone();
        self.previous_solution = x.clone();

        self.components.update_all_states(&x.as_ref(), &dc_ctx);

        self.record_state(&dc_ctx);
        self.step_count += 1;

        Ok(())
    }

    pub fn solve_step(&mut self, dt: T) {
        let state = match &mut self.solver_state {
            Some(s) => s,
            None => panic!("Solver state not initialized!"),
        };

        self.time = self.time + dt;

        let total_size = state.l_size + state.n_size;
        let l_size = state.l_size;
        let n_size = state.n_size;

        // Initialize the solution guess for this time step.
        // A good guess is the solution from the previous time step.
        // If we don't do this, the diodes for example starts at 0V (off) every step,
        if self.previous_solution.nrows() == total_size {
            if self.step_count > 1 {
                // First-order linear extrapolation
                for i in 0..total_size {
                    // x_n = x_{n-1} + (x_{n-1} - x_{n-2})
                    self.current_solution[i] = self.previous_solution[i]
                        + (self.previous_solution[i] - self.prev_prev_solution[i]);
                }
            } else {
                self.current_solution.copy_from(&self.previous_solution);
            }
        } else {
            // First run or resize
            self.current_solution = Col::<T>::zeros(total_size);
        }

        let ctx = SimulationContext {
            dt,
            time: self.time,
            step: self.step_count,
            is_dc_analysis: false,
        };

        for i in 0..total_size {
            state.workspace.b_full[i] = T::zero();
        }

        // Stamp Dynamic (Capacitor/Inductor history)
        // dynamic history depends on t-1
        self.components.stamp_all_dynamic(
            &self.previous_solution.as_ref(),
            &mut state.workspace.b_full.as_mut(),
            &ctx,
        );

        state
            .workspace
            .v_temp
            .copy_from(&state.workspace.b_full.subrows(0, l_size));
        state.l_lu.solve_in_place(state.workspace.v_temp.as_mut());

        faer::linalg::matmul::matmul(
            state.workspace.coupling_effect.as_mut(),
            Accum::Replace,
            state.a_nl.as_ref(),
            state.workspace.v_temp.as_ref(),
            T::one(),
            Par::Seq,
        );

        let b_n = state.workspace.b_full.subrows(l_size, n_size);
        for i in 0..n_size {
            state.workspace.b_reduced_base[i] = b_n[i] - state.workspace.coupling_effect[i];
        }

        // Newton-Raphson Iteration Loop
        let tolerance = T::from(NR_TOLERANCE).unwrap();
        let mut converged = false;
        let mut damping_factor = 1.0f64;

        state
            .workspace
            .initial_guess_n
            .copy_from(&self.current_solution.subrows(l_size, n_size));
        state
            .workspace
            .x_n
            .copy_from(&state.workspace.initial_guess_n);

        // try to converge, if not, reduce damping factor
        for attempt in 0..3 {
            if attempt > 0 {
                state
                    .workspace
                    .x_n
                    .copy_from(&state.workspace.initial_guess_n);
            }
            for _iter in 0..MAX_NR_ITERATIONS {
                let inner_span = tracing::info_span!("NR Iteration").entered();
                state
                    .workspace
                    .iter_matrix
                    .copy_from(&state.reduced_base_matrix);
                state
                    .workspace
                    .iter_rhs
                    .copy_from(&state.workspace.b_reduced_base);

                self.components.stamp_all_dynamic_and_nonlinear(
                    &self.current_solution.as_ref(),
                    &mut state.workspace.iter_matrix.as_mut(),
                    &mut state.workspace.iter_rhs.as_mut(),
                    &ctx,
                    state.l_size,
                );

                let stack = MemStack::new(&mut state.workspace.lu_workspace_memory);

                faer::linalg::lu::partial_pivoting::factor::lu_in_place(
                    state.workspace.iter_matrix.as_mut(),
                    &mut state.workspace.row_perm_fwd,
                    &mut state.workspace.row_perm_inv,
                    Par::Seq,
                    stack,
                    Spec::default(),
                );

                let perm = unsafe {
                    PermRef::new_unchecked(
                        &state.workspace.row_perm_fwd,
                        &state.workspace.row_perm_inv,
                        state.workspace.iter_matrix.nrows(),
                    )
                };

                faer::linalg::lu::partial_pivoting::solve::solve_in_place_with_conj(
                    state.workspace.iter_matrix.as_ref(),
                    state.workspace.iter_matrix.as_ref(),
                    perm,
                    Conj::No,
                    state.workspace.iter_rhs.as_mat_mut(),
                    Par::Seq,
                    stack,
                );

                // Element-wise diff and damped update
                for i in 0..n_size {
                    let d = state.workspace.iter_rhs[i] - state.workspace.x_n[i];
                    state.workspace.diff[i] = d;

                    let damped = T::from(damping_factor).unwrap() * d;
                    state.workspace.x_n[i] = state.workspace.x_n[i] + damped;
                }

                let error = state.workspace.diff.norm_max();
                self.current_solution
                    .subrows_mut(l_size, n_size)
                    .copy_from(&state.workspace.x_n);

                inner_span.exit();

                if error < T::real_part_impl(&tolerance) {
                    converged = true;
                    break;
                }
            }

            if converged {
                break;
            }

            // Reduce damping factor and try again
            // This helps with convergence in tough non-linear cases but slows down the simulation.
            damping_factor *= 0.5;
            log::warn!(
                "Reducing damping factor to {} for better convergence",
                damping_factor
            );
        }

        if !converged {
            log::warn!(
                "Warning: Newton-Raphson did not converge within {} iterations",
                MAX_NR_ITERATIONS
            );
        }

        // adjustment = t_mat * x_n
        faer::linalg::matmul::matmul(
            state.workspace.adjustment.as_mut(),
            Accum::Replace,
            state.t_mat.as_ref(),
            state.workspace.x_n.as_ref(),
            T::one(),
            Par::Seq,
        );

        for i in 0..l_size {
            self.current_solution[i] = state.workspace.v_temp[i] - state.workspace.adjustment[i];
        }

        self.components
            .update_all_states(&self.current_solution.as_ref(), &ctx);

        self.record_state(&ctx);
        self.prev_prev_solution.copy_from(&self.previous_solution);
        self.previous_solution.copy_from(&self.current_solution);
        self.step_count += 1;
    }

    pub fn record_state(&mut self, ctx: &SimulationContext<T>) {
        let workspace = &mut self.solver_state.as_mut().unwrap().workspace;
        self.batch_buffer.times.push(self.time.to_f64().unwrap());

        let v_start = self.batch_buffer.voltages.len();
        self.batch_buffer
            .voltages
            .resize(v_start + self.num_nodes + 1, 0.0);

        if let Some(partition) = &self.partition {
            for (node_id, &matrix_row_idx) in &partition.node_map {
                if matrix_row_idx < self.current_solution.nrows() {
                    let val = self.current_solution[matrix_row_idx].to_f64().unwrap();
                    if node_id.0 < self.num_nodes + 1 {
                        self.batch_buffer.voltages[v_start + node_id.0] = val;
                    }
                }
            }
        }

        for &graph_idx in &self.component_order {
            if let CircuitElement::Device(comp_id) = &self.graph[graph_idx] {
                let num_terminals = self.components.get_num_terminals(*comp_id);
                let current_slice = &mut workspace.component_buffer[..num_terminals];

                self.components.get_terminal_currents(
                    *comp_id,
                    &self.current_solution.as_ref(),
                    ctx,
                    current_slice,
                );

                for val in current_slice.iter() {
                    self.batch_buffer.currents.push(val.to_f64().unwrap());
                }

                let num_probes = self.components.get_probe_definitions(*comp_id).len();
                let obs_slice = &mut workspace.component_buffer[..num_probes];

                self.components.get_observables(
                    *comp_id,
                    &self.current_solution.as_ref(),
                    ctx,
                    obs_slice,
                );

                for val in obs_slice.iter() {
                    self.batch_buffer.observables.push(val.to_f64().unwrap());
                }
            }
        }
    }

    /// Returns the accumulated history and clears the internal buffer
    pub fn extract_history(&mut self, dst: &mut SimBatchData) {
        std::mem::swap(&mut self.batch_buffer, dst);

        self.batch_buffer.nodes_per_step = self.num_nodes + 1;
        self.batch_buffer.terminals_per_step = self.total_terminals;
        self.batch_buffer.observables_per_step = self.total_observables;
    }

    /// Reads the voltage of a specific node from the current solution.
    pub fn get_node_voltage(&self, node: NodeId) -> T {
        if node.0 == 0 {
            return T::zero();
        }

        if let Some(partition) = &self.partition {
            if let Some(&matrix_index) = partition.node_map.get(&node) {
                // Ensure the index is valid within our current solution vector
                if matrix_index < self.current_solution.nrows() {
                    return self.current_solution[matrix_index];
                }
            }
        }

        error!(
            "Warning: Requested voltage for unmapped or non-existent node {}",
            node.0
        );

        T::zero()
    }

    /// Read the terminal currents for a component at the current time step. The order of currents corresponds to the order of ports returned by `Component::ports()`.
    pub fn get_terminal_current(&self, component_idx: usize, dt: T, dst: &mut [T]) {
        if let Some(graph_idx) = self.component_order.get(component_idx) {
            if let CircuitElement::Device(comp_id) = self.graph[*graph_idx] {
                return self.components.get_terminal_currents(
                    comp_id,
                    &self.current_solution.as_ref(),
                    &SimulationContext {
                        dt,
                        time: self.time,
                        step: self.step_count,
                        is_dc_analysis: false,
                        // TODO: we need to check if time step 0 and set to dc analysis or else the measurement won't be accurate for the first step
                    },
                    dst,
                );
            }
        }
    }

    pub fn get_all_voltages_cloned(&self) -> Vec<T> {
        self.current_solution
            .as_ref()
            .iter()
            .take(self.num_nodes)
            .cloned()
            .collect()
    }
}
