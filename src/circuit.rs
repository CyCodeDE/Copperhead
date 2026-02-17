use std::collections::{HashMap, HashSet};
use crate::components::resistor::Resistor;
use crate::model::{CircuitScalar, Component, ComponentLinearity, NodeId, SimulationContext};
use crate::ui::SimStepData;
use faer::prelude::Solve;
use faer::{Col, Mat};
use faer::linalg::solvers::PartialPivLu;
use faer::sparse::linalg::solvers::Lu;
use petgraph::graph::NodeIndex;
use petgraph::prelude::{EdgeRef, UnGraph};

pub enum CircuitElement<T> {
    Net(NodeId),
    Device(Box<dyn Component<T>>)
}

pub type Terminal = usize;

#[derive(Debug, Clone, Copy, PartialEq)]
enum NodePartition {
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
    /// Holds components and nets
    pub graph: UnGraph<CircuitElement<T>, Terminal>,
    /// Fast lookup to find existing nodes in the graph
    pub net_lookup: HashMap<NodeId, NodeIndex>,
    pub partition: Option<PartitionedCircuit>,
    /// Important for the UI to know the order of components for data collection
    pub component_order: Vec<NodeIndex>,
    num_nodes: usize,
    pub solver_state: Option<SolverState<T>>,

    previous_solution: Col<T>,
    current_solution: Col<T>,

    pub time: T,
    step_count: usize,

    pub step_buffer: Vec<SimStepData>,

    pub total_terminals: usize,
    pub total_observables: usize,
}

pub struct SolverState<T: CircuitScalar> {
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

impl<T: CircuitScalar> Circuit<T> {
    pub fn new() -> Self {
        Self {
            graph: UnGraph::<CircuitElement<T>, Terminal>::default(),
            net_lookup: HashMap::new(),
            component_order: Vec::new(),
            partition: None,
            num_nodes: 0,
            solver_state: None,
            previous_solution: Col::<T>::zeros(0),
            current_solution: Col::<T>::zeros(0),
            time: T::zero(),
            step_count: 0,
            total_terminals: 0,
            total_observables: 0,

            step_buffer: Vec::new(),
        }
    }

    pub fn add_component(&mut self, component: Box<dyn Component<T>>) {
        let ports = component.ports();
        self.total_observables += component.probe_definitions().len();
        self.total_terminals += component.ports().len();
        let comp_node_idx = self.graph.add_node(CircuitElement::Device(component));

        for (port_idx, node_id) in ports.iter().enumerate() {
            // check if the nodeid already exists, if not, create a new node in the graph
            let net_node_idx = *self.net_lookup.entry(*node_id).or_insert_with(|| {
                // node doesn't exist, create it
                self.num_nodes = self.num_nodes.max(node_id.0);
                self.graph.add_node(CircuitElement::Net(*node_id))
            });

            self.graph.add_edge(comp_node_idx, net_node_idx, port_idx);
        }
    }

    /// Prepares the MNA system
    pub fn prepare(&mut self, dt: T) {
        let partition = self.partition();

        let ctx = SimulationContext {
            dt,
            time: self.time,
            step: self.step_count,
            node_map: partition.node_map.clone(),
        };;

        let l_aux_start = partition.num_l_nodes;
        let mut current_l_aux = 0;

        let n_block_start = partition.num_l_nodes + partition.num_l_aux;
        let n_aux_start = n_block_start + partition.num_n_nodes;
        let mut current_n_aux = 0;

        for node_idx in self.graph.node_indices() {
            if let CircuitElement::Device(ref mut comp) = self.graph[node_idx] {
                // Bake ports first (lookup node IDs in ctx)
                comp.bake_indices(&ctx);

                // Assign Aux Row Indices based on Linearity
                let needed = comp.auxiliary_row_count();
                if needed > 0 {
                    match comp.linearity() {
                        ComponentLinearity::LinearStatic | ComponentLinearity::LinearDynamic => {
                            comp.set_auxiliary_index(l_aux_start + current_l_aux);
                            current_l_aux += needed;
                        }
                        _ => {
                            comp.set_auxiliary_index(n_aux_start + current_n_aux);
                            current_n_aux += needed;
                        }
                    }
                }
            }
        }

        let l_size = partition.num_l_nodes + partition.num_l_aux;
        let n_size = partition.num_n_nodes + partition.num_n_aux;
        let total_size = l_size + n_size;

        let mut matrix_a = Mat::<T>::zeros(total_size, total_size);

        for node_idx in self.graph.node_indices() {
            if let CircuitElement::Device(ref comp) = self.graph[node_idx] {
                comp.stamp_static(&mut matrix_a.as_mut());
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

        self.component_order.clear();
        for node_idx in self.graph.node_indices() {
            if let CircuitElement::Device(_) = &self.graph[node_idx] {
                self.component_order.push(node_idx);
            }
        }

        // TODO: run a DC operating point to populate initial conditions
        self.previous_solution = Col::<T>::zeros(total_size);
        self.current_solution = Col::<T>::zeros(total_size);

        self.partition = Some(partition);

        self.solver_state = Some(SolverState {
            l_lu: l_block_lu,
            a_ln: a_ln.to_owned(),
            a_nl: a_nl.to_owned(),
            reduced_base_matrix,
            t_mat: t_mat.to_owned(),
            ctx,
            l_size,
            n_size,
        });
    }

    pub fn partition(&self) -> PartitionedCircuit {
        let mut node_status = HashMap::new();

        let mut num_l_aux = 0;
        let mut num_n_aux = 0;

        // Analyze Connectivity & Component Requirements
        for node_idx in self.graph.node_indices() {
            if let CircuitElement::Device(ref comp) = self.graph[node_idx] {

                // 1. Check Linearity to propagate "Retained" status to nodes
                match comp.linearity() {
                    ComponentLinearity::TimeVariant | ComponentLinearity::NonLinear => {
                        for edge in self.graph.edges(node_idx) {
                            let target_node_idx = edge.target();
                            if let CircuitElement::Net(net_id) = self.graph[target_node_idx] {
                                node_status.insert(net_id, NodePartition::Retained);
                            }
                        }
                    }
                    _ => {}
                }

                // Count Auxiliary Rows
                let aux_count = comp.auxiliary_row_count();
                if aux_count > 0 {
                    match comp.linearity() {
                        // Static linear components go into the L-Block
                        ComponentLinearity::LinearStatic | ComponentLinearity::LinearDynamic => {
                            num_l_aux += aux_count;
                        }
                        // Everything else goes into the N-Block
                        _ => {
                            num_n_aux += aux_count;
                        }
                    }
                }
            }
        }

        let mut l_nodes = Vec::new();
        let mut n_nodes = Vec::new();

        for node_idx in self.graph.node_indices() {
            if let CircuitElement::Net(node_id) = self.graph[node_idx] {
                if node_id == NodeId(0) { continue; } // Skip Ground

                // Default to Eliminated (Linear) unless marked Retained
                match node_status.get(&node_id).unwrap_or(&NodePartition::Eliminated) {
                    NodePartition::Eliminated => l_nodes.push(node_id),
                    NodePartition::Retained => n_nodes.push(node_id),
                }
            }
        }

        let mut map = HashMap::new();

        // L-Nodes start at 0
        let mut idx_counter = 0;
        for node_id in l_nodes {
            map.insert(node_id, idx_counter);
            idx_counter += 1;
        }
        let num_l_nodes = idx_counter;

        // Skip space for L-Aux rows
        // The L-Block size is (num_l_nodes + num_l_aux).
        // The N-Block must start AFTER this entire block.
        idx_counter += num_l_aux;

        // N-Nodes start after L-Block
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

    pub fn solve_step(&mut self, dt: T) {
        let state = match &mut self.solver_state {
            Some(s) => s,
            None => panic!("Solver state not initialized!"),
        };

        let total_size = state.l_size + state.n_size;
        let l_size = state.l_size;
        let n_size = state.n_size;

        // Initialize the solution guess for this time step.
        // A good guess is the solution from the previous time step.
        // If we don't do this, the diodes for example starts at 0V (off) every step,
        // requiring more iterations to turn on.
        if self.previous_solution.nrows() == total_size {
            self.current_solution.copy_from(&self.previous_solution);
        } else {
            // First run or resize
            self.current_solution = Col::<T>::zeros(total_size);
        }

        let ctx = SimulationContext {
            dt,
            time: self.time,
            step: self.step_count,
            node_map: self.partition.as_ref().unwrap().node_map.clone(),
        };

        let mut b_full = Col::<T>::zeros(total_size);

        // Stamp Dynamic (Capacitor/Inductor history)
        // dynamic history depends on t-1
        for idx in &mut self.graph.node_indices() {
            if let CircuitElement::Device(ref mut comp) = self.graph[idx] {
                comp.stamp_dynamic(
                    &self.previous_solution.as_ref(),
                    &mut b_full.as_mut(),
                    &ctx,
                );
            }
        }

        let b_l = b_full.subrows(0, l_size);
        let b_n = b_full.subrows(l_size, n_size);

        let v_temp = state.l_lu.solve(&b_l);

        let coupling_effect = &state.a_nl * &v_temp;
        let b_reduced_base = b_n - coupling_effect;

        // Newton-Raphson Iteration Loop

        let max_iterations = 50;
        let tolerance = T::from(1e-6).unwrap();
        let mut converged = false;
        let mut damping_factor = 1.0f64;

        let initial_guess_n = self.current_solution.subrows(l_size, n_size).to_owned();
        let mut x_n = initial_guess_n.cloned();

        // try to converge, if not, reduce damping factor
        for attempt in 0..3 {
            if attempt > 0 {
                x_n.copy_from(&initial_guess_n);
            }
            for _iter in 0..max_iterations {
                let mut iter_matrix = state.reduced_base_matrix.clone();
                let mut iter_rhs = b_reduced_base.clone();

                for idx in &mut self.graph.node_indices() {
                    if let CircuitElement::Device(ref mut comp) = self.graph[idx] {
                        match comp.linearity() {
                            ComponentLinearity::NonLinear => {
                                comp.stamp_nonlinear(
                                    &self.current_solution.as_ref(),
                                    &mut iter_matrix.as_mut(),
                                    &mut iter_rhs.as_mut(),
                                    &ctx,
                                    state.l_size,
                                )
                            },
                            ComponentLinearity::TimeVariant => {
                                comp.stamp_time_variant(&mut iter_matrix.as_mut(), &ctx);
                            },
                            _ => {}
                        }
                    }
                }

                let lu = iter_matrix.partial_piv_lu();
                let next_x_n = lu.solve(&iter_rhs);

                let diff = &next_x_n - &x_n;
                let error = diff.norm_max();

                x_n = &x_n + &(&diff * damping_factor);

                self.current_solution.subrows_mut(l_size, n_size).copy_from(&x_n);

                if error < T::real_part_impl(&T::from(tolerance).unwrap()) {
                    converged = true;
                    break;
                }
            }

            if converged { break }

            // Reduce damping factor and try again
            // This helps with convergence in tough non-linear cases but slows down the simulation.
            damping_factor = damping_factor * 0.5;

            println!(
                "Reducing damping factor to {} for better convergence",
                damping_factor
            );
        }

        let adjustment = &state.t_mat * &x_n;
        let x_l = v_temp - adjustment;

        for i in 0..l_size {
            self.current_solution[i] = x_l[i];
        }
        for i in 0..n_size {
            self.current_solution[l_size + i] = x_n[i];
        }

        for idx in &mut self.graph.node_indices() {
            if let CircuitElement::Device(ref mut comp) = self.graph[idx] {
                comp.update_state(&self.current_solution.as_ref(), &ctx);
            }
        }

        // Data Collection
        let mut voltages = vec![0.0; self.num_nodes + 1];
        let mut currents = Vec::with_capacity(self.total_terminals);
        let mut observables = Vec::with_capacity(self.total_observables);

        if let Some(partition) = &self.partition {
            for (node_id, &matrix_row_idx) in &partition.node_map {
                // Ensure the solution has this row (it should, but safety first)
                if matrix_row_idx < self.current_solution.nrows() {
                    let val = self.current_solution[matrix_row_idx].to_f64().unwrap();

                    // Place the voltage at the index corresponding to the NodeId
                    if node_id.0 < voltages.len() {
                        voltages[node_id.0] = val;
                    }
                }
            }
        }

        for &graph_idx in &self.component_order {
            if let CircuitElement::Device(comp) = &self.graph[graph_idx] {
                let term_currents = comp.terminal_currents(&self.current_solution.as_ref(), &ctx);
                for val in term_currents {
                    currents.push(val.to_f64().unwrap());
                }

                let obs_vals = comp.calculate_observables(&self.current_solution.as_ref(), &ctx);
                for val in obs_vals {
                    observables.push(val.to_f64().unwrap());
                }
            }
        }

        self.step_buffer.push(SimStepData {
            time: self.time.to_f64().unwrap(),
            voltages,
            currents,
            observables,
        });

        // Update history and time
        self.previous_solution.copy_from(&self.current_solution);
        self.time = self.time + dt;
        self.step_count += 1;
    }

    /// Returns the accumulated history and clears the internal buffer
    pub fn extract_history(&mut self) -> Vec<SimStepData> {
        std::mem::take(&mut self.step_buffer)
    }

    /// Reads the voltage of a specific node from the last solution
    pub fn get_node_voltage(&self, node: NodeId) -> T {
        if node.0 == 0 {
            // Ground is always 0
            T::zero()
        } else {
            let matrix_index = node.0 - 1;

            if matrix_index < self.num_nodes {
                self.current_solution[matrix_index]
            } else {
                println!(
                    "Warning: Requested voltage for non-existent node {}",
                    node.0
                );
                T::zero()
            }
        }
    }

    /// Read the terminal currents for a component at the current time step. The order of currents corresponds to the order of ports returned by `Component::ports()`.
    pub fn get_terminal_current(&self, component_idx: usize, dt: T) -> Vec<T> {
        /*if component_idx < self.components.len() {
            self.components[component_idx].terminal_currents(
                &self.current_solution.as_ref(),
                &SimulationContext {
                    dt,
                    time: self.time,
                    step: self.step_count,
                },
            )
        } else {
            vec![]
        }*/

        // map the component idx to the graph index
        if let Some(graph_idx) = self.component_order.get(component_idx) {
            if let CircuitElement::Device(comp) = &self.graph[*graph_idx] {
                comp.terminal_currents(
                    &self.current_solution.as_ref(),
                    &SimulationContext {
                        dt,
                        time: self.time,
                        step: self.step_count,
                        node_map: self.partition.as_ref().unwrap().node_map.clone(),
                    },
                )
            } else {
                vec![]
            }
        } else {
            vec![]
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
