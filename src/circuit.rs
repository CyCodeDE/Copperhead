use crate::components::resistor::Resistor;
use crate::model::{CircuitScalar, Component, NodeId, SimulationContext};
use crate::ui::SimStepData;
use faer::prelude::Solve;
use faer::{Col, Mat};

pub struct Circuit<T: CircuitScalar> {
    pub components: Vec<Box<dyn Component<T>>>,
    num_nodes: usize,
    matrix_size: usize,

    previous_solution: Col<T>,
    current_solution: Col<T>,

    pub time: T,
    step_count: usize,

    pub step_buffer: Vec<SimStepData>,

    pub total_terminals: usize,
    pub total_observables: usize,
}

impl<T: CircuitScalar> Circuit<T> {
    pub fn new() -> Self {
        Self {
            components: Vec::new(),
            num_nodes: 0,
            matrix_size: 0,
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
        // Track max node ID to size the matrix later
        for node in component.ports() {
            self.num_nodes = self.num_nodes.max(node.0);
        }

        self.total_observables += component.probe_definitions().len();
        self.total_terminals += component.ports().len();
        self.components.push(component);
    }

    /// Prepares the MNA system
    pub fn prepare(&mut self) {
        // Auxiliary rows start after Node rows
        let mut current_aux_idx = self.num_nodes;

        for comp in &mut self.components {
            let needed = comp.auxiliary_row_count();
            if needed > 0 {
                comp.set_auxiliary_index(current_aux_idx);
                current_aux_idx += needed;
            }
        }

        self.matrix_size = current_aux_idx;

        // TODO: run a DC operating point to populate initial conditions
        self.previous_solution = Col::<T>::zeros(self.matrix_size);
        self.current_solution = Col::<T>::zeros(self.matrix_size);
    }

    pub fn solve_step(&mut self, dt: T) {
        let size = self.matrix_size;

        // Initialize the solution guess for this time step.
        // A good guess is the solution from the previous time step.
        // If we don't do this, the diodes for example starts at 0V (off) every step,
        // requiring more iterations to turn on.
        if self.previous_solution.nrows() == size {
            self.current_solution.copy_from(&self.previous_solution);
        } else {
            // First run or resize
            self.current_solution = Col::<T>::zeros(size);
        }

        let ctx = SimulationContext {
            dt,
            time: self.time,
            step: self.step_count,
        };

        let mut base_matrix = Mat::<T>::zeros(size, size);
        let mut base_rhs = Col::<T>::zeros(size);

        // Stamp Static (Resistors)
        for comp in &self.components {
            comp.stamp_static(&mut base_matrix.as_mut());
        }

        // Stamp Dynamic (Capacitor/Inductor history)
        // used previous_solution here because dynamic history depends on t-1
        for comp in &mut self.components {
            comp.stamp_dynamic(
                &self.previous_solution.as_ref(),
                &mut base_rhs.as_mut(),
                &ctx,
            );
        }

        // Newton-Raphson Iteration Loop
        let max_iterations = 100;
        // Tolerance: 1 microvolt / 1 picoamp
        let tolerance = T::from(1e-6).unwrap();

        let mut converged = false;
        let mut damping_factor = T::from(1.0).unwrap();

        let is_nonlinear_circuit = self.components.iter().any(|c| !c.is_linear());

        // try to converge, if not, reduce damping factor
        for _attempt in 0..3 {
            for _iter in 0..max_iterations {
                let mut iter_matrix = base_matrix.clone();
                let mut iter_rhs = base_rhs.clone();

                for comp in &self.components {
                    comp.stamp_nonlinear(
                        &self.current_solution.as_ref(),
                        &mut iter_matrix.as_mut(),
                        &mut iter_rhs.as_mut(),
                        &ctx,
                    );
                }

                let lu = iter_matrix.partial_piv_lu();
                let next_solution = lu.solve(&iter_rhs);

                if !is_nonlinear_circuit {
                    // Linear circuit optimization
                    self.current_solution = next_solution;
                    converged = true;
                    break;
                }

                // Global Voltage Convergence
                let mut global_voltage_converged = true;
                let mut max_error = T::zero();
                for i in 0..size {
                    let diff = (next_solution[i] - self.current_solution[i]).abs();
                    if diff > max_error {
                        max_error = diff;
                    }
                }

                if max_error > tolerance {
                    global_voltage_converged = false;
                }

                // Component Physics Convergence
                let mut devices_converged = true;
                for comp in &self.components {
                    if !comp.is_linear() {
                        if !comp.is_converged(&next_solution.as_ref()) {
                            devices_converged = false;
                        }
                    }
                }

                // Update solution for next iteration
                self.current_solution = &self.current_solution
                    + &(&(&next_solution - &self.current_solution)
                        * damping_factor.to_f64().unwrap());

                // Only stop if BOTH agree
                if global_voltage_converged && devices_converged {
                    converged = true;
                    break;
                }
            }

            if converged {
                break;
            } else {
                // Reduce damping factor and try again
                // This helps with convergence in tough non-linear cases but slows down the simulation.
                damping_factor = damping_factor * T::from(0.5).unwrap();

                println!(
                    "Reducing damping factor to {} for better convergence",
                    damping_factor.to_f64().unwrap()
                );
            }
        }

        // Post-Solve update
        for comp in &mut self.components {
            comp.update_state(&self.current_solution.as_ref(), &ctx);
        }

        // Data Collection
        let mut voltages = Vec::with_capacity(self.num_nodes + 1);
        let mut currents = Vec::with_capacity(self.total_terminals);
        let mut observables = Vec::with_capacity(self.total_observables);

        voltages.push(0.0);
        for i in 0..self.num_nodes {
            if i < self.current_solution.nrows() {
                voltages.push(self.current_solution[i].to_f64().unwrap())
            } else {
                voltages.push(0.0)
            }
        }

        for comp in &self.components {
            let term_currents = comp.terminal_currents(&self.current_solution.as_ref(), &ctx);
            for val in term_currents {
                currents.push(val.to_f64().unwrap());
            }

            let obs_vals = comp.calculate_observables(&self.current_solution.as_ref(), &ctx);
            for val in obs_vals {
                observables.push(val.to_f64().unwrap());
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
        if component_idx < self.components.len() {
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
