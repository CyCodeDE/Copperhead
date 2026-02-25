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
use crate::circuit::NodePartition;
use crate::components::audio_probe::AudioProbe;
use crate::components::capacitor::Capacitor;
use crate::components::diode::Diode;
use crate::components::inductor::Inductor;
use crate::components::resistor::Resistor;
use crate::components::transistor::bjt::Bjt;
use crate::components::triode::Triode;
use crate::components::voltage_source::VoltageSource;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use faer::{ColMut, ColRef, MatMut};
use std::collections::HashMap;

pub mod audio_probe;
pub mod capacitor;
pub mod diode;
pub mod inductor;
pub mod resistor;
pub mod transistor;
pub mod triode;
pub mod voltage_source;

pub trait InsertIntoSoA<T: CircuitScalar> {
    fn insert_into(self, components: &mut CircuitComponents<T>) -> ComponentId;
}

macro_rules! define_circuit_components {
    (
        $( $field:ident : $comp_type:ident ),* $(,)?
    ) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum ComponentId {
            $( $comp_type(usize), )*
        }

        pub struct CircuitComponents<T: CircuitScalar> {
            $( pub $field: Vec<$comp_type<T>>, )*
        }

        impl<T: CircuitScalar> CircuitComponents<T> {
            pub fn new() -> Self {
                Self {
                    $( $field: Vec::new(), )*
                }
            }

            #[inline(always)]
            pub fn update_all_states(&mut self, current_sol: &ColRef<T>, ctx: &SimulationContext<T>) {
                $(
                    for comp in &mut self.$field {
                        comp.update_state(current_sol, ctx);
                    }
                )*
            }

            // Generate baking loop (called during prepare)
            pub fn bake_all_indices(&mut self, ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
                $(
                    for comp in &mut self.$field {
                        comp.bake_indices(ctx, node_map);
                    }
                )*
            }

            // Generate a static stamping loop
            #[inline(always)]
            pub fn stamp_all_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
                $(
                    for comp in &self.$field {
                        comp.stamp_static(matrix, ctx);
                    }
                )*
            }

            // Generate a dynamic stamping loop
            #[inline(always)]
            pub fn stamp_all_dynamic(&mut self, prev: &ColRef<T>, b: &mut ColMut<T>, ctx: &SimulationContext<T>) {
                $(
                    for comp in &mut self.$field {
                        // Assuming all components implement a trait or have this method
                        comp.stamp_dynamic(prev, b, ctx);
                    }
                )*
            }

            // Generate a non-linear stamping loop
            #[inline(always)]
            pub fn stamp_all_nonlinear(
                &mut self,
                current_sol: &ColRef<T>,
                iter_matrix: &mut MatMut<T>,
                iter_rhs: &mut ColMut<T>,
                ctx: &SimulationContext<T>,
                l_size: usize,
            ) {
                $(
                    for comp in &mut self.$field {
                        if let ComponentLinearity::NonLinear = comp.linearity() {
                            comp.stamp_nonlinear(current_sol, iter_matrix, iter_rhs, ctx, l_size);
                        }
                    }
                )*
            }

            #[inline(always)]
            pub fn stamp_all_dynamic_and_nonlinear(
                &mut self,
                current_sol: &ColRef<T>,
                iter_matrix: &mut MatMut<T>,
                iter_rhs: &mut ColMut<T>,
                ctx: &SimulationContext<T>,
                l_size: usize,
            ) {
                $(
                    for comp in &mut self.$field {
                        match comp.linearity() {
                            ComponentLinearity::NonLinear => {
                                comp.stamp_nonlinear(current_sol, iter_matrix, iter_rhs, ctx, l_size);
                            }
                            ComponentLinearity::TimeVariant => {
                                comp.stamp_time_variant(iter_matrix, ctx);
                            }
                            _ => {} // Completely stripped out for static components
                        }
                    }
                )*
            }

            pub fn find_retained_nodes(&self, node_status: &mut HashMap<NodeId, NodePartition>) {
            $(
                for comp in &self.$field {
                    match comp.linearity() {
                        ComponentLinearity::TimeVariant | ComponentLinearity::NonLinear => {
                            // Assuming your components have a method to return their connected NodeIds
                            for node_id in comp.ports() {
                                if node_id != NodeId(0) {
                                    node_status.insert(node_id, NodePartition::Retained);
                                }
                            }
                        }
                        _ => {}
                    }
                }
            )*
            }

            #[inline(always)]
            pub fn get_terminal_currents(
                &self,
                id: ComponentId,
                current_sol: &ColRef<T>,
                ctx: &SimulationContext<T>,
                out_currents: &mut [T],
            ) {
                match id {
                    $(
                        ComponentId::$comp_type(idx) => {
                            self.$field[idx].terminal_currents(current_sol, ctx, out_currents);
                        }
                    )*
                }
            }

            #[inline(always)]
            pub fn get_observables(
                &self,
                id: ComponentId,
                current_sol: &ColRef<T>,
                ctx: &SimulationContext<T>,
                out_observables: &mut [T],
            ) {
                match id {
                    $(
                        ComponentId::$comp_type(idx) => {
                            self.$field[idx].calculate_observables(current_sol, ctx, out_observables);
                        }
                    )*
                }
            }

            #[inline(always)]
            pub fn get_num_terminals(&self, id: ComponentId) -> usize {
                match id {
                    $(
                        ComponentId::$comp_type(idx) => {
                            self.$field[idx].ports().len()
                        }
                    )*
                }
            }

            #[inline(always)]
            pub fn get_probe_definitions(&self, id: ComponentId) -> Vec<ComponentProbe> {
                match id {
                    $(
                        ComponentId::$comp_type(idx) => {
                            self.$field[idx].probe_definitions()
                        }
                    )*
                }
            }

            // Pass 1: Used by partition()
            pub fn count_auxiliary_rows(&self, node_status: &HashMap<NodeId, NodePartition>) -> (usize, usize) {
                let mut num_l_aux = 0;
                let mut num_n_aux = 0;

                $(
                    for comp in &self.$field {
                        let needed = comp.auxiliary_row_count();
                        if needed > 0 {
                            // Assuming components have a .nodes() method returning connected NodeIds
                            let touches_nonlinear = comp.ports().iter().any(|node_id| {
                                matches!(node_status.get(node_id), Some(NodePartition::Retained))
                            });

                            match comp.linearity() {
                                ComponentLinearity::LinearStatic | ComponentLinearity::LinearDynamic
                                    if !touches_nonlinear =>
                                {
                                    num_l_aux += needed;
                                }
                                _ => {
                                    num_n_aux += needed;
                                }
                            }
                        }
                    }
                )*

                (num_l_aux, num_n_aux)
            }

            // Pass 2: Used by prepare()
            pub fn assign_auxiliary_rows(
                &mut self,
                partition_map: &HashMap<NodeId, usize>,
                n_block_start: usize,
                mut l_aux_start: usize,
                mut n_aux_start: usize,
            ) {
                $(
                    for comp in &mut self.$field {
                        let needed = comp.auxiliary_row_count();
                        if needed > 0 {
                            let touches_nonlinear = comp.ports().iter().any(|node_id| {
                                if let Some(&row_idx) = partition_map.get(node_id) {
                                    row_idx >= n_block_start
                                } else {
                                    false
                                }
                            });

                            match comp.linearity() {
                                ComponentLinearity::LinearStatic | ComponentLinearity::LinearDynamic
                                    if !touches_nonlinear =>
                                {
                                    comp.set_auxiliary_index(l_aux_start);
                                    l_aux_start += needed;
                                }
                                _ => {
                                    comp.set_auxiliary_index(n_aux_start);
                                    n_aux_start += needed;
                                }
                            }
                        }
                    }
                )*
            }
        }

        // Generate the routing logic for every component
        $(
            impl<T: CircuitScalar> InsertIntoSoA<T> for $comp_type<T> {
                #[inline(always)]
                fn insert_into(self, components: &mut CircuitComponents<T>) -> ComponentId {
                    let index = components.$field.len();
                    components.$field.push(self);
                    ComponentId::$comp_type(index)
                }
            }
        )*
    };
}

define_circuit_components!(
    resistors: Resistor,
    capacitors: Capacitor,
    inductors: Inductor,
    voltage_sources: VoltageSource,
    diodes: Diode,
    bjts: Bjt,
    triodes: Triode,
    audio_probes: AudioProbe,
);

/// The interface a component must implement.
pub trait Component<T: CircuitScalar> {
    /// Returns true if the component allows the matrix A to be pre-solved
    /// (e.g. Resistors, Capacitors, Inductors with fixed sample rate)
    /// Returns false for non-linear components (Diodes, Tubes, Transistors, etc.)
    fn linearity(&self) -> ComponentLinearity;

    /// Returns the nodes this component is connected to
    fn ports(&self) -> Vec<NodeId>;

    /// Called after partitioning, but before solving
    /// Gives the component the ability to cache the matrix indices for ports and auxiliary rows
    fn bake_indices(&mut self, ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {}

    /// How many extra rows/cols does this component add to the matrix?
    /// For example Resistors = 0, Voltage Sources = 1, ...
    fn auxiliary_row_count(&self) -> usize {
        0
    }

    /// Called to inform the component where its rows live in the matrix
    /// `start_idx` is the row index in the matrix (already offset by num_nodes)
    fn set_auxiliary_index(&mut self, _start_idx: usize) {
        // Default: Do nothing
    }

    /// Called when the circuit is built
    /// Used for components that do not change over time
    /// (e.g. Resistors, discretized capacitors and inductors with fixed sample rate)
    /// Adds G (conductance) values to the A Matrix.
    fn stamp_static(&self, _matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {}

    /// Time step preparation
    /// Called at the start of every audio sample
    /// Used to calculate the history current for capacitors and inductors based on previous voltages
    /// Modifies the Vector b
    fn stamp_dynamic(
        &mut self,
        prev_node_voltages: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
    }

    /// Used for Time-Variant components (e.g. Potentiometer, LDR, Switch) that change their conductance G at runtime
    /// in the reduced matrix without re-inverting A_LL every time step.
    fn stamp_time_variant(&self, _matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {}

    /// Non-Linear iteration (Newton-Raphson)
    /// Called multiple times per sample
    /// Only for Diodes, Tubes, Transistors, etc.
    /// Modifies Matrix A (Jacobian) and Vector b (Current correction)
    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        _matrix: &mut MatMut<T>,
        _rhs: &mut ColMut<T>,
        _ctx: &SimulationContext<T>,
        _l_size: usize,
    ) {
    }

    /// Post-Step update
    /// Called after the solver found the solution for the current frame
    /// Used to update internal state (e.g. capacitor charge)
    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {}

    fn is_converged(&self, _current_node_voltages: &ColRef<T>) -> bool {
        false
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![]
    }

    /// Calculates the values for the probes defined in `probe_definitions`
    /// TODO: pass a mutable slice of the probe values to avoid allocations in the audio thread.
    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
    }

    /// Calculates the exact current flowing INTO every port defined in `ports()`
    /// For example for a BJT with ports [C, B, E], this returns [I_C, I_B, I_E]
    /// KCL defines that theoretically the sums should be zero
    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    );

    /// Updates a parameter by name.
    /// Returns true if the static matrix needs to be rebuilt.
    /// For example if a Resistor value changes -> true
    /// If a Voltage source amplitude changes, return false since it only affects the b vector.
    fn set_parameter(&mut self, name: &str, value: T, ctx: &SimulationContext<T>) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
pub struct ComponentProbe {
    pub name: String,
    pub unit: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ComponentLinearity {
    /// Conductance G is constant (e.g. Fixed Resistor)
    /// Goes into the Static Block (A_LL).
    LinearStatic,
    /// G is constant, b changes per sample (e.g. Capacitor, Inductor)
    /// Goes into the Static Block (A_LL), but updates b_L every step.
    LinearDynamic,
    /// G changes at runtime (e.g. Potentiometer, LDR, Switch)
    /// Must be in the Active Block (A_NN) to avoid re-inverting A_LL.
    TimeVariant,
    /// G changes during iteration (e.g. Diode, BJT, Tube)
    /// Must be in the Active Block (A_NN).
    NonLinear,
}
