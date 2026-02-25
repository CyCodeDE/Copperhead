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
use egui::{Pos2, Vec2};
use faer::traits::ComplexField;
use faer::{ColMut, ColRef, MatMut};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::{Add, Sub};

/// The numerical trait.
/// For inference: T = f32
/// For high quality: T = f64
pub trait CircuitScalar:
    num_traits::Float + std::fmt::Debug + Copy + Send + Sync + ComplexField + Display + 'static
{
    // possibly helper methods for fast approximations
}

impl CircuitScalar for f32 {}
impl CircuitScalar for f64 {}

/// A handle to a node in the circuit (like a wire junction)
/// In the matrix, this corresponds to a Row/Column index.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

/// Data chunk for a single simulation step
#[derive(Clone, Debug)]
pub struct SimStepData {
    pub time: f64,
    // Index corresponds to NodeId
    pub voltages: Vec<f64>,
    // Flattened list of all terminal currents from all components
    pub currents: Vec<f64>,
    // Flattend list of all observables from all components
    // Layout: [Comp0_Probe0, Comp0_Probe1, Comp1_Probe0, ...]
    pub observables: Vec<f64>,
}

#[derive(Clone, Debug, Default)]
pub struct SimBatchData {
    pub times: Vec<f64>,
    pub voltages: Vec<f64>,
    pub currents: Vec<f64>,
    pub observables: Vec<f64>,

    // Metadata to help the UI reconstruct SimStepData
    pub nodes_per_step: usize,
    pub terminals_per_step: usize,
    pub observables_per_step: usize,
}

impl SimBatchData {
    pub fn clear_for_reuse(&mut self) {
        self.times.clear();
        self.voltages.clear();
        self.currents.clear();
        self.observables.clear();
    }
}

pub struct SimulationContext<T> {
    pub dt: T,
    pub time: T,
    pub step: usize,
    pub is_dc_analysis: bool,
}