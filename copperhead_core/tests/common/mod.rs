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

//! Shared test utilities for copperhead_core integration tests.
//!
//! Included into each test binary via `mod common;`.
//! All items here are `#[allow(dead_code)]` because not every test module
//! uses every helper.

#![allow(dead_code)]

use std::f64::consts::PI;
use std::sync::Arc;

use copperhead_core::circuit::Circuit;
use copperhead_core::components::capacitor::Capacitor;
use copperhead_core::components::diode::{DiodeDef, DiodeModel};
use copperhead_core::components::inductor::Inductor;
use copperhead_core::components::resistor::Resistor;
use copperhead_core::components::voltage_source::VoltageSource;
use copperhead_core::descriptor::Instantiable;
use copperhead_core::model::NodeId;
use copperhead_core::parameter::parser::BuiltinValues;
use copperhead_core::parameter::{FormulaInstance, ParamSystem, ParamSystemBuilder, ParamValue};
use copperhead_core::signals::{ConstantSignal, SineSignal, SignalType};

// ---------------------------------------------------------------------------
// Numeric tolerance helpers
// ---------------------------------------------------------------------------

/// Asserts that `actual` is within `tol` of `expected`, with a descriptive panic message.
pub fn assert_approx(actual: f64, expected: f64, tol: f64) {
    let err = (actual - expected).abs();
    assert!(
        err < tol,
        "assert_approx failed: expected {expected:.10}, got {actual:.10} (err={err:.3e}, tol={tol:.3e})"
    );
}

/// Asserts that `actual` is strictly within the half-open interval `[lo, hi)`.
pub fn assert_in_range(actual: f64, lo: f64, hi: f64) {
    assert!(
        actual >= lo && actual < hi,
        "assert_in_range failed: expected value in [{lo:.6}, {hi:.6}), got {actual:.6}"
    );
}

// ---------------------------------------------------------------------------
// BuiltinValues factories
// ---------------------------------------------------------------------------

/// Builtins for typical audio tests: 96 000 Hz sample rate.
pub fn audio_builtins() -> BuiltinValues {
    let dt = 1.0 / 96000.0;
    BuiltinValues {
        sample_rate: 96000.0,
        time_step: dt,
        simulation_time: 0.0,
        step_index: 0.0,
        bpm: 120.0,
        oversampling_factor: 1.0,
    }
}

/// Builtins with dt = 1.0 — convenient for smooth() convergence tests where
/// you don't want to run many iterations just to reach steady state.
pub fn slow_builtins() -> BuiltinValues {
    BuiltinValues {
        sample_rate: 1.0,
        time_step: 1.0,
        simulation_time: 0.0,
        step_index: 0.0,
        bpm: 120.0,
        oversampling_factor: 1.0,
    }
}

// ---------------------------------------------------------------------------
// ParamSystem factories
// ---------------------------------------------------------------------------

/// Build a ParamSystem with the given named float parameters.
pub fn param_system_with(params: &[(&str, f64)]) -> ParamSystem {
    let mut b = ParamSystemBuilder::new();
    for &(name, default) in params {
        b.declare_param(name, default);
    }
    b.build()
}

/// Build an empty ParamSystem (no parameters, no voltages).
pub fn empty_param_system() -> ParamSystem {
    ParamSystemBuilder::new().build()
}

// ---------------------------------------------------------------------------
// Formula evaluation helpers
// ---------------------------------------------------------------------------

/// Compile `src` against an empty ParamSystem and evaluate it once with
/// default builtins and no node voltages. Panics if compilation fails.
pub fn eval_formula(src: &str) -> f64 {
    let sys = empty_param_system();
    let prog = Arc::new(sys.compile(src).expect("formula should compile"));
    let mut inst = FormulaInstance::new(prog, sys.table());
    inst.eval(&[], &BuiltinValues::default())
}

/// Like `eval_formula` but with explicit builtins (e.g. to set time_step for
/// smooth() or simulation_time for lfo_*).
pub fn eval_formula_with_builtins(src: &str, builtins: &BuiltinValues) -> f64 {
    let sys = empty_param_system();
    let prog = Arc::new(sys.compile(src).expect("formula should compile"));
    let mut inst = FormulaInstance::new(prog, sys.table());
    inst.eval(&[], builtins)
}

/// Evaluate `src` N times on the same FormulaInstance and return the last
/// value. Needed for formulas with state (smooth, LFO).
pub fn eval_formula_n_times(src: &str, n: usize, builtins: &BuiltinValues) -> f64 {
    let sys = empty_param_system();
    let prog = Arc::new(sys.compile(src).expect("formula should compile"));
    let mut inst = FormulaInstance::new(prog, sys.table());
    let mut last = 0.0;
    for _ in 0..n {
        last = inst.eval(&[], builtins);
    }
    last
}

// ---------------------------------------------------------------------------
// Circuit test builder
// ---------------------------------------------------------------------------

/// Default audio-rate time step (96 kHz).
pub const AUDIO_DT: f64 = 1.0 / 96000.0;

/// A thin wrapper around `Circuit<f64>` that provides convenience methods
/// for building and solving test circuits without boilerplate.
///
/// # Node convention
/// `NodeId(0)` is always ground. Use `tc.node()` to allocate fresh nodes
/// starting from `NodeId(1)`, or `TestCircuit::gnd()` for the ground node.
pub struct TestCircuit {
    pub circuit: Circuit<f64>,
    next_node: usize,
    pub dt: f64,
}

impl TestCircuit {
    /// Create a test circuit running at 96 kHz (dt = 1/96000).
    pub fn new() -> Self {
        Self::with_dt(AUDIO_DT)
    }

    /// Create a test circuit with an explicit time step.
    pub fn with_dt(dt: f64) -> Self {
        Self {
            circuit: Circuit::new(),
            next_node: 1, // 0 is reserved for ground
            dt,
        }
    }

    /// Allocate a fresh node ID (never reuses IDs).
    pub fn node(&mut self) -> NodeId {
        let id = NodeId(self.next_node);
        self.next_node += 1;
        id
    }

    /// The ground node (NodeId(0), always 0 V).
    pub fn gnd() -> NodeId {
        NodeId(0)
    }

    // ---- component builders ----

    /// Add a constant DC voltage source: `v` volts from `pos` to `neg`.
    pub fn add_dc_source(&mut self, pos: NodeId, neg: NodeId, v: f64) {
        let sig = SignalType::Constant(ConstantSignal { voltage: v });
        self.circuit.add_component(VoltageSource::new(pos, neg, sig));
    }

    /// Add a sine-wave voltage source.
    pub fn add_sine_source(&mut self, pos: NodeId, neg: NodeId, amplitude: f64, frequency: f64) {
        let omega = 2.0 * PI * frequency;
        let sig = SignalType::Sine(SineSignal {
            amplitude,
            frequency,
            phase: 0.0,
            omega,
        });
        self.circuit.add_component(VoltageSource::new(pos, neg, sig));
    }

    /// Add a resistor (constant resistance in Ohms).
    pub fn add_resistor(&mut self, a: NodeId, b: NodeId, r: f64) {
        self.circuit
            .add_component(Resistor::new(a, b, ParamValue::Constant(r)));
    }

    /// Add a capacitor (zero ESR) in Farads.
    pub fn add_capacitor(&mut self, a: NodeId, b: NodeId, c: f64) {
        let dt = self.dt;
        self.circuit.add_component(Capacitor::new(
            a,
            b,
            ParamValue::Constant(c),
            ParamValue::Constant(0.0),
            dt,
        ));
    }

    /// Add a capacitor with explicit ESR.
    pub fn add_capacitor_esr(&mut self, a: NodeId, b: NodeId, c: f64, esr: f64) {
        let dt = self.dt;
        self.circuit.add_component(Capacitor::new(
            a,
            b,
            ParamValue::Constant(c),
            ParamValue::Constant(esr),
            dt,
        ));
    }

    /// Add an inductor (inductance in Henrys, series resistance in Ohms).
    pub fn add_inductor(&mut self, a: NodeId, b: NodeId, l: f64, rs: f64) {
        let dt = self.dt;
        self.circuit.add_component(Inductor::new(
            a,
            b,
            ParamValue::Constant(l),
            ParamValue::Constant(rs),
            dt,
        ));
    }

    /// Add a diode from `anode` to `cathode` using the given built-in model.
    pub fn add_diode(&mut self, anode: NodeId, cathode: NodeId, model: DiodeModel) {
        let def = DiodeDef { model };
        def.instantiate(&[anode, cathode], self.dt, &mut self.circuit, 1024);
    }

    // ---- solver ----

    /// Solve the DC operating point. Returns `Err` if Newton-Raphson fails to
    /// converge. The circuit retains the DC solution as the starting point for
    /// any subsequent transient simulation.
    pub fn solve_dc(&mut self) -> Result<(), String> {
        self.circuit
            .calculate_dc_operating_point(1e-9, 50, self.dt)
    }

    /// Rebuild the solver for transient analysis. Must be called after
    /// `solve_dc()` and before the first `solve_steps()` call.
    pub fn prepare_transient(&mut self) {
        self.circuit.prepare(self.dt, false);
    }

    /// Advance the simulation by `n` time steps of length `self.dt`.
    pub fn solve_steps(&mut self, n: usize) {
        for _ in 0..n {
            self.circuit.solve_step(self.dt);
        }
    }

    // ---- observation ----

    /// Read the current voltage at `node` (in Volts). Ground is always 0 V.
    pub fn v(&self, node: NodeId) -> f64 {
        self.circuit.get_node_voltage(node)
    }

    /// Number of transient steps completed (including the DC step).
    pub fn step_count(&self) -> usize {
        self.circuit.step_count
    }
}

impl Default for TestCircuit {
    fn default() -> Self {
        Self::new()
    }
}
