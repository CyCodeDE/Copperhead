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

//! Tests for the classification predicates on `Program` and `ParamValue`,
//! and for `resolve_param_value`.
//!
//! These predicates drive linearity decisions in the solver (LinearStatic vs
//! TimeVariant vs NonLinear) so it is important that they are correct.

mod common;

use std::sync::Arc;

use copperhead_core::parameter::{ParamSystemBuilder, ParamValue, resolve_param_value};

use common::{empty_param_system, param_system_with};

// ---------------------------------------------------------------------------
// Program::depends_on_voltage
// ---------------------------------------------------------------------------

#[test]
fn constant_formula_does_not_depend_on_voltage() {
    let sys = empty_param_system();
    let prog = sys.compile("42").unwrap();
    assert!(!prog.depends_on_voltage());
}

#[test]
fn param_only_formula_does_not_depend_on_voltage() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let prog = sys.compile("gain * 1000").unwrap();
    assert!(!prog.depends_on_voltage());
}

#[test]
fn builtin_formula_does_not_depend_on_voltage() {
    let sys = empty_param_system();
    let prog = sys.compile("t * 1000").unwrap();
    assert!(!prog.depends_on_voltage());
}

#[test]
fn voltage_formula_depends_on_voltage() {
    let mut b = ParamSystemBuilder::new();
    b.declare_voltage("vout");
    let sys = b.build();
    let prog = sys.compile("vout").unwrap();
    assert!(prog.depends_on_voltage());
}

#[test]
fn mixed_voltage_and_param_depends_on_voltage() {
    let mut b = ParamSystemBuilder::new();
    b.declare_param("gain", 1.0);
    b.declare_voltage("vout");
    let sys = b.build();
    let prog = sys.compile("gain * vout").unwrap();
    assert!(prog.depends_on_voltage());
}

// ---------------------------------------------------------------------------
// Program::depends_on_builtins
// ---------------------------------------------------------------------------

#[test]
fn constant_formula_does_not_depend_on_builtins() {
    let sys = empty_param_system();
    let prog = sys.compile("1 + 2").unwrap();
    assert!(!prog.depends_on_builtins());
}

#[test]
fn simulation_time_depends_on_builtins() {
    let sys = empty_param_system();
    let prog = sys.compile("t").unwrap();
    assert!(prog.depends_on_builtins());
}

#[test]
fn sample_rate_depends_on_builtins() {
    let sys = empty_param_system();
    let prog = sys.compile("sr").unwrap();
    assert!(prog.depends_on_builtins());
}

#[test]
fn time_step_depends_on_builtins() {
    let sys = empty_param_system();
    let prog = sys.compile("dt").unwrap();
    assert!(prog.depends_on_builtins());
}

#[test]
fn step_index_depends_on_builtins() {
    let sys = empty_param_system();
    let prog = sys.compile("step").unwrap();
    assert!(prog.depends_on_builtins());
}

#[test]
fn lfo_depends_on_builtins() {
    // lfo_sine uses simulation_time internally.
    let sys = empty_param_system();
    let prog = sys.compile("lfo_sine(1)").unwrap();
    assert!(prog.depends_on_builtins());
}

#[test]
fn smooth_depends_on_builtins() {
    // smooth uses time_step internally.
    let sys = empty_param_system();
    let prog = sys.compile("smooth(1, 0.01)").unwrap();
    assert!(prog.depends_on_builtins());
}

// ---------------------------------------------------------------------------
// Program::is_freeze_eligible
// ---------------------------------------------------------------------------

#[test]
fn constant_is_freeze_eligible() {
    let sys = empty_param_system();
    let prog = sys.compile("100").unwrap();
    assert!(prog.is_freeze_eligible());
}

#[test]
fn param_only_is_freeze_eligible() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let prog = sys.compile("gain * 1000").unwrap();
    assert!(prog.is_freeze_eligible());
}

#[test]
fn voltage_dependent_is_not_freeze_eligible() {
    let mut b = ParamSystemBuilder::new();
    b.declare_voltage("vout");
    let sys = b.build();
    let prog = sys.compile("vout").unwrap();
    assert!(!prog.is_freeze_eligible());
}

#[test]
fn time_dependent_is_not_freeze_eligible() {
    let sys = empty_param_system();
    let prog = sys.compile("t * 100").unwrap();
    assert!(!prog.is_freeze_eligible());
}

#[test]
fn lfo_is_not_freeze_eligible() {
    let sys = empty_param_system();
    let prog = sys.compile("lfo_sine(1)").unwrap();
    assert!(!prog.is_freeze_eligible());
}

#[test]
fn smooth_is_not_freeze_eligible() {
    let sys = empty_param_system();
    let prog = sys.compile("smooth(1, 0.01)").unwrap();
    assert!(!prog.is_freeze_eligible());
}

// ---------------------------------------------------------------------------
// ParamValue::Constant classification
// ---------------------------------------------------------------------------

#[test]
fn param_value_constant_is_not_dynamic() {
    let pv = ParamValue::constant(1.0);
    assert!(!pv.is_dynamic());
}

#[test]
fn param_value_constant_does_not_depend_on_voltage() {
    let pv = ParamValue::constant(1.0);
    assert!(!pv.depends_on_voltage());
}

#[test]
fn param_value_constant_is_freeze_eligible() {
    let pv = ParamValue::constant(1.0);
    assert!(pv.is_freeze_eligible());
}

#[test]
fn constant_as_constant_returns_value() {
    let pv = ParamValue::constant(42.0);
    assert_eq!(pv.as_constant(), Some(42.0));
}

#[test]
fn constant_zero_as_constant() {
    let pv = ParamValue::constant(0.0);
    assert_eq!(pv.as_constant(), Some(0.0));
}

// ---------------------------------------------------------------------------
// ParamValue::Formula classification
// ---------------------------------------------------------------------------

#[test]
fn formula_is_dynamic() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let prog = Arc::new(sys.compile("gain").unwrap());
    let pv = ParamValue::formula(prog, sys.table());
    assert!(pv.is_dynamic());
}

#[test]
fn formula_as_constant_is_none() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let prog = Arc::new(sys.compile("gain").unwrap());
    let pv = ParamValue::formula(prog, sys.table());
    assert!(pv.as_constant().is_none());
}

#[test]
fn voltage_formula_depends_on_voltage_via_param_value() {
    let mut b = ParamSystemBuilder::new();
    b.declare_voltage("vout");
    let sys = b.build();
    let prog = Arc::new(sys.compile("vout").unwrap());
    let pv = ParamValue::formula(prog, sys.table());
    assert!(pv.depends_on_voltage());
}

#[test]
fn param_formula_does_not_depend_on_voltage_via_param_value() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let prog = Arc::new(sys.compile("gain").unwrap());
    let pv = ParamValue::formula(prog, sys.table());
    assert!(!pv.depends_on_voltage());
}

#[test]
fn param_only_formula_is_freeze_eligible_via_param_value() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let prog = Arc::new(sys.compile("gain * 1000").unwrap());
    let pv = ParamValue::formula(prog, sys.table());
    assert!(pv.is_freeze_eligible());
}

#[test]
fn builtin_formula_is_not_freeze_eligible_via_param_value() {
    let sys = empty_param_system();
    let prog = Arc::new(sys.compile("t * 100").unwrap());
    let pv = ParamValue::formula(prog, sys.table());
    assert!(!pv.is_freeze_eligible());
}

// ---------------------------------------------------------------------------
// resolve_param_value
// ---------------------------------------------------------------------------

#[test]
fn resolve_plain_integer_string_is_constant() {
    let sys = empty_param_system();
    let pv = resolve_param_value("42", &sys);
    assert_eq!(pv.as_constant(), Some(42.0));
}

#[test]
fn resolve_float_string_is_constant() {
    let sys = empty_param_system();
    let pv = resolve_param_value("3.16", &sys);
    assert_eq!(pv.as_constant(), Some(3.16));
}

#[test]
fn resolve_scientific_notation_is_constant() {
    let sys = empty_param_system();
    let pv = resolve_param_value("1e3", &sys);
    assert_eq!(pv.as_constant(), Some(1000.0));
}

#[test]
fn resolve_si_kilo_is_constant() {
    let sys = empty_param_system();
    let pv = resolve_param_value("10k", &sys);
    assert_eq!(pv.as_constant(), Some(10_000.0));
}

#[test]
fn resolve_si_mega_is_constant() {
    let sys = empty_param_system();
    let pv = resolve_param_value("1M", &sys);
    assert_eq!(pv.as_constant(), Some(1_000_000.0));
}

#[test]
fn resolve_si_nano_is_constant() {
    let sys = empty_param_system();
    let pv = resolve_param_value("100n", &sys);
    // 100 * 1e-9 = 1e-7
    let got = pv.as_constant().expect("should be constant");
    assert!((got - 1e-7).abs() < 1e-20, "expected 1e-7, got {got}");
}

#[test]
fn resolve_si_micro_is_constant() {
    let sys = empty_param_system();
    let pv = resolve_param_value("47u", &sys);
    let got = pv.as_constant().expect("should be constant");
    assert!((got - 47e-6).abs() < 1e-20, "expected 47e-6, got {got}");
}

#[test]
fn resolve_formula_string_is_dynamic() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let pv = resolve_param_value("gain * 1000", &sys);
    assert!(pv.is_dynamic(), "formula string should produce a dynamic ParamValue");
}

#[test]
fn resolve_identifier_expression_is_dynamic() {
    // "pi" doesn't parse as a plain f64 and doesn't start with a digit, so
    // parse_si returns None and resolve_param_value falls through to the
    // formula compiler. The result is a FormulaInstance (dynamic), even though
    // the program is just Op::Const(PI) internally.
    let sys = empty_param_system();
    let pv = resolve_param_value("pi", &sys);
    assert!(pv.is_dynamic());
}

#[test]
fn resolve_param_constant_is_not_dynamic() {
    let sys = empty_param_system();
    let pv = resolve_param_value("1000", &sys);
    assert!(!pv.is_dynamic());
}

// ---------------------------------------------------------------------------
// ParamSystem builder helpers
// ---------------------------------------------------------------------------

#[test]
fn declare_param_returns_stable_index() {
    let mut b = ParamSystemBuilder::new();
    let i0 = b.declare_param("gain", 1.0);
    let i1 = b.declare_param("freq", 440.0);
    // Indices must be different.
    assert_ne!(i0, i1);
}

#[test]
fn declare_param_idempotent() {
    let mut b = ParamSystemBuilder::new();
    let i0 = b.declare_param("gain", 1.0);
    let i1 = b.declare_param("gain", 1.0); // same name again
    assert_eq!(i0, i1, "re-declaring the same param should return the same index");
}

#[test]
fn param_system_lookup_by_name() {
    let mut b = ParamSystemBuilder::new();
    let idx = b.declare_param("cutoff", 1000.0);
    let sys = b.build();
    assert_eq!(sys.param_index("cutoff"), Some(idx));
}

#[test]
fn param_system_lookup_unknown_name_is_none() {
    let sys = empty_param_system();
    assert_eq!(sys.param_index("does_not_exist"), None);
}

#[test]
fn set_param_by_name_round_trips_through_table() {
    let sys = param_system_with(&[("volume", 0.5)]);
    let idx = sys.param_index("volume").unwrap();

    assert!(sys.set_param("volume", 0.8));
    assert_eq!(sys.table().get(idx), 0.8);
}

#[test]
fn set_param_unknown_name_returns_false() {
    let sys = empty_param_system();
    assert!(!sys.set_param("ghost", 1.0));
}

#[test]
fn param_table_default_value_accessible() {
    let sys = param_system_with(&[("drive", 0.75)]);
    let idx = sys.param_index("drive").unwrap();
    assert_eq!(sys.table().get(idx), 0.75);
}
