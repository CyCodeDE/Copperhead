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

//! Tests for formula VM evaluation.
//!
//! Each test compiles a formula string and evaluates it, verifying the
//! concrete output value. Edge cases document the VM's deliberate NaN-safe
//! behavior (e.g. division by zero returns 0.0, not NaN).
//!
//! These tests go through the full compile → eval pipeline. VM opcode
//! isolation (building Programs by hand) is not needed because the parser
//! tests already verify compilation; here we only care about runtime values.

mod common;

use std::sync::Arc;

use copperhead_core::parameter::parser::BuiltinValues;
use copperhead_core::parameter::FormulaInstance;

use common::{
    audio_builtins, eval_formula, eval_formula_n_times, eval_formula_with_builtins,
    param_system_with, slow_builtins,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Assert |result - expected| <= tolerance and print a useful message.
fn assert_near(result: f64, expected: f64, tol: f64, label: &str) {
    assert!(
        (result - expected).abs() <= tol,
        "{label}: expected {expected}, got {result} (diff {})",
        (result - expected).abs()
    );
}

// ---------------------------------------------------------------------------
// Literals / constants
// ---------------------------------------------------------------------------

#[test]
fn literal_zero() {
    assert_eq!(eval_formula("0"), 0.0);
}

#[test]
fn literal_integer() {
    assert_eq!(eval_formula("42"), 42.0);
}

#[test]
fn literal_float() {
    assert_eq!(eval_formula("3.16"), 3.16);
}

#[test]
fn literal_true_is_one() {
    assert_eq!(eval_formula("true"), 1.0);
}

#[test]
fn literal_false_is_zero() {
    assert_eq!(eval_formula("false"), 0.0);
}

#[test]
fn constant_pi() {
    assert_eq!(eval_formula("pi"), std::f64::consts::PI);
}

#[test]
fn constant_tau() {
    assert_eq!(eval_formula("tau"), std::f64::consts::TAU);
}

#[test]
fn constant_e() {
    assert_eq!(eval_formula("e"), std::f64::consts::E);
}

// ---------------------------------------------------------------------------
// Arithmetic
// ---------------------------------------------------------------------------

#[test]
fn addition() {
    assert_eq!(eval_formula("1 + 2"), 3.0);
}

#[test]
fn subtraction() {
    assert_eq!(eval_formula("10 - 3"), 7.0);
}

#[test]
fn multiplication() {
    assert_eq!(eval_formula("3 * 4"), 12.0);
}

#[test]
fn division() {
    assert_eq!(eval_formula("10 / 5"), 2.0);
}

#[test]
fn modulo() {
    assert_eq!(eval_formula("7 % 3"), 1.0);
}

#[test]
fn power() {
    assert_eq!(eval_formula("2 ^ 3"), 8.0);
}

#[test]
fn unary_negation() {
    assert_eq!(eval_formula("-5"), -5.0);
}

#[test]
fn unary_negation_of_expression() {
    assert_eq!(eval_formula("-(3 + 2)"), -5.0);
}

#[test]
fn mul_before_add() {
    // Standard precedence: 3 * 4 = 12, then 2 + 12 = 14.
    assert_eq!(eval_formula("2 + 3 * 4"), 14.0);
}

#[test]
fn parentheses_override_precedence() {
    // (2 + 3) * 4 = 20.
    assert_eq!(eval_formula("(2 + 3) * 4"), 20.0);
}

// ---------------------------------------------------------------------------
// Math functions
// ---------------------------------------------------------------------------

#[test]
fn abs_of_negative() {
    assert_eq!(eval_formula("abs(-7)"), 7.0);
}

#[test]
fn abs_of_positive() {
    assert_eq!(eval_formula("abs(3)"), 3.0);
}

#[test]
fn floor() {
    assert_eq!(eval_formula("floor(2.7)"), 2.0);
    assert_eq!(eval_formula("floor(-2.3)"), -3.0);
}

#[test]
fn ceil() {
    assert_eq!(eval_formula("ceil(2.3)"), 3.0);
    assert_eq!(eval_formula("ceil(-2.7)"), -2.0);
}

#[test]
fn sin_of_zero() {
    assert_eq!(eval_formula("sin(0)"), 0.0);
}

#[test]
fn cos_of_zero() {
    assert_eq!(eval_formula("cos(0)"), 1.0);
}

#[test]
fn tanh_of_zero() {
    assert_eq!(eval_formula("tanh(0)"), 0.0);
}

#[test]
fn exp_of_zero_is_one() {
    assert_eq!(eval_formula("exp(0)"), 1.0);
}

#[test]
fn ln_of_one_is_zero() {
    assert_eq!(eval_formula("ln(1)"), 0.0);
}

#[test]
fn sqrt_of_four_is_two() {
    assert_eq!(eval_formula("sqrt(4)"), 2.0);
}

#[test]
fn pow_function() {
    assert_eq!(eval_formula("pow(2, 10)"), 1024.0);
}

#[test]
fn min_returns_smaller() {
    assert_eq!(eval_formula("min(3, 5)"), 3.0);
    assert_eq!(eval_formula("min(5, 3)"), 3.0);
}

#[test]
fn max_returns_larger() {
    assert_eq!(eval_formula("max(3, 5)"), 5.0);
    assert_eq!(eval_formula("max(5, 3)"), 5.0);
}

#[test]
fn clamp_below_lo() {
    assert_eq!(eval_formula("clamp(-1, 0, 3)"), 0.0);
}

#[test]
fn clamp_above_hi() {
    assert_eq!(eval_formula("clamp(5, 0, 3)"), 3.0);
}

#[test]
fn clamp_in_range() {
    assert_eq!(eval_formula("clamp(2, 0, 3)"), 2.0);
}

// ---------------------------------------------------------------------------
// Comparison operators
// ---------------------------------------------------------------------------

#[test]
fn gt_true() {
    assert_eq!(eval_formula("5 > 3"), 1.0);
}

#[test]
fn gt_false() {
    assert_eq!(eval_formula("3 > 5"), 0.0);
}

#[test]
fn lt_true() {
    assert_eq!(eval_formula("3 < 5"), 1.0);
}

#[test]
fn lt_false() {
    assert_eq!(eval_formula("5 < 3"), 0.0);
}

#[test]
fn ge_equal() {
    assert_eq!(eval_formula("3 >= 3"), 1.0);
}

#[test]
fn le_equal() {
    assert_eq!(eval_formula("3 <= 3"), 1.0);
}

#[test]
fn eq_equal() {
    assert_eq!(eval_formula("1 == 1"), 1.0);
}

#[test]
fn eq_not_equal() {
    assert_eq!(eval_formula("1 == 2"), 0.0);
}

#[test]
fn ne_true() {
    assert_eq!(eval_formula("1 != 2"), 1.0);
}

#[test]
fn ne_false() {
    assert_eq!(eval_formula("1 != 1"), 0.0);
}

// ---------------------------------------------------------------------------
// Boolean operators
// ---------------------------------------------------------------------------

#[test]
fn and_both_true() {
    assert_eq!(eval_formula("1 && 1"), 1.0);
}

#[test]
fn and_one_false() {
    assert_eq!(eval_formula("1 && 0"), 0.0);
}

#[test]
fn or_one_true() {
    assert_eq!(eval_formula("0 || 1"), 1.0);
}

#[test]
fn or_both_false() {
    assert_eq!(eval_formula("0 || 0"), 0.0);
}

#[test]
fn not_of_zero_is_one() {
    assert_eq!(eval_formula("!0"), 1.0);
}

#[test]
fn not_of_nonzero_is_zero() {
    assert_eq!(eval_formula("!1"), 0.0);
    assert_eq!(eval_formula("!42"), 0.0);
}

// ---------------------------------------------------------------------------
// Conditional
// ---------------------------------------------------------------------------

#[test]
fn if_true_branch() {
    assert_eq!(eval_formula("if 1 then 7 else 3"), 7.0);
}

#[test]
fn if_false_branch() {
    assert_eq!(eval_formula("if 0 then 7 else 3"), 3.0);
}

#[test]
fn if_with_comparison_condition() {
    assert_eq!(eval_formula("if 5 > 3 then 100 else 200"), 100.0);
    assert_eq!(eval_formula("if 3 > 5 then 100 else 200"), 200.0);
}

// ---------------------------------------------------------------------------
// NaN-safe edge cases (documented VM behavior, NOT IEEE-754 default)
// ---------------------------------------------------------------------------

#[test]
fn division_by_zero_returns_zero_not_nan() {
    // The VM guards: `if y != 0.0 { x/y } else { 0.0 }`.
    let v = eval_formula("6 / 0");
    assert_eq!(v, 0.0, "division by zero should be 0.0, not NaN or inf");
    assert!(!v.is_nan());
}

#[test]
fn zero_divided_by_zero_returns_zero_not_nan() {
    let v = eval_formula("0 / 0");
    assert_eq!(v, 0.0);
    assert!(!v.is_nan());
}

#[test]
fn sqrt_of_negative_returns_zero_not_nan() {
    // The VM guards: `if *t >= 0.0 { sqrt } else { 0.0 }`.
    let v = eval_formula("sqrt(-1)");
    assert_eq!(v, 0.0, "sqrt(-1) should be 0.0, not NaN");
    assert!(!v.is_nan());
}

#[test]
fn ln_of_zero_returns_zero_not_neg_inf() {
    // The VM guards: `if *t > 0.0 { ln } else { 0.0 }`.
    let v = eval_formula("ln(0)");
    assert_eq!(v, 0.0, "ln(0) should be 0.0, not -inf");
    assert!(v.is_finite());
}

#[test]
fn ln_of_negative_returns_zero_not_nan() {
    let v = eval_formula("ln(-1)");
    assert_eq!(v, 0.0, "ln(-1) should be 0.0, not NaN");
    assert!(!v.is_nan());
}

#[test]
fn modulo_by_zero_returns_zero() {
    // The VM guards: `if y != 0.0 { rem_euclid } else { 0.0 }`.
    let v = eval_formula("7 % 0");
    assert_eq!(v, 0.0);
    assert!(!v.is_nan());
}

// ---------------------------------------------------------------------------
// Parameter loading and live updates
// ---------------------------------------------------------------------------

#[test]
fn formula_reads_declared_param() {
    let sys = param_system_with(&[("gain", 2.0)]);
    let prog = Arc::new(sys.compile("gain").unwrap());
    let mut inst = FormulaInstance::new(prog, sys.table());
    assert_eq!(inst.eval(&[], &BuiltinValues::default()), 2.0);
}

#[test]
fn param_update_is_visible_immediately() {
    let sys = param_system_with(&[("gain", 1.0)]);
    let prog = Arc::new(sys.compile("gain * 1000").unwrap());
    let mut inst = FormulaInstance::new(prog, sys.table());
    let builtins = BuiltinValues::default();

    sys.set_param("gain", 1.5);
    assert_eq!(inst.eval(&[], &builtins), 1500.0);

    sys.set_param("gain", 2.0);
    assert_eq!(inst.eval(&[], &builtins), 2000.0);
}

#[test]
fn multiple_param_refs_in_one_formula() {
    let sys = param_system_with(&[("r1", 1000.0), ("r2", 2000.0)]);
    let prog = Arc::new(sys.compile("r1 + r2").unwrap());
    let mut inst = FormulaInstance::new(prog, sys.table());
    assert_eq!(inst.eval(&[], &BuiltinValues::default()), 3000.0);
}

// ---------------------------------------------------------------------------
// Builtin values
// ---------------------------------------------------------------------------

#[test]
fn formula_reads_sample_rate() {
    let builtins = audio_builtins();
    let result = eval_formula_with_builtins("sr", &builtins);
    assert_eq!(result, 96000.0);
}

#[test]
fn formula_reads_time_step() {
    let builtins = audio_builtins();
    let result = eval_formula_with_builtins("dt", &builtins);
    assert_near(result, 1.0 / 96000.0, 1e-12, "dt");
}

// ---------------------------------------------------------------------------
// Smooth operator
// ---------------------------------------------------------------------------

#[test]
fn smooth_starts_at_zero_and_converges_upward() {
    // With dt=1.0 and tau=0.001, alpha ≈ 0.999. After 100 steps the output
    // should be indistinguishable from the target (1.0) within 1e-6.
    let builtins = slow_builtins(); // dt = 1.0
    let last = eval_formula_n_times("smooth(1.0, 0.001)", 100, &builtins);
    assert_near(last, 1.0, 1e-6, "smooth convergence to 1.0");
}

#[test]
fn smooth_tracks_target_after_convergence() {
    let builtins = slow_builtins();
    // Converge toward 1.0 first.
    let sys = param_system_with(&[("target", 1.0)]);
    let prog = Arc::new(sys.compile("smooth(target, 0.001)").unwrap());
    let mut inst = FormulaInstance::new(prog, sys.table());

    for _ in 0..100 {
        inst.eval(&[], &builtins);
    }

    let near_one = inst.eval(&[], &builtins);
    assert_near(near_one, 1.0, 1e-6, "converged to 1.0");

    // Now steer toward 0.0. Smooth state starts near 1.0.
    sys.set_param("target", 0.0);
    for _ in 0..100 {
        inst.eval(&[], &builtins);
    }
    let near_zero = inst.eval(&[], &builtins);
    assert_near(near_zero, 0.0, 1e-6, "converged to 0.0");
}

// ---------------------------------------------------------------------------
// LFO
// ---------------------------------------------------------------------------

#[test]
fn lfo_sine_output_is_in_valid_range() {
    // Drive at 1 Hz. At t=0 and various other times the output is in [-1, 1].
    let mut builtins = audio_builtins();
    let sys = param_system_with(&[]);
    let prog = Arc::new(sys.compile("lfo_sine(1)").unwrap());
    let mut inst = FormulaInstance::new(prog, sys.table());

    for step in 0..1000 {
        builtins.simulation_time = step as f64 * builtins.time_step;
        let v = inst.eval(&[], &builtins);
        assert!(
            (-1.0..=1.0).contains(&v),
            "lfo_sine out of range at step {step}: {v}"
        );
        assert!(!v.is_nan(), "lfo_sine is NaN at step {step}");
    }
}

#[test]
fn lfo_output_is_finite_for_all_shapes() {
    let shapes = ["lfo_sine(1)", "lfo_tri(1)", "lfo_sqr(1)", "lfo_saw(1)"];
    let mut builtins = audio_builtins();
    for formula in &shapes {
        let last = {
            let sys = param_system_with(&[]);
            let prog = Arc::new(sys.compile(formula).unwrap());
            let mut inst = FormulaInstance::new(prog, sys.table());
            let mut v = 0.0;
            for step in 0..1000 {
                builtins.simulation_time = step as f64 * builtins.time_step;
                v = inst.eval(&[], &builtins);
                assert!(!v.is_nan(), "{formula} produced NaN at step {step}");
                assert!(v.is_finite(), "{formula} produced inf at step {step}");
            }
            v
        };
        let _ = last;
    }
}
