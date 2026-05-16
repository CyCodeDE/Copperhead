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

//! Tests for the formula language parser / compiler.
//!
//! These tests verify that:
//!   - valid inputs produce a Program without panicking,
//!   - the correct CompileError variant is returned for invalid inputs,
//!   - metadata (smooth_slots, stack_depth) is computed correctly.
//!
//! Evaluation correctness is tested separately in `parameter_vm`.

use copperhead_core::parameter::parser::{CompileError, SymbolTable, compile};

fn empty_syms() -> SymbolTable {
    SymbolTable::default()
}

/// Compile `src` and assert we get the expected `CompileError` variant.
/// The `check` closure returns true when the error is the right one.
fn assert_compile_error(src: &str, check: impl FnOnce(&CompileError) -> bool) {
    let result = compile(src, &empty_syms());
    match result {
        Err(ref e) if check(e) => {}
        Err(e) => panic!("wrong error for {:?}: {}", src, e),
        Ok(_) => panic!("expected a compile error for {:?} but it compiled", src),
    }
}

// ---------------------------------------------------------------------------
// Happy paths — literals
// ---------------------------------------------------------------------------

#[test]
fn integer_literal_compiles() {
    compile("42", &empty_syms()).unwrap();
}

#[test]
fn float_literal_compiles() {
    compile("3.14", &empty_syms()).unwrap();
}

#[test]
fn scientific_notation_compiles() {
    compile("1e3", &empty_syms()).unwrap();
    compile("1.5e-6", &empty_syms()).unwrap();
    compile("2.0E+4", &empty_syms()).unwrap();
}

#[test]
fn boolean_literals_compile() {
    compile("true", &empty_syms()).unwrap();
    compile("false", &empty_syms()).unwrap();
}

#[test]
fn math_constants_compile() {
    compile("pi", &empty_syms()).unwrap();
    compile("tau", &empty_syms()).unwrap();
    compile("e", &empty_syms()).unwrap();
}

// ---------------------------------------------------------------------------
// Happy paths — builtins (both short and long-form aliases)
// ---------------------------------------------------------------------------

#[test]
fn builtin_short_forms_compile() {
    for src in &["sr", "dt", "t", "step", "bpm", "ovs"] {
        compile(src, &empty_syms()).unwrap_or_else(|e| panic!("{:?} failed: {}", src, e));
    }
}

#[test]
fn builtin_long_forms_compile() {
    for src in &[
        "sample_rate",
        "time_step",
        "time",
        "step_index",
        "oversampling",
    ] {
        compile(src, &empty_syms()).unwrap_or_else(|e| panic!("{:?} failed: {}", src, e));
    }
}

// ---------------------------------------------------------------------------
// Happy paths — operators
// ---------------------------------------------------------------------------

#[test]
fn arithmetic_operators_compile() {
    for src in &["1 + 2", "5 - 3", "4 * 3", "8 / 2", "7 % 3", "2 ^ 10"] {
        compile(src, &empty_syms()).unwrap_or_else(|e| panic!("{:?} failed: {}", src, e));
    }
}

#[test]
fn unary_operators_compile() {
    compile("-1", &empty_syms()).unwrap();
    compile("+1", &empty_syms()).unwrap();
    compile("-pi", &empty_syms()).unwrap();
    compile("!0", &empty_syms()).unwrap();
    compile("not 1", &empty_syms()).unwrap();
}

#[test]
fn comparison_operators_compile() {
    for src in &["1 > 0", "1 < 0", "1 >= 1", "1 <= 2", "1 == 1", "1 != 2"] {
        compile(src, &empty_syms()).unwrap_or_else(|e| panic!("{:?} failed: {}", src, e));
    }
}

#[test]
fn logical_operators_compile() {
    for src in &["1 && 0", "1 || 0", "1 and 0", "1 or 0"] {
        compile(src, &empty_syms()).unwrap_or_else(|e| panic!("{:?} failed: {}", src, e));
    }
}

#[test]
fn if_then_else_compiles() {
    compile("if 1 then 2 else 3", &empty_syms()).unwrap();
    compile("if 0 > 1 then 10 else 20", &empty_syms()).unwrap();
}

#[test]
fn nested_if_then_else_compiles() {
    compile("if 1 then if 0 then 1 else 2 else 3", &empty_syms()).unwrap();
}

#[test]
fn parenthesized_expression_compiles() {
    compile("(1 + 2) * 3", &empty_syms()).unwrap();
}

// ---------------------------------------------------------------------------
// Happy paths — functions
// ---------------------------------------------------------------------------

#[test]
fn one_arg_functions_compile() {
    for func in &[
        "sin", "cos", "tan", "tanh", "abs", "sqrt", "exp", "ln", "floor", "ceil",
    ] {
        let src = format!("{}(1)", func);
        compile(&src, &empty_syms()).unwrap_or_else(|e| panic!("{} failed: {}", src, e));
    }
}

#[test]
fn two_arg_functions_compile() {
    for src in &["pow(2, 3)", "min(1, 2)", "max(1, 2)"] {
        compile(src, &empty_syms()).unwrap_or_else(|e| panic!("{:?} failed: {}", src, e));
    }
}

#[test]
fn clamp_compiles() {
    compile("clamp(5, 0, 3)", &empty_syms()).unwrap();
}

#[test]
fn lfo_functions_compile() {
    for src in &["lfo_sine(1)", "lfo_tri(1)", "lfo_sqr(1)", "lfo_saw(1)"] {
        compile(src, &empty_syms()).unwrap_or_else(|e| panic!("{:?} failed: {}", src, e));
    }
}

#[test]
fn smooth_compiles() {
    compile("smooth(1, 0.01)", &empty_syms()).unwrap();
}

#[test]
fn nested_function_calls_compile() {
    compile("abs(sin(pi))", &empty_syms()).unwrap();
    compile("clamp(sin(t), -0.5, 0.5)", &empty_syms()).unwrap();
}

// ---------------------------------------------------------------------------
// Happy paths — symbol table integration
// ---------------------------------------------------------------------------

#[test]
fn declared_param_resolves() {
    let mut syms = SymbolTable::default();
    syms.params.insert("gain".to_string(), 0);
    compile("gain * 1000", &syms).unwrap();
}

#[test]
fn declared_voltage_resolves() {
    let mut syms = SymbolTable::default();
    syms.voltages.insert("vout".to_string(), 0);
    compile("vout * 2", &syms).unwrap();
}

#[test]
fn declared_enum_resolves() {
    let mut syms = SymbolTable::default();
    syms.enums.insert("mode".to_string(), 0);
    compile("mode", &syms).unwrap();
}

#[test]
fn symbol_table_merge_other_shadows_self() {
    let mut a = SymbolTable::default();
    a.params.insert("x".to_string(), 0);

    let mut b = SymbolTable::default();
    b.params.insert("x".to_string(), 1); // b's index shadows a's
    b.params.insert("y".to_string(), 2);

    let merged = a.merged(&b);
    assert_eq!(merged.params["x"], 1, "b should shadow a for 'x'");
    assert_eq!(merged.params["y"], 2);
    // a's own 'x' is gone, but that's the contract
}

#[test]
fn symbol_table_merge_keeps_self_entries_absent_from_other() {
    let mut a = SymbolTable::default();
    a.params.insert("a_only".to_string(), 0);

    let b = SymbolTable::default();
    let merged = a.merged(&b);
    assert_eq!(merged.params["a_only"], 0);
}

// ---------------------------------------------------------------------------
// Program metadata
// ---------------------------------------------------------------------------

#[test]
fn smooth_allocates_one_slot_per_call() {
    let one = compile("smooth(1, 0.01)", &empty_syms()).unwrap();
    assert_eq!(one.smooth_slots, 1);

    let two = compile("smooth(1, 0.01) + smooth(2, 0.1)", &empty_syms()).unwrap();
    assert_eq!(two.smooth_slots, 2);
}

#[test]
fn constant_has_nonzero_stack_depth() {
    let prog = compile("42", &empty_syms()).unwrap();
    assert!(prog.stack_depth >= 1);
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

#[test]
fn empty_input_is_eof_error() {
    assert_compile_error("", |e| matches!(e, CompileError::UnexpectedEof));
}

#[test]
fn at_sign_is_unexpected_char() {
    assert_compile_error("@", |e| {
        matches!(e, CompileError::UnexpectedChar { ch: '@', .. })
    });
}

#[test]
fn single_equals_is_unexpected_char() {
    // `=` alone is invalid; `==` is equality
    assert_compile_error("1 = 2", |e| {
        matches!(e, CompileError::UnexpectedChar { ch: '=', .. })
    });
}

#[test]
fn single_ampersand_is_unexpected_char() {
    assert_compile_error("1 & 2", |e| {
        matches!(e, CompileError::UnexpectedChar { ch: '&', .. })
    });
}

#[test]
fn single_pipe_is_unexpected_char() {
    assert_compile_error("1 | 2", |e| {
        matches!(e, CompileError::UnexpectedChar { ch: '|', .. })
    });
}

#[test]
fn undeclared_identifier_is_unknown_ident() {
    assert_compile_error("foobar", |e| {
        matches!(e, CompileError::UnknownIdent(s) if s == "foobar")
    });
}

#[test]
fn unknown_function_name_error() {
    assert_compile_error("totally_unknown_fn(1)", |e| {
        matches!(e, CompileError::UnknownFunction(s) if s == "totally_unknown_fn")
    });
}

#[test]
fn sin_with_zero_args_is_wrong_arity() {
    assert_compile_error("sin()", |e| {
        matches!(e, CompileError::WrongArity { name, expected: 1, got: 0 } if name == "sin")
    });
}

#[test]
fn sin_with_two_args_is_wrong_arity() {
    assert_compile_error("sin(1, 2)", |e| {
        matches!(e, CompileError::WrongArity { name, expected: 1, got: 2 } if name == "sin")
    });
}

#[test]
fn clamp_with_one_arg_is_wrong_arity() {
    assert_compile_error("clamp(1)", |e| {
        matches!(e, CompileError::WrongArity { name, expected: 3, got: 1 } if name == "clamp")
    });
}

#[test]
fn min_with_one_arg_is_wrong_arity() {
    assert_compile_error("min(1)", |e| {
        matches!(e, CompileError::WrongArity { name, expected: 2, got: 1 } if name == "min")
    });
}

#[test]
fn max_with_three_args_is_wrong_arity() {
    assert_compile_error("max(1, 2, 3)", |e| {
        matches!(e, CompileError::WrongArity { name, expected: 2, got: 3 } if name == "max")
    });
}

#[test]
fn smooth_with_one_arg_is_wrong_arity() {
    assert_compile_error("smooth(1)", |e| {
        matches!(e, CompileError::WrongArity { name, expected: 2, got: 1 } if name == "smooth")
    });
}

#[test]
fn incomplete_binary_op_is_eof() {
    assert_compile_error("1 +", |e| matches!(e, CompileError::UnexpectedEof));
    assert_compile_error("1 *", |e| matches!(e, CompileError::UnexpectedEof));
}

#[test]
fn unclosed_paren_is_unexpected_token() {
    // After "1 + 2", the current token is EOF, expect(RParen) fires UnexpectedToken.
    assert_compile_error("(1 + 2", |e| matches!(e, CompileError::UnexpectedToken { .. }));
}

#[test]
fn extra_closing_paren_is_unexpected_token() {
    // After parsing "1 + 2", expect_eof() fires UnexpectedToken because RParen is next.
    assert_compile_error("1 + 2)", |e| matches!(e, CompileError::UnexpectedToken { .. }));
}

#[test]
fn if_without_then_is_unexpected_token() {
    assert_compile_error("if 1 else 2", |e| {
        matches!(e, CompileError::UnexpectedToken { .. })
    });
}

#[test]
fn if_without_else_is_unexpected_token() {
    // After "if 1 then 2", expect(Else) sees EOF → UnexpectedToken.
    assert_compile_error("if 1 then 2", |e| {
        matches!(e, CompileError::UnexpectedToken { .. })
    });
}

#[test]
fn param_not_in_symbol_table_is_unknown_ident() {
    // "gain" is not declared in empty_syms(), so it should fail.
    assert_compile_error("gain * 1000", |e| {
        matches!(e, CompileError::UnknownIdent(s) if s == "gain")
    });
}
