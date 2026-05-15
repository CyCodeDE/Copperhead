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

//! Lexer, parser and bytecode compiler for the formula language.
//!
//! Grammar (C-like expression syntax):
//!
//! ```text
//! expr      := if_expr | logic_or
//! if_expr   := 'if' expr 'then' expr 'else' expr
//! logic_or  := logic_and  (('||' | 'or')  logic_and)*
//! logic_and := equality   (('&&' | 'and') equality)*
//! equality  := compare    (('==' | '!=') compare)*
//! compare   := add        (('<' | '>' | '<=' | '>=') add)*
//! add       := mul        (('+' | '-')  mul)*
//! mul       := unary      (('*' | '/' | '%') unary)*
//! unary     := ('-' | '+' | '!' | 'not') unary | power
//! power     := primary ('^' unary)?
//! primary   := number | ident (func_call)? | '(' expr ')'
//! ```
//!
//! Identifiers resolve in this order: builtins (`sr`, `dt`, `t`, `step`,
//! `bpm`, `ovs`), then `SymbolTable::params`, `enums`, `voltages`, then
//! math constants (`pi`, `tau`, `e`). Functions are recognised by name at
//! call sites (`sin`, `cos`, `pow`, `clamp`, `smooth`, `lfo_sine`, ...).
//!
//! Booleans: `true`/`false` lex as `1.0`/`0.0`. Comparisons and boolean ops
//! return `0.0` or `1.0`. `and`/`or`/`not` are evaluated branch-free (NOT
//! short-circuited), which matches the audio-thread "never branch on data"
//! constraint.

use std::collections::HashMap;

use super::{Builtin, LfoShape, Op, Program, STACK_CAP};

#[derive(Debug, Clone)]
pub enum CompileError {
    UnexpectedChar {
        ch: char,
        pos: usize,
    },
    UnexpectedToken {
        found: String,
        pos: usize,
    },
    UnexpectedEof,
    UnknownIdent(String),
    UnknownFunction(String),
    WrongArity {
        name: String,
        expected: usize,
        got: usize,
    },
    TooManySmoothSlots,
    StackOverflow {
        depth: i32,
    },
    NumberParse(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedChar { ch, pos } => {
                write!(f, "unexpected character '{}' at byte {}", ch, pos)
            }
            Self::UnexpectedToken { found, pos } => {
                write!(f, "unexpected token `{}` at byte {}", found, pos)
            }
            Self::UnexpectedEof => write!(f, "unexpected end of input"),
            Self::UnknownIdent(s) => write!(f, "unknown identifier `{}`", s),
            Self::UnknownFunction(s) => write!(f, "unknown function `{}`", s),
            Self::WrongArity {
                name,
                expected,
                got,
            } => {
                write!(f, "`{}` expects {} args, got {}", name, expected, got)
            }
            Self::TooManySmoothSlots => write!(f, "too many smooth() calls (max 256)"),
            Self::StackOverflow { depth } => {
                write!(
                    f,
                    "formula exceeds VM stack depth ({} > {})",
                    depth, STACK_CAP
                )
            }
            Self::NumberParse(s) => write!(f, "invalid number literal `{}`", s),
        }
    }
}

impl std::error::Error for CompileError {}

/// Name → index mappings provided by the host at compile time. The indices
/// are what the VM uses at runtime to address the corresponding slice inside
/// `EvalContext`.
#[derive(Default, Clone, Debug)]
pub struct SymbolTable {
    pub params: HashMap<String, u16>,
    pub enums: HashMap<String, u16>,
    pub voltages: HashMap<String, u16>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Merge two symbol tables. Entries in `other` override entries in `self`.
    /// Useful for composing a component-local table on top of a global one.
    pub fn merged(&self, other: &SymbolTable) -> SymbolTable {
        let mut out = self.clone();
        for (k, v) in &other.params {
            out.params.insert(k.clone(), *v);
        }
        for (k, v) in &other.enums {
            out.enums.insert(k.clone(), *v);
        }
        for (k, v) in &other.voltages {
            out.voltages.insert(k.clone(), *v);
        }
        out
    }
}

/// Entry point: parse `src` and lower it to a `Program`.
pub fn compile(src: &str, syms: &SymbolTable) -> Result<Program, CompileError> {
    let mut c = Compiler::new(src, syms)?;
    c.parse_expr()?;
    c.expect_eof()?;
    if c.max_stack as usize > STACK_CAP {
        return Err(CompileError::StackOverflow { depth: c.max_stack });
    }
    Ok(Program {
        code: c.code,
        smooth_slots: c.smooth_slots,
        stack_depth: c.max_stack as u16,
    })
}

// ---------------------------------------------------------------------------
// Lexer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
enum Tok {
    Num(f64),
    Ident(String),
    LParen,
    RParen,
    Comma,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    Not,
    If,
    Then,
    Else,
    Eof,
}

impl Tok {
    fn label(&self) -> String {
        format!("{:?}", self)
    }
}

struct Lexer<'a> {
    src: &'a [u8],
    pos: usize,
    tok_start: usize,
}

impl<'a> Lexer<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            src: s.as_bytes(),
            pos: 0,
            tok_start: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        self.src.get(self.pos).copied()
    }
    fn peek2(&self) -> Option<u8> {
        self.src.get(self.pos + 1).copied()
    }

    fn next_tok(&mut self) -> Result<Tok, CompileError> {
        // Skip whitespace.
        while let Some(c) = self.peek() {
            if c.is_ascii_whitespace() {
                self.pos += 1;
            } else {
                break;
            }
        }

        self.tok_start = self.pos;
        let c = match self.peek() {
            None => return Ok(Tok::Eof),
            Some(c) => c,
        };

        // Number literal.
        if c.is_ascii_digit() || (c == b'.' && self.peek2().map_or(false, |d| d.is_ascii_digit())) {
            let start = self.pos;
            let mut saw_dot = false;
            let mut saw_exp = false;
            while let Some(d) = self.src.get(self.pos).copied() {
                match d {
                    b'0'..=b'9' => self.pos += 1,
                    b'.' if !saw_dot && !saw_exp => {
                        saw_dot = true;
                        self.pos += 1;
                    }
                    b'e' | b'E' if !saw_exp => {
                        saw_exp = true;
                        self.pos += 1;
                        if matches!(self.peek(), Some(b'+') | Some(b'-')) {
                            self.pos += 1;
                        }
                    }
                    _ => break,
                }
            }
            let s = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
            let v: f64 = s
                .parse()
                .map_err(|_| CompileError::NumberParse(s.to_string()))?;
            return Ok(Tok::Num(v));
        }

        // Identifier / keyword.
        if c.is_ascii_alphabetic() || c == b'_' {
            let start = self.pos;
            while let Some(d) = self.src.get(self.pos).copied() {
                if d.is_ascii_alphanumeric() || d == b'_' {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            let s = std::str::from_utf8(&self.src[start..self.pos]).unwrap();
            return Ok(match s {
                "if" => Tok::If,
                "then" => Tok::Then,
                "else" => Tok::Else,
                "and" => Tok::And,
                "or" => Tok::Or,
                "not" => Tok::Not,
                "true" => Tok::Num(1.0),
                "false" => Tok::Num(0.0),
                _ => Tok::Ident(s.to_string()),
            });
        }

        // Punctuation / operators.
        let start = self.pos;
        self.pos += 1;
        Ok(match c {
            b'(' => Tok::LParen,
            b')' => Tok::RParen,
            b',' => Tok::Comma,
            b'+' => Tok::Plus,
            b'-' => Tok::Minus,
            b'*' => Tok::Star,
            b'/' => Tok::Slash,
            b'%' => Tok::Percent,
            b'^' => Tok::Caret,
            b'=' => {
                if self.peek() == Some(b'=') {
                    self.pos += 1;
                    Tok::Eq
                } else {
                    return Err(CompileError::UnexpectedChar {
                        ch: '=',
                        pos: start,
                    });
                }
            }
            b'!' => {
                if self.peek() == Some(b'=') {
                    self.pos += 1;
                    Tok::Ne
                } else {
                    Tok::Not
                }
            }
            b'<' => {
                if self.peek() == Some(b'=') {
                    self.pos += 1;
                    Tok::Le
                } else {
                    Tok::Lt
                }
            }
            b'>' => {
                if self.peek() == Some(b'=') {
                    self.pos += 1;
                    Tok::Ge
                } else {
                    Tok::Gt
                }
            }
            b'&' => {
                if self.peek() == Some(b'&') {
                    self.pos += 1;
                    Tok::And
                } else {
                    return Err(CompileError::UnexpectedChar {
                        ch: '&',
                        pos: start,
                    });
                }
            }
            b'|' => {
                if self.peek() == Some(b'|') {
                    self.pos += 1;
                    Tok::Or
                } else {
                    return Err(CompileError::UnexpectedChar {
                        ch: '|',
                        pos: start,
                    });
                }
            }
            _ => {
                return Err(CompileError::UnexpectedChar {
                    ch: c as char,
                    pos: start,
                });
            }
        })
    }
}

// ---------------------------------------------------------------------------
// Single-pass parser + code generator
// ---------------------------------------------------------------------------

struct Compiler<'a> {
    lex: Lexer<'a>,
    cur: Tok,
    cur_pos: usize,
    syms: &'a SymbolTable,

    code: Vec<Op>,
    stack: i32,
    max_stack: i32,
    smooth_slots: u16,
}

impl<'a> Compiler<'a> {
    fn new(src: &'a str, syms: &'a SymbolTable) -> Result<Self, CompileError> {
        let mut lex = Lexer::new(src);
        let cur = lex.next_tok()?;
        let cur_pos = lex.tok_start;
        Ok(Self {
            lex,
            cur,
            cur_pos,
            syms,
            code: Vec::new(),
            stack: 0,
            max_stack: 0,
            smooth_slots: 0,
        })
    }

    fn advance(&mut self) -> Result<(), CompileError> {
        self.cur = self.lex.next_tok()?;
        self.cur_pos = self.lex.tok_start;
        Ok(())
    }

    fn expect(&mut self, expected: Tok) -> Result<(), CompileError> {
        if std::mem::discriminant(&self.cur) == std::mem::discriminant(&expected) {
            self.advance()
        } else {
            Err(CompileError::UnexpectedToken {
                found: self.cur.label(),
                pos: self.cur_pos,
            })
        }
    }

    fn expect_eof(&self) -> Result<(), CompileError> {
        if matches!(self.cur, Tok::Eof) {
            Ok(())
        } else {
            Err(CompileError::UnexpectedToken {
                found: self.cur.label(),
                pos: self.cur_pos,
            })
        }
    }

    fn emit(&mut self, op: Op, delta: i32) {
        self.code.push(op);
        self.stack += delta;
        if self.stack > self.max_stack {
            self.max_stack = self.stack;
        }
    }

    // ---- grammar -----------------------------------------------------------

    fn parse_expr(&mut self) -> Result<(), CompileError> {
        if matches!(self.cur, Tok::If) {
            self.advance()?;
            self.parse_or()?; // condition
            self.expect(Tok::Then)?;
            self.parse_expr()?; // then-branch (allows nested if)
            self.expect(Tok::Else)?;
            self.parse_expr()?; // else-branch
            self.emit(Op::Select, -2);
            return Ok(());
        }
        self.parse_or()
    }

    fn parse_or(&mut self) -> Result<(), CompileError> {
        self.parse_and()?;
        while matches!(self.cur, Tok::Or) {
            self.advance()?;
            self.parse_and()?;
            self.emit(Op::Or, -1);
        }
        Ok(())
    }

    fn parse_and(&mut self) -> Result<(), CompileError> {
        self.parse_eq()?;
        while matches!(self.cur, Tok::And) {
            self.advance()?;
            self.parse_eq()?;
            self.emit(Op::And, -1);
        }
        Ok(())
    }

    fn parse_eq(&mut self) -> Result<(), CompileError> {
        self.parse_cmp()?;
        loop {
            let op = match self.cur {
                Tok::Eq => Op::Eq,
                Tok::Ne => Op::Ne,
                _ => return Ok(()),
            };
            self.advance()?;
            self.parse_cmp()?;
            self.emit(op, -1);
        }
    }

    fn parse_cmp(&mut self) -> Result<(), CompileError> {
        self.parse_add()?;
        loop {
            let op = match self.cur {
                Tok::Lt => Op::Lt,
                Tok::Gt => Op::Gt,
                Tok::Le => Op::Le,
                Tok::Ge => Op::Ge,
                _ => return Ok(()),
            };
            self.advance()?;
            self.parse_add()?;
            self.emit(op, -1);
        }
    }

    fn parse_add(&mut self) -> Result<(), CompileError> {
        self.parse_mul()?;
        loop {
            let op = match self.cur {
                Tok::Plus => Op::Add,
                Tok::Minus => Op::Sub,
                _ => return Ok(()),
            };
            self.advance()?;
            self.parse_mul()?;
            self.emit(op, -1);
        }
    }

    fn parse_mul(&mut self) -> Result<(), CompileError> {
        self.parse_unary()?;
        loop {
            let op = match self.cur {
                Tok::Star => Op::Mul,
                Tok::Slash => Op::Div,
                Tok::Percent => Op::Mod,
                _ => return Ok(()),
            };
            self.advance()?;
            self.parse_unary()?;
            self.emit(op, -1);
        }
    }

    fn parse_unary(&mut self) -> Result<(), CompileError> {
        match self.cur {
            Tok::Minus => {
                self.advance()?;
                self.parse_unary()?;
                self.emit(Op::Neg, 0);
                Ok(())
            }
            Tok::Plus => {
                self.advance()?;
                self.parse_unary()
            }
            Tok::Not => {
                self.advance()?;
                self.parse_unary()?;
                self.emit(Op::Not, 0);
                Ok(())
            }
            _ => self.parse_pow(),
        }
    }

    fn parse_pow(&mut self) -> Result<(), CompileError> {
        self.parse_primary()?;
        if matches!(self.cur, Tok::Caret) {
            self.advance()?;
            self.parse_unary()?; // right-associative
            self.emit(Op::Pow, -1);
        }
        Ok(())
    }

    fn parse_primary(&mut self) -> Result<(), CompileError> {
        let tok = self.cur.clone();
        self.advance()?;
        match tok {
            Tok::Num(n) => {
                self.emit(Op::Const(n), 1);
                Ok(())
            }
            Tok::LParen => {
                self.parse_expr()?;
                self.expect(Tok::RParen)
            }
            Tok::Ident(name) => {
                if matches!(self.cur, Tok::LParen) {
                    self.advance()?;
                    let mut args = 0usize;
                    if !matches!(self.cur, Tok::RParen) {
                        self.parse_expr()?;
                        args += 1;
                        while matches!(self.cur, Tok::Comma) {
                            self.advance()?;
                            self.parse_expr()?;
                            args += 1;
                        }
                    }
                    self.expect(Tok::RParen)?;
                    self.emit_call(&name, args)
                } else {
                    self.emit_ident(&name)
                }
            }
            Tok::Eof => Err(CompileError::UnexpectedEof),
            other => Err(CompileError::UnexpectedToken {
                found: other.label(),
                pos: self.cur_pos,
            }),
        }
    }

    // ---- identifier / call resolution --------------------------------------

    fn emit_ident(&mut self, name: &str) -> Result<(), CompileError> {
        // Builtins first (fast, short names).
        if let Some(b) = match_builtin(name) {
            self.emit(Op::LoadBuiltin(b), 1);
            return Ok(());
        }
        if let Some(&i) = self.syms.params.get(name) {
            self.emit(Op::LoadParam(i), 1);
            return Ok(());
        }
        if let Some(&i) = self.syms.enums.get(name) {
            self.emit(Op::LoadEnum(i), 1);
            return Ok(());
        }
        if let Some(&i) = self.syms.voltages.get(name) {
            self.emit(Op::LoadVoltage(i), 1);
            return Ok(());
        }
        let k = match name {
            "pi" => std::f64::consts::PI,
            "tau" => std::f64::consts::TAU,
            "e" => std::f64::consts::E,
            _ => return Err(CompileError::UnknownIdent(name.to_string())),
        };
        self.emit(Op::Const(k), 1);
        Ok(())
    }

    fn emit_call(&mut self, name: &str, args: usize) -> Result<(), CompileError> {
        let arity = |expected: usize| {
            if args != expected {
                Err(CompileError::WrongArity {
                    name: name.to_string(),
                    expected,
                    got: args,
                })
            } else {
                Ok(())
            }
        };

        match name {
            // 1-arg math
            "sin" => {
                arity(1)?;
                self.emit(Op::Sin, 0);
            }
            "cos" => {
                arity(1)?;
                self.emit(Op::Cos, 0);
            }
            "tan" => {
                arity(1)?;
                self.emit(Op::Tan, 0);
            }
            "tanh" => {
                arity(1)?;
                self.emit(Op::Tanh, 0);
            }
            "abs" => {
                arity(1)?;
                self.emit(Op::Abs, 0);
            }
            "sqrt" => {
                arity(1)?;
                self.emit(Op::Sqrt, 0);
            }
            "exp" => {
                arity(1)?;
                self.emit(Op::Exp, 0);
            }
            "ln" => {
                arity(1)?;
                self.emit(Op::Ln, 0);
            }
            "floor" => {
                arity(1)?;
                self.emit(Op::Floor, 0);
            }
            "ceil" => {
                arity(1)?;
                self.emit(Op::Ceil, 0);
            }

            // 2-arg math
            "pow" => {
                arity(2)?;
                self.emit(Op::Pow, -1);
            }
            "min" => {
                arity(2)?;
                self.emit(Op::Min, -1);
            }
            "max" => {
                arity(2)?;
                self.emit(Op::Max, -1);
            }

            // 3-arg
            "clamp" => {
                arity(3)?;
                self.emit(Op::Clamp, -2);
            }

            // LFOs. Shape is picked by name to avoid needing string-arg tokens.
            "lfo_sine" => {
                arity(1)?;
                self.emit(Op::Lfo(LfoShape::Sine), 0);
            }
            "lfo_tri" => {
                arity(1)?;
                self.emit(Op::Lfo(LfoShape::Triangle), 0);
            }
            "lfo_sqr" => {
                arity(1)?;
                self.emit(Op::Lfo(LfoShape::Square), 0);
            }
            "lfo_saw" => {
                arity(1)?;
                self.emit(Op::Lfo(LfoShape::Saw), 0);
            }

            // Smoothing. Allocates a fresh per-formula slot.
            "smooth" => {
                arity(2)?;
                if self.smooth_slots >= u8::MAX as u16 {
                    return Err(CompileError::TooManySmoothSlots);
                }
                let slot = self.smooth_slots as u8;
                self.smooth_slots += 1;
                self.emit(Op::Smooth(slot), -1);
            }

            _ => return Err(CompileError::UnknownFunction(name.to_string())),
        }
        Ok(())
    }
}

fn match_builtin(name: &str) -> Option<Builtin> {
    Some(match name {
        "sr" | "sample_rate" => Builtin::SampleRate,
        "dt" | "time_step" => Builtin::TimeStep,
        "t" | "time" => Builtin::SimulationTime,
        "step" | "step_index" => Builtin::StepIndex,
        "bpm" => Builtin::Bpm,
        "ovs" | "oversampling" => Builtin::OversamplingFactor,
        _ => return None,
    })
}
