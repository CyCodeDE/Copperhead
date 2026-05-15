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
use atomic_float::AtomicF64;
use portable_atomic::AtomicU64;
use serde::{Deserialize, Serialize};
use std::sync::atomic::Ordering;

pub mod compile;

pub use compile::{compile, CompileError, SymbolTable};

/// Bytecode operation. The VM is a stack machine over f64.
///
/// Invariants (enforced by the compiler):
/// - All LoadParam/LoadVoltage/LoadEnum indices are in-bounds against the
///   `EvalContext` slices handed to `Program::eval`.
/// - All Smooth(slot) indices are < `Program::smooth_slots`.
/// - The maximum stack depth reached is < `STACK_CAP` (64).
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum Op {
    Const(f64),
    LoadParam(u16),
    LoadVoltage(u16),
    LoadBuiltin(Builtin),
    LoadEnum(u16),
    Add, Sub, Mul, Div, Neg, Mod,
    Sin, Cos, Tan, Tanh, Abs, Sqrt, Exp, Ln, Pow, Floor, Ceil, Min, Max, Clamp,
    Gt, Lt, Ge, Le, Eq, Ne,
    And, Or, Not,
    /// Ternary select. Stack before: [cond, then_val, else_val]. Branchless.
    Select,
    /// LFO of the given shape. Pops frequency (Hz), pushes value in [-1,1].
    /// Phase is derived from `builtins.simulation_time`.
    Lfo(LfoShape),
    /// One-pole lowpass smoothing with per-formula state slot.
    /// Pops (x, tau). Uses `builtins.time_step`. Updates `smooth_state[slot]`
    /// and pushes the new smoothed value.
    Smooth(u8),
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum LfoShape {
    Sine,
    Triangle,
    Square,
    Saw,
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub enum Builtin {
    SampleRate,
    TimeStep,
    SimulationTime,
    StepIndex,
    Bpm,
    OversamplingFactor,
}

/// Per-step scalar inputs to the VM. Pass by reference each eval call.
#[derive(Clone, Copy, Debug, Default)]
pub struct BuiltinValues {
    pub sample_rate: f64,
    pub time_step: f64,
    pub simulation_time: f64,
    pub step_index: f64,
    pub bpm: f64,
    pub oversampling_factor: f64,
}

/// Transient state passed to `Program::eval`.
///
/// The slices are expected to be laid out by the host once and reused:
/// - `params` / `enums` are views into an `Arc<ParamTable>` shared with the UI.
///   Writes happen lock-free via atomics on the UI side; this side reads with
///   `Ordering::Relaxed`.
/// - `voltages` is a snapshot of node voltages maintained by the solver.
/// - `smooth_state` is a per-formula-instance buffer of length
///   `Program::smooth_slots`.
pub struct EvalContext<'a> {
    pub params: &'a [AtomicF64],
    pub enums: &'a [AtomicU64],
    pub voltages: &'a [f64],
    pub builtins: &'a BuiltinValues,
    pub smooth_state: &'a mut [f64],
}

/// A compiled formula.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Program {
    pub code: Vec<Op>,
    /// Number of one-pole smoothing slots the host must allocate (`vec![0.0; n]`)
    /// for each evaluating instance of this program.
    pub smooth_slots: u16,
    /// Maximum stack depth observed during compilation. Always <= STACK_CAP.
    pub stack_depth: u16,
}

/// VM stack capacity. Formulas this deep will fail at compile time.
pub const STACK_CAP: usize = 64;

impl Program {
    /// Returns true if any opcode in this program reads a node voltage
    /// (`Op::LoadVoltage`). Used by the host to decide whether a component
    /// holding this formula must promote to `NonLinear` (value depends on
    /// the solution being iterated) or only `TimeVariant` (value fixed
    /// within one Newton iteration).
    pub fn depends_on_voltage(&self) -> bool {
        self.code
            .iter()
            .any(|op| matches!(op, Op::LoadVoltage(_)))
    }

    /// Returns true if any opcode reads a builtin scalar (sample rate, dt,
    /// simulation time, etc.). Builtin-dependent formulas produce different
    /// values across samples even without user interaction, so they cannot
    /// be frozen.
    ///
    /// Note: `Op::Lfo` and `Op::Smooth` also implicitly use builtins but are
    /// caught here because `Lfo` consumes `simulation_time` and `Smooth`
    /// consumes `time_step`.
    pub fn depends_on_builtins(&self) -> bool {
        self.code
            .iter()
            .any(|op| matches!(op, Op::LoadBuiltin(_) | Op::Lfo(_) | Op::Smooth(_)))
    }

    /// Returns true if this formula can be safely frozen: its only dynamic
    /// inputs are user-controllable parameters (`LoadParam` / `LoadEnum`).
    /// A frozen formula evaluates to a constant once all user parameters are
    /// snapshotted, so the component can be promoted to `LinearStatic`.
    pub fn is_freeze_eligible(&self) -> bool {
        !self.depends_on_voltage() && !self.depends_on_builtins()
    }

    /// Evaluate the program. Hot path — called per simulation step, possibly
    /// thousands of times per audio buffer.
    ///
    /// All indices inside `Op` are validated at compile time against the
    /// `SymbolTable` used during `compile`. The host MUST pass an
    /// `EvalContext` whose `params`, `enums`, `voltages`, and `smooth_state`
    /// lengths match or exceed what the compiler was told. Violating this is
    /// the caller's bug.
    #[inline]
    pub fn eval(&self, ctx: &mut EvalContext) -> f64 {
        let mut stack = [0.0f64; STACK_CAP];
        let mut sp: usize = 0;

        let params = ctx.params;
        let enums = ctx.enums;
        let voltages = ctx.voltages;
        let b = ctx.builtins;
        let smooth = &mut *ctx.smooth_state;

        // All of these macros assume the compiler-enforced stack depth bound.
        macro_rules! push {
            ($v:expr) => {{
                unsafe { *stack.get_unchecked_mut(sp) = $v };
                sp += 1;
            }};
        }
        macro_rules! pop1 {
            () => {{
                sp -= 1;
                unsafe { *stack.get_unchecked(sp) }
            }};
        }
        macro_rules! top {
            () => {
                unsafe { stack.get_unchecked_mut(sp - 1) }
            };
        }

        for op in self.code.iter() {
            match *op {
                Op::Const(c) => push!(c),

                Op::LoadParam(i) => {
                    let v = unsafe { params.get_unchecked(i as usize) }
                        .load(Ordering::Relaxed);
                    push!(v);
                }
                Op::LoadVoltage(i) => {
                    let v = unsafe { *voltages.get_unchecked(i as usize) };
                    push!(v);
                }
                Op::LoadEnum(i) => {
                    let v = unsafe { enums.get_unchecked(i as usize) }
                        .load(Ordering::Relaxed);
                    push!(v as f64);
                }
                Op::LoadBuiltin(bi) => {
                    let v = match bi {
                        Builtin::SampleRate => b.sample_rate,
                        Builtin::TimeStep => b.time_step,
                        Builtin::SimulationTime => b.simulation_time,
                        Builtin::StepIndex => b.step_index,
                        Builtin::Bpm => b.bpm,
                        Builtin::OversamplingFactor => b.oversampling_factor,
                    };
                    push!(v);
                }

                Op::Add => { let y = pop1!(); *top!() += y; }
                Op::Sub => { let y = pop1!(); *top!() -= y; }
                Op::Mul => { let y = pop1!(); *top!() *= y; }
                Op::Div => {
                    let y = pop1!();
                    let t = top!();
                    *t = if y != 0.0 { *t / y } else { 0.0 };
                }
                Op::Neg => { let t = top!(); *t = -*t; }
                Op::Mod => {
                    let y = pop1!();
                    let t = top!();
                    *t = if y != 0.0 { t.rem_euclid(y) } else { 0.0 };
                }

                Op::Sin  => { let t = top!(); *t = t.sin(); }
                Op::Cos  => { let t = top!(); *t = t.cos(); }
                Op::Tan  => { let t = top!(); *t = t.tan(); }
                Op::Tanh => { let t = top!(); *t = t.tanh(); }
                Op::Abs  => { let t = top!(); *t = t.abs(); }
                Op::Sqrt => {
                    let t = top!();
                    *t = if *t >= 0.0 { t.sqrt() } else { 0.0 };
                }
                Op::Exp  => { let t = top!(); *t = t.exp(); }
                Op::Ln   => {
                    let t = top!();
                    *t = if *t > 0.0 { t.ln() } else { 0.0 };
                }
                Op::Pow  => { let y = pop1!(); let t = top!(); *t = t.powf(y); }
                Op::Floor => { let t = top!(); *t = t.floor(); }
                Op::Ceil  => { let t = top!(); *t = t.ceil(); }
                Op::Min => { let y = pop1!(); let t = top!(); *t = t.min(y); }
                Op::Max => { let y = pop1!(); let t = top!(); *t = t.max(y); }
                Op::Clamp => {
                    let hi = pop1!();
                    let lo = pop1!();
                    let t = top!();
                    *t = t.clamp(lo, hi);
                }

                Op::Gt => { let y = pop1!(); let t = top!(); *t = (*t >  y) as i32 as f64; }
                Op::Lt => { let y = pop1!(); let t = top!(); *t = (*t <  y) as i32 as f64; }
                Op::Ge => { let y = pop1!(); let t = top!(); *t = (*t >= y) as i32 as f64; }
                Op::Le => { let y = pop1!(); let t = top!(); *t = (*t <= y) as i32 as f64; }
                Op::Eq => { let y = pop1!(); let t = top!(); *t = (*t == y) as i32 as f64; }
                Op::Ne => { let y = pop1!(); let t = top!(); *t = (*t != y) as i32 as f64; }
                Op::And => {
                    let y = pop1!();
                    let t = top!();
                    *t = ((*t != 0.0) & (y != 0.0)) as i32 as f64;
                }
                Op::Or => {
                    let y = pop1!();
                    let t = top!();
                    *t = ((*t != 0.0) | (y != 0.0)) as i32 as f64;
                }
                Op::Not => { let t = top!(); *t = (*t == 0.0) as i32 as f64; }

                Op::Select => {
                    // stack: cond, then, else  (top is else)
                    let else_v = pop1!();
                    let then_v = pop1!();
                    let t = top!();
                    *t = if *t != 0.0 { then_v } else { else_v };
                }

                Op::Lfo(shape) => {
                    let f = pop1!();
                    let phase = (f * b.simulation_time).rem_euclid(1.0);
                    let v = match shape {
                        LfoShape::Sine => (phase * std::f64::consts::TAU).sin(),
                        LfoShape::Triangle => {
                            if phase < 0.5 { 4.0 * phase - 1.0 } else { 3.0 - 4.0 * phase }
                        }
                        LfoShape::Square => if phase < 0.5 { 1.0 } else { -1.0 },
                        LfoShape::Saw => 2.0 * phase - 1.0,
                    };
                    push!(v);
                }

                Op::Smooth(slot) => {
                    // stack: x, tau  (top is tau)
                    let tau = pop1!();
                    let x = pop1!();
                    let dt = b.time_step;
                    // First-order approximation of 1 - exp(-dt/tau); cheap, stable.
                    let denom = tau + dt;
                    let alpha = if denom > 0.0 { dt / denom } else { 1.0 };
                    let s_ref = unsafe { smooth.get_unchecked_mut(slot as usize) };
                    let new = *s_ref + alpha * (x - *s_ref);
                    *s_ref = new;
                    push!(new);
                }
            }
        }

        if sp == 0 {
            0.0
        } else {
            unsafe { *stack.get_unchecked(0) }
        }
    }
}