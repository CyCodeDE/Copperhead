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

//! Parameter system: a lock-free, bytecode-VM-driven way to flow user-facing
//! values (knobs, switches) and internal solver values (node voltages) into
//! component formulas on the simulation thread.
//!
//! ## Architecture
//!
//! ```text
//!  UI thread                            Simulation / audio thread
//!  ─────────                            ────────────────────────
//!  ParamHandle::set(idx, v)  ──writes──►  Arc<ParamTable>
//!       (AtomicF64 store,                  AtomicF64 load (Relaxed)
//!        wait-free, ~3 ns)                 inside Program::eval
//! ```
//!
//! Both sides share a single `Arc<ParamTable>` that is constructed during
//! circuit build and never resized afterwards. All numeric parameters are
//! addressed by a stable `u16` index assigned at build time. The UI writes
//! directly into those atomic slots; the VM reads them every simulation step.
//! There is **no queue, no lock, no allocation** on the hot path.
//!
//! For structural changes (re-assigning formulas, adding/removing params,
//! resizing the table) the simulation thread must be rebuilt — those aren't
//! hot-path operations.
//!
//! ## Typical integration with the solver
//!
//! 1. During circuit build, create a [`ParamSystemBuilder`].
//! 2. For each knob/switch exposed by a component, call
//!    [`ParamSystemBuilder::declare_param`] (or `declare_enum`). Store the
//!    returned `u16` on the component.
//! 3. For each node whose voltage a component wants to read inside a formula,
//!    call [`ParamSystemBuilder::declare_voltage`].
//! 4. Call [`ParamSystemBuilder::build`] to freeze the layout and obtain a
//!    [`ParamSystem`]. Clone its `Arc<ParamTable>` into every place that
//!    needs it (one copy for the UI side, one for the simulation side).
//! 5. Compile any formula strings via [`ParamSystem::compile`], and store
//!    the resulting [`parser::Program`] inside the component. Allocate one
//!    `Vec<f64>` of length `program.smooth_slots` per formula instance.
//! 6. On the simulation thread, every step:
//!    - gather the latest node voltages into a `&[f64]` slice,
//!    - fill a [`parser::BuiltinValues`],
//!    - build a [`parser::EvalContext`] pointing at the shared slices,
//!    - call `program.eval(&mut ctx)` to get the live parameter value,
//!    - feed that into the component's stamping.
//! 7. On the UI thread, the event loop calls
//!    [`ParamSystem::set_param`] / [`ParamTable::set`] in response to knob
//!    changes. These are wait-free and safe to call from any thread.

pub mod parser;

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::Ordering;

use atomic_float::AtomicF64;
use portable_atomic::AtomicU64;

use parser::{BuiltinValues, CompileError, Program, SymbolTable, compile};

// ---------------------------------------------------------------------------
// Shared, lock-free parameter storage
// ---------------------------------------------------------------------------

/// The slim per-step context handed to component code on the simulation
/// thread. Holds everything a [`ParamValue::eval`] needs *outside* of
/// per-formula state:
/// - `voltages`: snapshot of node voltages in the layout established by
///   `ParamSystemBuilder::declare_voltage`.
/// - `builtins`: scalar inputs (sample rate, dt, time, ...).
///
/// Per-formula state (smoothing) lives inside each `FormulaInstance`.
/// Atomic param/enum reads happen out-of-band via the `Arc<ParamTable>` that
/// each `FormulaInstance` owns.
pub struct ComponentEvalCtx<'a> {
    pub voltages: &'a [f64],
    pub builtins: &'a BuiltinValues,
}

/// Outcome of mutating a parameter on a component. The simulation thread
/// uses this to decide what to do with the matrix.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RebuildKind {
    /// Value swapped, no matrix change required (e.g. b-only voltage source).
    None,
    /// Static matrix needs re-stamping but the L/N partitioning is unchanged
    /// (e.g. a constant resistor's value changed).
    Restamp,
    /// Linearity changed — must rerun `Circuit::prepare()` to re-partition
    /// and re-factorise A_LL.
    Repartition,
}

pub enum ParamValue {
    Constant(f64),
    Formula(FormulaInstance),
}

impl ParamValue {
    pub fn constant(v: f64) -> Self {
        Self::Constant(v)
    }

    pub fn formula(program: Arc<Program>, table: Arc<ParamTable>) -> Self {
        Self::Formula(FormulaInstance::new(program, table))
    }

    /// Hot path: evaluate to a concrete f64.
    /// For constants this is a plain read; for formulas it runs the bytecode.
    pub fn eval(&mut self, ctx: &ComponentEvalCtx) -> f64 {
        match self {
            Self::Constant(v) => *v,
            Self::Formula(f) => f.eval(ctx.voltages, ctx.builtins),
        }
    }

    /// True if this value is formula-driven (changes between samples).
    pub fn is_dynamic(&self) -> bool {
        matches!(self, Self::Formula(_))
    }

    /// True if this value reads a node voltage (and so participates in
    /// the Newton-Raphson iteration as a user-defined nonlinearity).
    pub fn depends_on_voltage(&self) -> bool {
        match self {
            Self::Constant(_) => false,
            Self::Formula(f) => f.depends_on_voltage(),
        }
    }

    /// True if this value can be safely treated as constant during freeze.
    /// Constants always qualify; formulas qualify when they have no voltage
    /// or builtin dependencies (see `Program::is_freeze_eligible`).
    pub fn is_freeze_eligible(&self) -> bool {
        match self {
            Self::Constant(_) => true,
            Self::Formula(f) => f.is_freeze_eligible(),
        }
    }

    /// Returns `Some(v)` if this value is a plain constant.
    pub fn as_constant(&self) -> Option<f64> {
        match self {
            Self::Constant(v) => Some(*v),
            Self::Formula(_) => None,
        }
    }
}

/// The shared atomic slab that both the UI thread and the simulation thread
/// point at. Fixed layout after construction.
pub struct ParamTable {
    values: Box<[AtomicF64]>,
    enums: Box<[AtomicU64]>,
}

impl ParamTable {
    fn with_defaults(values: &[f64], enums: &[u64]) -> Self {
        Self {
            values: values.iter().map(|v| AtomicF64::new(*v)).collect(),
            enums: enums.iter().map(|v| AtomicU64::new(*v)).collect(),
        }
    }

    /// Wait-free write. Safe from any thread.
    #[inline]
    pub fn set(&self, idx: u16, v: f64) {
        if let Some(slot) = self.values.get(idx as usize) {
            slot.store(v, Ordering::Relaxed);
        }
    }

    /// Wait-free read.
    #[inline]
    pub fn get(&self, idx: u16) -> f64 {
        self.values
            .get(idx as usize)
            .map(|s| s.load(Ordering::Relaxed))
            .unwrap_or(0.0)
    }

    #[inline]
    pub fn set_enum(&self, idx: u16, v: u64) {
        if let Some(slot) = self.enums.get(idx as usize) {
            slot.store(v, Ordering::Relaxed);
        }
    }

    #[inline]
    pub fn get_enum(&self, idx: u16) -> u64 {
        self.enums
            .get(idx as usize)
            .map(|s| s.load(Ordering::Relaxed))
            .unwrap_or(0)
    }

    /// Borrow the underlying slices. Hand these to an [`EvalContext`] once
    /// per evaluation.
    #[inline]
    pub fn values_slice(&self) -> &[AtomicF64] {
        &self.values
    }
    #[inline]
    pub fn enums_slice(&self) -> &[AtomicU64] {
        &self.enums
    }

    pub fn param_count(&self) -> usize {
        self.values.len()
    }
    pub fn enum_count(&self) -> usize {
        self.enums.len()
    }
}

// ---------------------------------------------------------------------------
// Builder
// ---------------------------------------------------------------------------

/// Collects parameter / enum / voltage declarations before the simulation
/// starts, then freezes them into a [`ParamSystem`].
#[derive(Default)]
pub struct ParamSystemBuilder {
    param_names: HashMap<String, u16>,
    param_defaults: Vec<f64>,

    enum_names: HashMap<String, u16>,
    enum_defaults: Vec<u64>,

    voltage_names: HashMap<String, u16>,
    voltage_count: u16,
}

impl ParamSystemBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Declare (or look up) a global numeric parameter. Returns its stable index.
    pub fn declare_param(&mut self, name: &str, default: f64) -> u16 {
        if let Some(&i) = self.param_names.get(name) {
            return i;
        }
        let i = self.param_defaults.len() as u16;
        self.param_defaults.push(default);
        self.param_names.insert(name.to_string(), i);
        i
    }

    /// Declare (or look up) a discrete enum-like parameter (e.g. a switch).
    pub fn declare_enum(&mut self, name: &str, default: u64) -> u16 {
        if let Some(&i) = self.enum_names.get(name) {
            return i;
        }
        let i = self.enum_defaults.len() as u16;
        self.enum_defaults.push(default);
        self.enum_names.insert(name.to_string(), i);
        i
    }

    /// Bind a voltage-probe name to an index in the solver's voltage slice.
    /// Unlike params/enums, voltages are owned by the solver — the index here
    /// is a simple sequential allocation and the caller is responsible for
    /// plumbing it through to the voltage slice layout it feeds to
    /// `EvalContext::voltages`. Returns the allocated index.
    pub fn declare_voltage(&mut self, name: &str) -> u16 {
        if let Some(&i) = self.voltage_names.get(name) {
            return i;
        }
        let i = self.voltage_count;
        self.voltage_count += 1;
        self.voltage_names.insert(name.to_string(), i);
        i
    }

    /// Freeze the layout and produce a usable [`ParamSystem`].
    pub fn build(self) -> ParamSystem {
        let table = Arc::new(ParamTable::with_defaults(
            &self.param_defaults,
            &self.enum_defaults,
        ));

        let symbols = SymbolTable {
            params: self.param_names.clone(),
            enums: self.enum_names.clone(),
            voltages: self.voltage_names.clone(),
        };

        ParamSystem {
            table,
            symbols,
            param_names: self.param_names,
            enum_names: self.enum_names,
            voltage_count: self.voltage_count as usize,
        }
    }
}

// ---------------------------------------------------------------------------
// Frozen system
// ---------------------------------------------------------------------------

/// The user-facing frozen parameter system. Hand out `Arc<ParamTable>` to
/// whoever needs it (UI widgets, simulation thread) and use `compile` to
/// turn formula strings into ready-to-eval [`Program`]s.
pub struct ParamSystem {
    table: Arc<ParamTable>,
    symbols: SymbolTable,
    param_names: HashMap<String, u16>,
    enum_names: HashMap<String, u16>,
    voltage_count: usize,
}

impl ParamSystem {
    /// Clone the shared table. Give one to the UI side and one to the
    /// simulation side. Both point at the same atomic slab.
    pub fn table(&self) -> Arc<ParamTable> {
        self.table.clone()
    }

    /// Borrow the symbol table to compose with component-local symbols via
    /// [`SymbolTable::merged`].
    pub fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }

    /// Compile a formula against the global symbols only.
    pub fn compile(&self, src: &str) -> Result<Program, CompileError> {
        compile(src, &self.symbols)
    }

    /// Compile a formula against the global symbols plus a set of component-
    /// local symbols. Component-local entries shadow globals of the same name.
    pub fn compile_with(&self, src: &str, locals: &SymbolTable) -> Result<Program, CompileError> {
        let merged = self.symbols.merged(locals);
        compile(src, &merged)
    }

    /// Look up a parameter by name (for the UI to bind knobs to indices).
    pub fn param_index(&self, name: &str) -> Option<u16> {
        self.param_names.get(name).copied()
    }
    pub fn enum_index(&self, name: &str) -> Option<u16> {
        self.enum_names.get(name).copied()
    }

    /// Convenience: write a parameter by name. Wait-free.
    pub fn set_param(&self, name: &str, v: f64) -> bool {
        match self.param_names.get(name) {
            Some(&i) => {
                self.table.set(i, v);
                true
            }
            None => false,
        }
    }

    pub fn set_enum(&self, name: &str, v: u64) -> bool {
        match self.enum_names.get(name) {
            Some(&i) => {
                self.table.set_enum(i, v);
                true
            }
            None => false,
        }
    }

    pub fn voltage_count(&self) -> usize {
        self.voltage_count
    }

    /// Allocate a fresh smoothing-state buffer for one instance of `prog`.
    pub fn alloc_smooth_state(&self, prog: &Program) -> Vec<f64> {
        vec![0.0; prog.smooth_slots as usize]
    }
}

// ---------------------------------------------------------------------------
// Per-formula evaluator helper
// ---------------------------------------------------------------------------

/// Thin bundle of everything a component needs to evaluate one formula on
/// the simulation thread. Construct once at build time, call `eval` every
/// step.
///
/// `FormulaInstance` owns only the per-instance smoothing state and a handle
/// to the program and the shared table. Voltages and builtins are passed in
/// at call time because they change every step and are laid out by the
/// solver.
pub struct FormulaInstance {
    program: Arc<Program>,
    table: Arc<ParamTable>,
    smooth_state: Vec<f64>,
    depends_on_voltage: bool,
    is_freeze_eligible: bool,
}

impl FormulaInstance {
    pub fn new(program: Arc<Program>, table: Arc<ParamTable>) -> Self {
        let smooth_state = vec![0.0; program.smooth_slots as usize];
        let depends_on_voltage = program.depends_on_voltage();
        let is_freeze_eligible = program.is_freeze_eligible();
        Self {
            program,
            table,
            smooth_state,
            depends_on_voltage,
            is_freeze_eligible,
        }
    }

    /// Hot path. `voltages` is the solver's current voltage slice in the
    /// layout established by `ParamSystemBuilder::declare_voltage`.
    #[inline]
    pub fn eval(&mut self, voltages: &[f64], builtins: &BuiltinValues) -> f64 {
        let mut ctx = parser::EvalContext {
            params: self.table.values_slice(),
            enums: self.table.enums_slice(),
            voltages,
            builtins,
            smooth_state: &mut self.smooth_state,
        };
        self.program.eval(&mut ctx)
    }

    /// True if the underlying program reads any node voltage.
    /// Cached at construction so component linearity decisions are O(1).
    #[inline]
    pub fn depends_on_voltage(&self) -> bool {
        self.depends_on_voltage
    }

    /// True if this formula's only dynamic inputs are user parameters —
    /// no voltage reads and no builtin reads. A component whose every
    /// formula is freeze-eligible can be promoted to `LinearStatic` while
    /// the simulation is frozen.
    #[inline]
    pub fn is_freeze_eligible(&self) -> bool {
        self.is_freeze_eligible
    }

    pub fn reset_smoothing(&mut self) {
        for s in &mut self.smooth_state {
            *s = 0.0;
        }
    }
}

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

/// Resolve a user-supplied string into a [`ParamValue`] at circuit-build time.
///
/// - Plain number strings (`"10000"`, `"1e-6"`) → `ParamValue::Constant`.
/// - Everything else is compiled as a formula against the global symbols; on
///   compile failure a warning is printed and `ParamValue::Constant(0.0)` is
///   returned as a safe fallback.
pub fn resolve_param_value(src: &str, param_system: &ParamSystem) -> ParamValue {
    let trimmed = src.trim();
    if let Ok(v) = trimmed.parse::<f64>() {
        return ParamValue::Constant(v);
    }
    
    if let Some(v) = crate::util::parse_si(trimmed) {
        return ParamValue::Constant(v);
    }
    match param_system.compile(trimmed) {
        Ok(prog) => ParamValue::formula(Arc::new(prog), param_system.table()),
        Err(e) => {
            eprintln!("copperhead: failed to compile formula {:?}: {}", trimmed, e);
            ParamValue::Constant(0.0)
        }
    }
}

// TESTS

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn end_to_end_system() {
        let mut b = ParamSystemBuilder::new();
        let gain_idx = b.declare_param("gain", 1.0);
        let _ = b.declare_voltage("vout");
        let sys = b.build();

        assert_eq!(sys.param_index("gain"), Some(gain_idx));

        let prog = Arc::new(sys.compile("clamp(gain * vout, -1, 1)").unwrap());

        let mut inst = FormulaInstance::new(prog.clone(), sys.table());

        // Mimic the UI pushing a knob update.
        sys.set_param("gain", 2.0);

        let builtins = BuiltinValues::default();
        let voltages = [0.6];
        assert_eq!(inst.eval(&voltages, &builtins), 1.0); // clamped

        sys.set_param("gain", 0.5);
        assert_eq!(inst.eval(&voltages, &builtins), 0.3);
    }
}
