# Parameter Integration Plan

## Current Architecture Summary

### What exists

**Parameter system** (`copperhead_core/src/parameter/`):
- `ParamTable`: lock-free atomic slab (`AtomicF64` / `AtomicU64`) shared between UI and simulation threads
- `ParamSystemBuilder`: declares params at build time, freezes into `ParamSystem`
- `ParamSystem`: owns the symbol table, compiles formula strings into `Program` bytecode
- `Program`: stack-machine bytecode (math, logic, LFOs, smoothing, ternary select)
- `FormulaInstance`: per-use evaluator holding a `Program` + per-instance smoothing state
- `Op::LoadParam(u16)` / `Op::LoadEnum(u16)` / `Op::LoadVoltage(u16)`: the bytecode already reads from `ParamTable` at eval time

**Component system** (`copperhead_core/src/components/`):
- `Component<T>` trait with `stamp_static`, `stamp_dynamic`, `stamp_time_variant`, `stamp_nonlinear`, `update_state`
- `ComponentLinearity` enum: `LinearStatic`, `LinearDynamic`, `TimeVariant`, `NonLinear`
- `set_parameter(name, value) -> bool` for runtime updates from the UI
- Components store their own scalar values (e.g. `Resistor::conductance`, `Potentiometer::position`)

**Solver** (`copperhead_core/src/circuit.rs`):
- Schur complement partitioning: A_LL (linear-static pre-factored), A_NN (everything else)
- Time-variant and nonlinear components stamp into A_NN during Newton-Raphson
- `stamp_all_time_variant()` is called every Newton iteration on the reduced block

**UI** (`copperhead_builder/src/ui/`):
- `ComponentUIExt` trait: `draw_modal()` for editing, `draw_property_panel()` for runtime control
- `SimCommand::UpdateValue { component_idx, name, value }` sends changes to simulation thread
- Builder-side `*Def` structs (e.g. `ResistorDef`) hold the UI-facing values

**Current value flow**: UI slider -> `SimCommand::UpdateValue` -> `component.set_parameter()` -> direct scalar write -> matrix rebuild / restamp.

---

## The Gap

Components currently store plain scalars. There is no way for a component to say "my resistance is `2 * knob1 + 100`". The parameter system exists but nothing connects it to components or to the partitioner's linearity decisions.

---

## Design: `ParamValue<T>`

The central idea: replace every user-facing scalar in a component with a `ParamValue<T>` that is either a constant or a compiled formula.

```rust
pub enum ParamValue {
    Constant(f64),
    Formula(FormulaInstance),
}

impl ParamValue {
    /// Hot path: evaluate to a concrete f64.
    /// For constants this is a plain read; for formulas it runs the bytecode.
    pub fn eval(&mut self, ctx: &EvalContext) -> f64 {
        match self {
            Self::Constant(v) => *v,
            Self::Formula(f) => f.eval(ctx.voltages, ctx.builtins),
        }
    }

    pub fn is_dynamic(&self) -> bool {
        matches!(self, Self::Formula(_))
    }
}
```

`EvalContext` bundles the `&ParamTable`, node voltage slice, and builtin values (sample rate, dt, time, etc.) that `Program::eval` already needs.

### Why a single wrapper

- Components don't need to know whether they're driven by a knob, an LFO, or a constant.
- The linearity check becomes trivial: if any `ParamValue` in a component is `Formula`, promote the component's linearity.
- The UI can toggle between "type a number" and "type a formula" per field without touching component internals.

---

## Integration: Components

### Step 1 - Add `ParamValue` to component structs

For every user-editable field, replace the scalar with `ParamValue`:

```rust
pub struct Resistor<T: CircuitScalar> {
    // before: resistance: T, conductance: T
    resistance: ParamValue,   // user-facing
    conductance: T,           // derived, recomputed from resistance.eval()
    // ...
}
```

Similarly:
- `Capacitor`: `capacitance: ParamValue`, `esr: ParamValue`
- `Potentiometer`: `position: ParamValue`, `total_resistance: ParamValue`
- `Switch`: needs a `ParamValue` that evaluates to a bool (threshold at 0.5, or use enum param)

### Step 2 - Refresh derived values

Add a method (or integrate into existing stamping) that re-evaluates ParamValues:

```rust
impl<T: CircuitScalar> Resistor<T> {
    fn refresh(&mut self, ctx: &EvalContext) {
        let r = self.resistance.eval(ctx);
        self.conductance = T::from_f64(1.0 / r);
    }
}
```

For `LinearStatic` components that become formula-driven, `refresh()` would be called inside `stamp_time_variant()`. For components already `TimeVariant` or `NonLinear`, it slots into their existing per-iteration stamp method.

### Step 3 - Dynamic linearity

Override `linearity()` to account for formula parameters:

```rust
fn linearity(&self) -> ComponentLinearity {
    if self.resistance.is_dynamic() {
        ComponentLinearity::TimeVariant  // promote from LinearStatic
    } else {
        ComponentLinearity::LinearStatic // original
    }
}
```

Rules for promotion:
| Base linearity | Has formula param? | Effective linearity |
|---|---|---|
| LinearStatic | no | LinearStatic |
| LinearStatic | yes | TimeVariant |
| LinearDynamic | no | LinearDynamic |
| LinearDynamic | yes | TimeVariant (*) |
| TimeVariant | either | TimeVariant |
| NonLinear | either | NonLinear |

(*) A `LinearDynamic` component like a capacitor stamps both a conductance into G and a history current into b. If its capacitance is formula-driven, the conductance changes every step, so it must move out of the pre-factored linear block into the active block. This makes it `TimeVariant`. Its `stamp_time_variant()` would then both update G and stamp the history current. This is the most invasive change because `LinearDynamic` components currently rely on `stamp_static()` for G and `stamp_dynamic()` for b; with a formula-driven value, the G part must move to `stamp_time_variant()`.

### Step 4 - `set_parameter` evolution

`set_parameter` currently takes `(name: &str, value: f64)`. Extend or complement it:

```rust
fn set_param_value(&mut self, name: &str, pv: ParamValue) -> bool {
    match name {
        "resistance" => {
            self.resistance = pv;
            true // linearity may have changed, needs re-partition check
        }
        _ => false,
    }
}
```

The existing `set_parameter(name, f64)` can remain as a convenience that wraps the value in `ParamValue::Constant`.

---

## Integration: Solver / Partitioner

### Partitioning awareness

The partitioner in `circuit.rs` already queries `component.linearity()` to decide which block a component belongs to. With dynamic linearity from Step 3, this works automatically **at build time**. However, there is a subtlety:

**Re-partitioning on formula assignment**: If the user changes a resistor from `1000` (constant) to `knob1 * 1000` (formula) while the simulation is running, the resistor's linearity changes from `LinearStatic` to `TimeVariant`. This means the matrix partitioning is now wrong: the resistor's nodes are in the L-block but need to be in the N-block.

Options:
1. **Re-partition and re-factor** (simplest, recommended initially): Tear down `SolverState`, call `prepare()` again. This is expensive but only happens when the user edits a formula, not every step. The simulation can pause for one batch while this happens.
2. **Always partition conservatively**: At build time, if a component *could* have a formula (i.e. it has any `ParamValue` field), put it in the active block. Downside: purely constant resistors still end up in A_NN if the system supports formulas on them.
3. **Lazy promotion**: Keep the component in A_LL until a formula is assigned, then re-partition. This is option 1 with the intent made explicit.

**Recommendation**: Option 1 (re-partition on change). It keeps the common case fast (constant components stay in A_LL) and the uncommon case (user types a formula) acceptably slow. The UI should debounce formula edits so you don't re-partition on every keystroke.

### EvalContext plumbing

`solve_step()` needs to pass an `EvalContext` to stamping methods. Today, `stamp_time_variant()` and `stamp_nonlinear()` receive matrix references but no eval context. Add `EvalContext` (or its constituents) as an argument:

```rust
fn stamp_time_variant(&mut self, ctx: &EvalContext, matrix: &mut MatMut<T>);
fn stamp_nonlinear(&mut self, ctx: &EvalContext, x: &[T], matrix: &mut MatMut<T>, rhs: &mut [T]);
```

`EvalContext` is cheap to construct each step:
```rust
struct EvalContext<'a> {
    table: &'a ParamTable,
    voltages: &'a [f64],       // current node voltage solution
    builtins: &'a BuiltinValues,  // sample_rate, dt, time, step_index, bpm, oversampling
}
```

The `voltages` field enables voltage-dependent formulas (e.g. a resistance that depends on a node voltage - essentially a user-defined nonlinearity). If a formula references a voltage probe, the component should be `NonLinear` rather than `TimeVariant`, because its value depends on the solution being iterated. This distinction matters:
- Formula references only params/builtins/LFOs -> `TimeVariant` (value fixed within one Newton iteration)
- Formula references a voltage probe -> `NonLinear` (value changes during Newton iteration)

The `Program` already tracks which `Op` types it contains. Add a method `program.depends_on_voltage() -> bool` that scans the bytecodes for `Op::LoadVoltage`. Use this to decide the linearity promotion level.

---

## Integration: UI (copperhead_builder)

### Builder-side Def structs

The `*Def` structs (`ResistorDef`, `CapacitorDef`, etc.) in the builder currently store plain `f64`. These need a UI-side representation:

```rust
/// Builder-side parameter value (serializable, not yet compiled)
pub enum ParamValueDef {
    Constant(f64),
    Formula(String),  // raw source text, compiled at circuit build time
}
```

`ParamValueDef` is what gets saved/loaded with the schematic. Compilation to `ParamValue` (with `FormulaInstance`) happens during `compile_netlist()` when the `ParamSystem` is available.

### Modal editing (draw_modal)

For each editable field, show a toggle or auto-detect mode:

```
[Resistance] [= ] [1000        ]   <- constant mode, shows a DragValue
[Resistance] [f(x)] [knob1 * 1k ]   <- formula mode, shows a text input
```

Implementation sketch:
- If the text starts with `=` or contains any non-numeric characters (beyond SI suffixes), treat it as a formula.
- Alternatively, add a small toggle button next to each field.
- On commit, parse: if it's a valid number, store `ParamValueDef::Constant(x)`. Otherwise, try to compile the formula via `ParamSystem::compile()`. Show errors inline if compilation fails.

### Pinned parameters window

This is the live-control surface. It renders `ParamTable` entries that the user has marked as "pinned":

```rust
struct PinnedParam {
    param_idx: u16,         // index into ParamTable
    name: String,           // display name
    kind: PinnedParamKind,  // Slider, Toggle, Dropdown
    range: (f64, f64),      // min/max for sliders
    step: f64,              // step size
}

enum PinnedParamKind {
    Slider,
    Toggle,    // for boolean/enum params
    Dropdown,  // for enum params with multiple options
}
```

Rendering:
- Sliders write to `ParamTable::set(idx, value)` (atomic, lock-free).
- Toggles write to `ParamTable::set_enum(idx, value)`.
- No `SimCommand` needed. The simulation thread reads the new value on the next `Program::eval()` call automatically. This is the beauty of the atomic `ParamTable` design: the UI and simulation communicate through shared memory with no channel overhead for parameter changes.

### Declaring parameters

The user needs a way to create named parameters. Options:
1. **Explicit parameter manager panel**: A dedicated UI panel where you create/rename/delete parameters, set their type (numeric/bool/enum), default value, range, and pin status.
2. **Implicit from formulas**: When a formula references an undeclared name, prompt the user to create it. This is more magical but can be confusing.

**Recommendation**: Option 1, with auto-suggestion from option 2. Have a parameter manager panel. When the user types a formula referencing an unknown symbol, show a "Create parameter 'knob1'?" prompt.

### Netlist compilation changes

In `compile_netlist()`, after building the node map:
1. Build the `ParamSystem` from the user's declared parameters.
2. For each component's `ParamValueDef::Formula(src)`, compile it via `param_system.compile(&src)` to get a `Program`.
3. Wrap in `FormulaInstance` and pass to the core component constructor.
4. Pass `Arc<ParamTable>` to the simulation thread (it may already have it if using the same `ParamSystem`).

### SimCommand changes

For pinned parameter sliders, **no SimCommand is needed** (atomic writes to `ParamTable`).

For structural changes (user edits a formula, adds/removes a parameter), send:
```rust
SimCommand::RebuildCircuit(Netlist)  // recompile and re-partition
```

This is already conceptually what `SimCommand::LoadCircuit` does.

---

## Integration: Smoothing

The parameter system has `Op::Smooth(slot, coeff)` which applies a one-pole lowpass to parameter changes. This is critical for avoiding clicks/pops when the user moves a slider.

Each `FormulaInstance` holds its own `smooth_state: Vec<f64>`. When the user moves a potentiometer knob, the raw `ParamTable` value jumps instantly, but the formula `smooth(knob1, 0.01)` will slew it over ~10ms (at 96kHz). This means:
- The component's value changes gradually, not abruptly.
- The solver sees a gently changing parameter, which helps Newton-Raphson convergence.
- No zipper noise in the audio output.

Users should be encouraged to wrap interactive parameters in `smooth()` when they control audio-path components. The UI could even auto-insert smoothing for pinned slider parameters.

---

## Execution Order

### Phase 1: Core plumbing
1. Add `ParamValue` enum to `copperhead_core`
2. Add `EvalContext` struct
3. Modify `Component<T>` trait: add `EvalContext` to `stamp_time_variant` and `stamp_nonlinear` signatures
4. Implement `ParamValue` in one component (Potentiometer is the best candidate - already `TimeVariant`, already has a position slider)

### Phase 2: Linearity promotion
5. Make `linearity()` dynamic based on `ParamValue::is_dynamic()`
6. Add `Program::depends_on_voltage()` for `NonLinear` promotion
7. Wire re-partitioning when linearity changes (simplest: re-call `prepare()`)

### Phase 3: UI
8. Add `ParamValueDef` to builder-side Def structs
9. Add formula input mode to `draw_modal()`
10. Add parameter manager panel (create/delete/rename parameters)
11. Add pinned parameter window with sliders/toggles
12. Wire `ParamTable` into simulation thread

### Phase 4: Remaining components
13. Convert Resistor, Capacitor, Switch, VoltageSource (AC frequency/amplitude) to use `ParamValue`
14. Handle `LinearDynamic` -> `TimeVariant` promotion for capacitors (move G stamp from `stamp_static` to `stamp_time_variant` when formula-driven)

### Phase 5: Polish
15. Auto-smoothing suggestion for pinned audio-path parameters
16. Formula validation and error display in UI
17. Save/load `ParamValueDef` in schematic files
18. Voltage probe parameters (declare probes, use in formulas, handle `NonLinear` promotion)
