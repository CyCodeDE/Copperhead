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

//! Transient simulation tests.
//!
//! Each test starts from a DC operating point and steps the circuit forward
//! in time, verifying that the BDF2 integrator converges to the physically
//! correct steady-state or produces a qualitatively correct waveform.
//!
//! Pattern: `solve_dc()` → `prepare_transient()` → `solve_steps(n)` → assert.

mod common;

use common::{assert_in_range, TestCircuit, AUDIO_DT};
use copperhead_core::components::diode::DiodeModel;
use std::f64::consts::PI;

// ---------------------------------------------------------------------------
// RC charging / discharging
// ---------------------------------------------------------------------------

/// After 5 time constants the capacitor voltage should be within 1% of V_in.
///
/// τ = R·C = 1kΩ · 1µF = 1ms.
/// At 96 kHz, dt ≈ 10.4µs → 5τ ≈ 480 steps.
#[test]
fn transient_rc_charges_to_supply() {
    let r = 1_000.0; // 1 kΩ
    let c = 1e-6; // 1 µF
    let tau = r * c; // 1 ms
    let dt = AUDIO_DT;
    let steps = (5.0 * tau / dt).ceil() as usize;

    let v_in = 5.0;

    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, v_in);
    tc.add_resistor(n1, n2, r);
    tc.add_capacitor(n2, gnd, c);

    tc.solve_dc().expect("DC op-point failed");
    tc.prepare_transient();
    tc.solve_steps(steps);

    let v_c = tc.v(n2);
    // After 5τ: V(t) = V_in · (1 − e^{−5}) ≈ 0.9933 · V_in
    assert!(
        v_c > 0.99 * v_in,
        "After 5τ, V_C should be >99% of V_in={v_in:.3}; got {v_c:.6}"
    );
    assert!(
        v_c <= v_in + 1e-6,
        "V_C should never exceed supply; got {v_c:.6} > {v_in}"
    );
}

/// Capacitor voltage is strictly monotonically increasing during a step response.
///
/// This guards against oscillation or overshoot that would indicate an unstable
/// integration scheme.
#[test]
fn transient_rc_monotonic_charge() {
    let dt = AUDIO_DT;
    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 10.0);
    tc.add_resistor(n1, n2, 1000.0);
    tc.add_capacitor(n2, gnd, 1e-6);

    tc.solve_dc().expect("DC op-point failed");
    tc.prepare_transient();

    let mut prev = tc.v(n2);
    for i in 0..500 {
        tc.solve_steps(1);
        let cur = tc.v(n2);
        assert!(
            cur >= prev - 1e-10,
            "Non-monotonic at step {i}: prev={prev:.8}, cur={cur:.8}"
        );
        prev = cur;
    }
}

/// RC cold-start: capacitor initialised to 0V, step response to 5V supply.
///
/// By skipping the DC operating point the capacitor history begins at zero,
/// giving a clean step response from 0V. After 1 time constant (τ = RC)
/// the voltage should reach ≈ 63.2% of V_in.
///
/// τ = 1kΩ · 1µF = 1 ms → at 96 kHz ≈ 96 steps.
#[test]
fn transient_rc_charges_from_cold_start() {
    let dt = AUDIO_DT;
    let r = 1_000.0;
    let c = 1e-6;
    let tau = r * c;

    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 5.0);
    tc.add_resistor(n1, n2, r);
    tc.add_capacitor(n2, gnd, c);

    // Prepare for transient directly — no DC analysis, cap history starts at 0V.
    tc.prepare_transient();

    let steps_per_tau = (tau / dt).ceil() as usize;
    tc.solve_steps(steps_per_tau);

    let v_c = tc.v(n2);
    // After exactly 1τ: V(τ) = 5 · (1 − e^{−1}) ≈ 3.16V.
    // Allow ±15% for BDF2 startup discretisation error.
    assert!(
        v_c > 2.4 && v_c < 4.0,
        "After 1τ cold start, expected V_C ≈ 3.16V; got {v_c:.6}"
    );
}

// ---------------------------------------------------------------------------
// RL transient
// ---------------------------------------------------------------------------

/// RL cold-start step response: with the inductor current initialised to 0,
/// the inductor initially blocks current (open circuit). The node between R
/// and L therefore sits at V_in at t = 0+ and decays toward zero as the
/// inductor current ramps up:
///
///   V_n2(t) = V_in · e^{−t/τ},   τ = L / R
///
/// τ = L / R = 100mH / 1kΩ = 100µs → ≈ 9.6 steps per τ at 96 kHz, so 5τ ≈ 48 steps.
/// Skipping `solve_dc()` is what gives the cold initial condition — otherwise
/// the inductor would be pre-charged to its DC steady-state current and there
/// would be no transient to observe.
#[test]
fn transient_rl_cold_start_decay() {
    let r = 1_000.0; // 1 kΩ
    let l = 100e-3; // 100 mH
    let tau = l / r; // 100 µs
    let dt = AUDIO_DT;
    let v_in = 5.0;

    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, v_in);
    tc.add_resistor(n1, n2, r);
    // Negligible Rs so τ is set by L/R, not L/Rs.
    tc.add_inductor(n2, gnd, l, 1e-3);

    // Skip solve_dc(): inductor history starts at i = 0 (open).
    tc.prepare_transient();

    // Step 1: inductor still blocks current → V_n2 should be near V_in.
    // Exact BE estimate after dt ≈ 0.1τ: V_n2 ≈ V_in · e^{−0.1} ≈ 4.52 V.
    tc.solve_steps(1);
    let v_initial = tc.v(n2);
    assert!(
        v_initial > 0.7 * v_in,
        "At t = 0+, inductor should block current and hold V_n2 near V_in; got {v_initial:.4}"
    );

    // Decay must be monotonic — any rise would indicate oscillation/overshoot.
    let steps_5tau = (5.0 * tau / dt).ceil() as usize;
    let mut prev = v_initial;
    for i in 1..steps_5tau {
        tc.solve_steps(1);
        let cur = tc.v(n2);
        assert!(
            cur <= prev + 1e-9,
            "Non-monotonic RL decay at step {i}: prev={prev:.8}, cur={cur:.8}"
        );
        prev = cur;
    }

    // After 5τ exact: V_n2 = V_in · e^{−5} ≈ 0.0067·V_in ≈ 0.034 V.
    // Allow generous slack for BDF1/BDF2 startup error.
    let v_final = tc.v(n2);
    assert!(
        v_final.abs() < 0.1 * v_in,
        "After 5τ, V_n2 should have decayed to ≈ 0; got {v_final:.4}"
    );
}

// ---------------------------------------------------------------------------
// Step counter and time
// ---------------------------------------------------------------------------

#[test]
fn transient_step_count_advances_correctly() {
    let dt = AUDIO_DT;
    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 1.0);
    tc.add_resistor(n1, gnd, 1000.0);

    tc.solve_dc().expect("DC op-point failed");
    let after_dc = tc.step_count();

    tc.prepare_transient();
    tc.solve_steps(10);

    assert_eq!(
        tc.step_count(),
        after_dc + 10,
        "step_count should increase by exactly 10 after 10 transient steps"
    );
}

#[test]
fn transient_time_advances_monotonically() {
    let dt = AUDIO_DT;
    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 1.0);
    tc.add_resistor(n1, gnd, 1000.0);

    tc.solve_dc().expect("DC op-point failed");
    tc.prepare_transient();

    let mut prev_time = tc.circuit.time;
    for _ in 0..20 {
        tc.solve_steps(1);
        let t = tc.circuit.time;
        assert!(
            t > prev_time,
            "time should strictly increase: prev={prev_time:.8}, curr={t:.8}"
        );
        prev_time = t;
    }
}

// ---------------------------------------------------------------------------
// Frequency-domain properties (AC via transient)
// ---------------------------------------------------------------------------

/// At the cutoff frequency f_c = 1/(2πRC), a first-order RC low-pass filter
/// attenuates the input by 1/√2 ≈ 0.707 (−3 dB).
///
/// We run the circuit in transient with a sine source at f_c, wait for steady
/// state, and measure the peak output amplitude.
#[test]
fn transient_rc_lowpass_at_cutoff() {
    let r = 1_000.0; // 1 kΩ
    let c = 1e-6; // 1 µF
    let f_c = 1.0 / (2.0 * PI * r * c); // ≈ 159 Hz
    let dt = AUDIO_DT;

    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_sine_source(n1, gnd, 1.0, f_c); // 1V amplitude
    tc.add_resistor(n1, n2, r);
    tc.add_capacitor(n2, gnd, c);

    tc.solve_dc().expect("DC op-point failed");
    tc.prepare_transient();

    // Run 5 periods to reach steady state
    let period_steps = (1.0 / (f_c * dt)).ceil() as usize;
    tc.solve_steps(5 * period_steps);

    // Measure peak amplitude over 2 complete periods
    let mut peak = 0.0_f64;
    for _ in 0..(2 * period_steps) {
        tc.solve_steps(1);
        let v = tc.v(n2).abs();
        if v > peak {
            peak = v;
        }
    }

    // Theory: amplitude = 1/√2 ≈ 0.707. Allow ±20% due to BDF2 numerical
    // damping and discretisation error at this sample rate.
    assert_in_range(peak, 0.5, 0.9);
}

/// A high-frequency sine (10× f_c) should be attenuated much more than −3 dB.
///
/// At 10·f_c, |H| = 1/√(1 + 100) ≈ 0.1 — well below the cutoff.
#[test]
fn transient_rc_lowpass_high_freq_attenuated() {
    let r = 1_000.0;
    let c = 1e-6;
    let f_c = 1.0 / (2.0 * PI * r * c);
    let f_test = 10.0 * f_c; // 10× cutoff
    let dt = AUDIO_DT;

    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_sine_source(n1, gnd, 1.0, f_test);
    tc.add_resistor(n1, n2, r);
    tc.add_capacitor(n2, gnd, c);

    tc.solve_dc().expect("DC op-point failed");
    tc.prepare_transient();

    let period_steps = (1.0 / (f_test * dt)).ceil() as usize;
    // Warmup: 10 periods
    tc.solve_steps(10 * period_steps);

    // Measure over 2 periods
    let mut peak = 0.0_f64;
    for _ in 0..(2 * period_steps) {
        tc.solve_steps(1);
        let v = tc.v(n2).abs();
        if v > peak {
            peak = v;
        }
    }

    // At 10·f_c the theoretical amplitude is ~0.1. Should be well below 0.5.
    assert!(
        peak < 0.5,
        "At 10× cutoff frequency, peak amplitude should be <0.5; got {peak:.4}"
    );
}

// ---------------------------------------------------------------------------
// Diode transient (NonLinear solver stability)
// ---------------------------------------------------------------------------

/// Half-wave rectifier runs stably for hundreds of steps — tests that the
/// Newton-Raphson damping and BDF2 state updates work together without
/// diverging on a nonlinear element.
#[test]
fn transient_diode_rectifier_stable() {
    let dt = AUDIO_DT;
    let f = 1000.0; // 1 kHz input
    let period_steps = (1.0 / (f * dt)).ceil() as usize;

    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_sine_source(n1, gnd, 5.0, f); // 5V peak sine
    tc.add_resistor(n1, n2, 1000.0);
    tc.add_diode(n2, gnd, DiodeModel::_1N4148);

    tc.solve_dc().expect("DC op-point failed");
    tc.prepare_transient();

    // Run 10 complete cycles — should not panic or produce NaN
    tc.solve_steps(10 * period_steps);

    let v = tc.v(n2);
    assert!(
        v.is_finite(),
        "Diode rectifier should produce finite output; got {v}"
    );
    // Output should be non-negative (rectified)
    // (We check the current voltage, which at end of cycle could be near zero)
    assert!(v >= -0.1, "Rectifier output should be ≥ 0; got {v:.6}");
}

/// After the diode rectifier reaches steady state, the peak output voltage is
/// close to (V_peak − V_forward_drop) where V_f ≈ 0.6–0.7V for 1N4148.
#[test]
fn transient_diode_rectifier_peak_voltage() {
    let dt = AUDIO_DT;
    let f = 500.0;
    let v_peak = 5.0;
    let period_steps = (1.0 / (f * dt)).ceil() as usize;

    let mut tc = TestCircuit::with_dt(dt);
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_sine_source(n1, gnd, v_peak, f);
    tc.add_resistor(n1, n2, 1000.0);
    tc.add_diode(n2, gnd, DiodeModel::_1N4148);

    tc.solve_dc().expect("DC op-point failed");
    tc.prepare_transient();

    // Warmup: 5 cycles
    tc.solve_steps(5 * period_steps);

    // Measure peak over 2 cycles
    let mut measured_peak = 0.0_f64;
    for _ in 0..(2 * period_steps) {
        tc.solve_steps(1);
        let v = tc.v(n2);
        if v > measured_peak {
            measured_peak = v;
        }
    }

    // At the source peak (V_n1 = 5 V) the diode clamps n2 to its forward
    // voltage, and the remainder of the supply drops across R:
    //   I = (V_n1 − V_f) / R ≈ (5 − 0.7) / 1k ≈ 4.3 mA
    //   V_n2_peak = V_f ≈ 0.65–0.75 V
    assert_in_range(measured_peak, 0.4, 1.0);
}
