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

//! DC operating-point tests.
//!
//! Each test builds a small circuit with a known analytical solution and
//! verifies that `calculate_dc_operating_point` produces the correct node
//! voltages. This exercises the MNA matrix assembly, Schur-complement
//! partitioning, and Newton-Raphson convergence at once.
//!
//! All expected values are hand-calculated from basic circuit theory.

mod common;

use common::{assert_approx, assert_in_range, TestCircuit};
use copperhead_core::components::diode::DiodeModel;

/// 1V tolerance for the DC tests (we're checking physics, not floating-point precision).
const TOL: f64 = 1e-5;

// ---------------------------------------------------------------------------
// Pure resistive circuits (all LinearStatic, no Newton-Raphson needed)
// ---------------------------------------------------------------------------

/// Classic resistive voltage divider.
///
///   V_in=10V ── R1=1kΩ ── node2 ── R2=1kΩ ── GND
///
/// By Ohm's law: V_node2 = 10 * 1k/(1k+1k) = 5V
#[test]
fn dc_resistive_divider() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 10.0);
    tc.add_resistor(n1, n2, 1000.0);
    tc.add_resistor(n2, gnd, 1000.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_approx(tc.v(n1), 10.0, TOL);
    assert_approx(tc.v(n2), 5.0, TOL);
}

/// Voltage source with a single load resistor — source voltage appears at node1.
#[test]
fn dc_single_resistor_load() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 7.5);
    tc.add_resistor(n1, gnd, 220.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_approx(tc.v(n1), 7.5, TOL);
}

/// Three-node resistive ladder with equal values.
///
///   V=9V ── R=1kΩ ── n2 ── R=1kΩ ── n3 ── R=1kΩ ── GND
///
/// Equal resistors → V_n2 = 6V, V_n3 = 3V.
#[test]
fn dc_resistive_ladder_three_nodes() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let n3 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 9.0);
    tc.add_resistor(n1, n2, 1000.0);
    tc.add_resistor(n2, n3, 1000.0);
    tc.add_resistor(n3, gnd, 1000.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_approx(tc.v(n1), 9.0, TOL);
    assert_approx(tc.v(n2), 6.0, TOL);
    assert_approx(tc.v(n3), 3.0, TOL);
}

/// Two independent voltage sources, both connected to a common node via resistors.
///
///   V1=10V ── R1=1kΩ ──┐
///                       n3 ── R_load=2kΩ ── GND
///   V2=5V ──  R2=2kΩ ──┘
///
/// By nodal analysis at n3:
///   (V3 − 10)/1k + (V3 − 5)/2k + V3/2k = 0
///   2(V3−10) + (V3−5) + V3 = 0  →  4·V3 = 25  →  V3 = 6.25V
#[test]
fn dc_two_sources_nodal() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node(); // V1 terminal
    let n2 = tc.node(); // V2 terminal
    let n3 = tc.node(); // shared output
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 10.0);
    tc.add_dc_source(n2, gnd, 5.0);
    tc.add_resistor(n1, n3, 1000.0);
    tc.add_resistor(n2, n3, 2000.0);
    tc.add_resistor(n3, gnd, 2000.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_approx(tc.v(n1), 10.0, TOL);
    assert_approx(tc.v(n2), 5.0, TOL);
    assert_approx(tc.v(n3), 6.25, TOL);
}

/// Unbalanced divider: checks correct use of conductance ratios.
///
///   V=12V ── R1=3kΩ ── n2 ── R2=1kΩ ── GND
///
/// V_n2 = 12 * 1/(3+1) = 3V
#[test]
fn dc_unbalanced_divider() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 12.0);
    tc.add_resistor(n1, n2, 3000.0);
    tc.add_resistor(n2, gnd, 1000.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_approx(tc.v(n2), 3.0, TOL);
}

// ---------------------------------------------------------------------------
// Reactive elements in DC
// ---------------------------------------------------------------------------

/// Capacitor in DC is an open circuit: no current flows, no voltage at node2.
///
///   V=7V ── C=1µF ── n2 ── R=1kΩ ── GND
///
/// In DC: cap blocks current → V_n2 = 0V (R carries no current, node floats).
#[test]
fn dc_capacitor_is_open_circuit() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 7.0);
    tc.add_capacitor(n1, n2, 1e-6);
    tc.add_resistor(n2, gnd, 1000.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_approx(tc.v(n1), 7.0, TOL);
    // Capacitor is open in DC: n2 is grounded only through R with no driving current.
    assert_approx(tc.v(n2), 0.0, 1e-3);
}

/// Inductor in DC is a short circuit (only series resistance remains).
///
///   V=5V ── L=1mH(Rs=0) ── n2 ── R=1kΩ ── GND
///
/// In DC: inductor is a wire → V_n2 = V_source = 5V (assuming Rs = 0).
#[test]
fn dc_inductor_is_short_circuit() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 5.0);
    // Rs = 1mΩ instead of 0 to avoid a singular system; the error should be tiny.
    tc.add_inductor(n1, n2, 1e-3, 1e-3);
    tc.add_resistor(n2, gnd, 1000.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_approx(tc.v(n1), 5.0, TOL);
    // V_n2 ≈ 5V * (1k / (1k + 0.001)) ≈ 5V to within 1mV
    assert_approx(tc.v(n2), 5.0, 1e-2);
}

// ---------------------------------------------------------------------------
// Nonlinear: diodes
// ---------------------------------------------------------------------------

/// Forward-biased 1N4148 diode: should produce ~0.5–0.8V forward drop.
///
///   V=5V ── R=1kΩ ── n2 ── D(1N4148, anode→n2, cathode→GND)
///
/// Current ≈ (5V − V_f) / 1kΩ ≈ 4mA → V_f for 1N4148 ≈ 0.65–0.7V.
#[test]
fn dc_diode_forward_bias() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 5.0);
    tc.add_resistor(n1, n2, 1000.0);
    tc.add_diode(n2, gnd, DiodeModel::_1N4148);

    tc.solve_dc().expect("DC operating point should converge");

    let v2 = tc.v(n2);
    assert_in_range(v2, 0.4, 1.0); // Generous bounds — forward drop depends on Is/N model
}

/// Reverse-biased 1N4148 diode: blocks current, so the output node is pulled
/// close to the negative supply rail.
///
///   V=−5V ── R=1kΩ ── n2 ── D(anode→n2, cathode→GND)
///
/// Diode is reverse biased → near zero current → V_n2 ≈ −5V.
#[test]
fn dc_diode_reverse_bias() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, -5.0);
    tc.add_resistor(n1, n2, 1000.0);
    tc.add_diode(n2, gnd, DiodeModel::_1N4148);

    tc.solve_dc().expect("DC operating point should converge");

    let v2 = tc.v(n2);
    // Near-zero leakage → almost the full −5V across the diode.
    assert!(
        v2 < -4.0,
        "Reverse-biased diode: expected V_n2 < -4V, got {v2:.6}"
    );
}

/// Two nonlinear diodes in the same circuit — tests that Newton-Raphson
/// converges with multiple NonLinear elements simultaneously.
///
///   V=5V (n1) ── D1(anode→n1, cathode→n2) ── R=1kΩ ── GND
///                                              └─ D2(anode→GND, cathode→n2, reverse)
///
/// D1 is forward biased: V_n2 = V_n1 − V_f ≈ 5 − 0.7 = 4.3V.
/// D2 has anode at GND (0V) and cathode at n2 (≈4.3V) — it is reverse biased
/// and contributes only leakage current.
#[test]
fn dc_two_diodes_converge() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 5.0);
    tc.add_diode(n1, n2, DiodeModel::_1N4148); // forward biased (drops ~0.7V)
    tc.add_resistor(n2, gnd, 1000.0);
    tc.add_diode(gnd, n2, DiodeModel::_1N4148); // reverse biased (cathode at n2)

    tc.solve_dc().expect("DC operating point should converge with two diodes");

    // D1 forward drop ≈ 0.6–0.75V → V_n2 = 5 − V_f ≈ 4.25–4.4V
    let v2 = tc.v(n2);
    assert_in_range(v2, 3.5, 5.0);
}

// ---------------------------------------------------------------------------
// Sanity: ground node is always 0V
// ---------------------------------------------------------------------------

#[test]
fn dc_ground_is_always_zero() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 3.3);
    tc.add_resistor(n1, gnd, 100.0);

    tc.solve_dc().expect("DC operating point should converge");

    assert_eq!(tc.v(gnd), 0.0);
}

// ---------------------------------------------------------------------------
// Convergence: verify the solver actually returns Ok for standard topologies
// ---------------------------------------------------------------------------

#[test]
fn dc_convergence_pure_resistive() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 1.0);
    tc.add_resistor(n1, gnd, 50.0);

    assert!(
        tc.solve_dc().is_ok(),
        "Pure resistive circuit should always converge"
    );
}

#[test]
fn dc_convergence_with_capacitor_and_resistor() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 9.0);
    tc.add_resistor(n1, n2, 10000.0);
    tc.add_capacitor(n2, gnd, 100e-9);

    assert!(
        tc.solve_dc().is_ok(),
        "RC circuit should converge in DC"
    );
}

#[test]
fn dc_convergence_with_diode() {
    let mut tc = TestCircuit::new();
    let n1 = tc.node();
    let n2 = tc.node();
    let gnd = TestCircuit::gnd();

    tc.add_dc_source(n1, gnd, 3.0);
    tc.add_resistor(n1, n2, 470.0);
    tc.add_diode(n2, gnd, DiodeModel::_1N4007);

    assert!(
        tc.solve_dc().is_ok(),
        "Diode circuit should converge in DC"
    );
}
