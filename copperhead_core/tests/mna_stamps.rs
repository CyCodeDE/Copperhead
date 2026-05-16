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

//! Unit tests for the low-level MNA stamping functions in `util::mna`.
//!
//! These functions are the mathematical foundation of the solver — every
//! component eventually calls into them. We verify:
//!   - Correct matrix/vector entries for valid node indices
//!   - Ground (None) nodes are silently ignored
//!   - The `offset` parameter correctly remaps indices into sub-matrices
//!   - Repeated stamps accumulate (+= semantics)

mod common;

use copperhead_core::util::mna::{
    get_voltage, get_voltage_diff, stamp_conductance, stamp_current_source, stamp_matrix_element,
    stamp_transconductance, stamp_vector_element,
};
use faer::{Col, Mat};

// ---------------------------------------------------------------------------
// stamp_conductance
// ---------------------------------------------------------------------------

#[test]
fn conductance_both_nodes_valid() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_conductance(&mut m.as_mut(), Some(0), Some(1), 2.0, 0);

    assert_eq!(m[(0, 0)], 2.0, "A[a,a]");
    assert_eq!(m[(1, 1)], 2.0, "A[b,b]");
    assert_eq!(m[(0, 1)], -2.0, "A[a,b]");
    assert_eq!(m[(1, 0)], -2.0, "A[b,a]");
}

#[test]
fn conductance_node_b_is_ground() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_conductance(&mut m.as_mut(), Some(0), None, 3.0, 0);

    assert_eq!(m[(0, 0)], 3.0, "only diagonal for a");
    assert_eq!(m[(1, 1)], 0.0);
    assert_eq!(m[(0, 1)], 0.0);
    assert_eq!(m[(1, 0)], 0.0);
}

#[test]
fn conductance_node_a_is_ground() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_conductance(&mut m.as_mut(), None, Some(1), 3.0, 0);

    assert_eq!(m[(1, 1)], 3.0, "only diagonal for b");
    assert_eq!(m[(0, 0)], 0.0);
    assert_eq!(m[(0, 1)], 0.0);
    assert_eq!(m[(1, 0)], 0.0);
}

#[test]
fn conductance_both_nodes_ground_does_nothing() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_conductance(&mut m.as_mut(), None, None, 5.0, 0);

    // All entries remain zero — grounding both terminals is a no-op.
    for r in 0..2 {
        for c in 0..2 {
            assert_eq!(m[(r, c)], 0.0, "entry ({r},{c}) should be 0");
        }
    }
}

#[test]
fn conductance_accumulates_across_stamps() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_conductance(&mut m.as_mut(), Some(0), Some(1), 1.0, 0);
    stamp_conductance(&mut m.as_mut(), Some(0), Some(1), 2.0, 0);

    // Each cell receives the sum: 1 + 2 = 3
    assert_eq!(m[(0, 0)], 3.0);
    assert_eq!(m[(1, 1)], 3.0);
    assert_eq!(m[(0, 1)], -3.0);
    assert_eq!(m[(1, 0)], -3.0);
}

#[test]
fn conductance_offset_remaps_indices() {
    // Global indices 2 and 3, offset 2 → local rows/cols 0 and 1 in a 2×2 matrix.
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_conductance(&mut m.as_mut(), Some(2), Some(3), 1.0, 2);

    assert_eq!(m[(0, 0)], 1.0);
    assert_eq!(m[(1, 1)], 1.0);
    assert_eq!(m[(0, 1)], -1.0);
    assert_eq!(m[(1, 0)], -1.0);
}

// ---------------------------------------------------------------------------
// stamp_transconductance
// ---------------------------------------------------------------------------

#[test]
fn transconductance_all_four_nodes_valid() {
    // gm source: I_out = gm * V_ctrl
    // out_pos=0, out_neg=1, ctrl_pos=0, ctrl_neg=1, gm=0.5
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_transconductance(&mut m.as_mut(), Some(0), Some(1), Some(0), Some(1), 0.5, 0);

    // m[out_pos, ctrl_pos] += gm → m[0,0] += 0.5
    // m[out_pos, ctrl_neg] -= gm → m[0,1] -= 0.5
    // m[out_neg, ctrl_pos] -= gm → m[1,0] -= 0.5
    // m[out_neg, ctrl_neg] += gm → m[1,1] += 0.5
    assert_eq!(m[(0, 0)], 0.5, "m[out_pos, ctrl_pos]");
    assert_eq!(m[(0, 1)], -0.5, "m[out_pos, ctrl_neg]");
    assert_eq!(m[(1, 0)], -0.5, "m[out_neg, ctrl_pos]");
    assert_eq!(m[(1, 1)], 0.5, "m[out_neg, ctrl_neg]");
}

#[test]
fn transconductance_single_ended_output_and_ctrl() {
    // out_pos=0 only, ctrl_pos=1 only — stamps a single entry
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_transconductance(&mut m.as_mut(), Some(0), None, Some(1), None, 2.0, 0);

    assert_eq!(m[(0, 1)], 2.0, "m[out_pos, ctrl_pos]");
    assert_eq!(m[(0, 0)], 0.0);
    assert_eq!(m[(1, 0)], 0.0);
    assert_eq!(m[(1, 1)], 0.0);
}

#[test]
fn transconductance_grounded_output_only() {
    // out_neg=1 only (out_pos=None, ctrl_pos=0)
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_transconductance(&mut m.as_mut(), None, Some(1), Some(0), None, 3.0, 0);

    // m[out_neg, ctrl_pos] -= gm → m[1,0] -= 3.0
    assert_eq!(m[(1, 0)], -3.0);
    assert_eq!(m[(0, 0)], 0.0);
    assert_eq!(m[(0, 1)], 0.0);
    assert_eq!(m[(1, 1)], 0.0);
}

#[test]
fn transconductance_all_ground_does_nothing() {
    let mut m: Mat<f64> = Mat::zeros(3, 3);
    stamp_transconductance(&mut m.as_mut(), None, None, None, None, 99.0, 0);

    for r in 0..3 {
        for c in 0..3 {
            assert_eq!(m[(r, c)], 0.0);
        }
    }
}

#[test]
fn transconductance_with_offset() {
    // Global indices: out_pos=2, ctrl_pos=3, offset=2 → local 0 and 1
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_transconductance(&mut m.as_mut(), Some(2), None, Some(3), None, 1.5, 2);

    assert_eq!(m[(0, 1)], 1.5);
}

// ---------------------------------------------------------------------------
// stamp_current_source
// ---------------------------------------------------------------------------

/// Current flows *from* idx_a *to* idx_b in the external circuit:
///   rhs[a] -= val  (KCL: current leaves node a)
///   rhs[b] += val  (KCL: current enters node b)

#[test]
fn current_source_both_nodes_valid() {
    let mut rhs: Col<f64> = Col::zeros(2);
    stamp_current_source(&mut rhs.as_mut(), Some(0), Some(1), 5.0, 0);

    assert_eq!(rhs[0], -5.0, "rhs[a] -= val");
    assert_eq!(rhs[1], 5.0, "rhs[b] += val");
}

#[test]
fn current_source_ground_b() {
    let mut rhs: Col<f64> = Col::zeros(2);
    stamp_current_source(&mut rhs.as_mut(), Some(0), None, 3.0, 0);

    assert_eq!(rhs[0], -3.0);
    assert_eq!(rhs[1], 0.0);
}

#[test]
fn current_source_ground_a() {
    let mut rhs: Col<f64> = Col::zeros(2);
    stamp_current_source(&mut rhs.as_mut(), None, Some(1), 3.0, 0);

    assert_eq!(rhs[0], 0.0);
    assert_eq!(rhs[1], 3.0);
}

#[test]
fn current_source_both_ground_does_nothing() {
    let mut rhs: Col<f64> = Col::zeros(2);
    stamp_current_source(&mut rhs.as_mut(), None, None, 99.0, 0);

    assert_eq!(rhs[0], 0.0);
    assert_eq!(rhs[1], 0.0);
}

#[test]
fn current_source_accumulates() {
    let mut rhs: Col<f64> = Col::zeros(2);
    stamp_current_source(&mut rhs.as_mut(), Some(0), Some(1), 2.0, 0);
    stamp_current_source(&mut rhs.as_mut(), Some(0), Some(1), 3.0, 0);

    assert_eq!(rhs[0], -5.0);
    assert_eq!(rhs[1], 5.0);
}

#[test]
fn current_source_with_offset() {
    // Global idx 2 and 3, offset 2 → local 0 and 1
    let mut rhs: Col<f64> = Col::zeros(3);
    stamp_current_source(&mut rhs.as_mut(), Some(2), Some(3), 1.0, 2);

    assert_eq!(rhs[0], -1.0);
    assert_eq!(rhs[1], 1.0);
    assert_eq!(rhs[2], 0.0);
}

// ---------------------------------------------------------------------------
// stamp_matrix_element
// ---------------------------------------------------------------------------

#[test]
fn matrix_element_basic() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_matrix_element(&mut m.as_mut(), Some(0), Some(1), 7.0, 0);

    assert_eq!(m[(0, 1)], 7.0);
    assert_eq!(m[(0, 0)], 0.0);
    assert_eq!(m[(1, 0)], 0.0);
    assert_eq!(m[(1, 1)], 0.0);
}

#[test]
fn matrix_element_none_row_does_nothing() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_matrix_element(&mut m.as_mut(), None, Some(0), 1.0, 0);
    stamp_matrix_element(&mut m.as_mut(), Some(0), None, 1.0, 0);
    stamp_matrix_element(&mut m.as_mut(), None, None, 1.0, 0);

    for r in 0..2 {
        for c in 0..2 {
            assert_eq!(m[(r, c)], 0.0);
        }
    }
}

#[test]
fn matrix_element_accumulates() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_matrix_element(&mut m.as_mut(), Some(0), Some(0), 3.0, 0);
    stamp_matrix_element(&mut m.as_mut(), Some(0), Some(0), 4.0, 0);

    assert_eq!(m[(0, 0)], 7.0);
}

#[test]
fn matrix_element_with_offset() {
    let mut m: Mat<f64> = Mat::zeros(2, 2);
    stamp_matrix_element(&mut m.as_mut(), Some(3), Some(4), 9.0, 3);

    assert_eq!(m[(0, 1)], 9.0);
}

// ---------------------------------------------------------------------------
// stamp_vector_element
// ---------------------------------------------------------------------------

#[test]
fn vector_element_basic() {
    let mut v: Col<f64> = Col::zeros(3);
    stamp_vector_element(&mut v.as_mut(), Some(1), 4.0, 0);

    assert_eq!(v[0], 0.0);
    assert_eq!(v[1], 4.0);
    assert_eq!(v[2], 0.0);
}

#[test]
fn vector_element_none_does_nothing() {
    let mut v: Col<f64> = Col::zeros(2);
    stamp_vector_element(&mut v.as_mut(), None, 99.0, 0);

    assert_eq!(v[0], 0.0);
    assert_eq!(v[1], 0.0);
}

#[test]
fn vector_element_accumulates() {
    let mut v: Col<f64> = Col::zeros(2);
    stamp_vector_element(&mut v.as_mut(), Some(0), 2.0, 0);
    stamp_vector_element(&mut v.as_mut(), Some(0), 3.0, 0);

    assert_eq!(v[0], 5.0);
}

#[test]
fn vector_element_with_offset() {
    // Global row 2, offset 2 → local row 0
    let mut v: Col<f64> = Col::zeros(3);
    stamp_vector_element(&mut v.as_mut(), Some(2), 1.5, 2);

    assert_eq!(v[0], 1.5);
    assert_eq!(v[1], 0.0);
    assert_eq!(v[2], 0.0);
}

// ---------------------------------------------------------------------------
// get_voltage / get_voltage_diff
// ---------------------------------------------------------------------------

#[test]
fn get_voltage_returns_indexed_value() {
    let v = Col::<f64>::from_fn(3, |i| (i + 1) as f64); // [1.0, 2.0, 3.0]
    let vref = v.as_ref();

    assert_eq!(get_voltage(&vref, Some(0)), 1.0);
    assert_eq!(get_voltage(&vref, Some(1)), 2.0);
    assert_eq!(get_voltage(&vref, Some(2)), 3.0);
}

#[test]
fn get_voltage_returns_zero_for_ground() {
    let v = Col::<f64>::from_fn(3, |i| (i + 1) as f64);
    let vref = v.as_ref();

    assert_eq!(get_voltage(&vref, None), 0.0);
}

#[test]
fn get_voltage_diff_both_valid() {
    let v = Col::<f64>::from_fn(2, |i| if i == 0 { 10.0 } else { 3.0 }); // [10, 3]
    let vref = v.as_ref();

    assert_eq!(get_voltage_diff(&vref, Some(0), Some(1)), 7.0);
    assert_eq!(get_voltage_diff(&vref, Some(1), Some(0)), -7.0);
}

#[test]
fn get_voltage_diff_ground_b() {
    let v = Col::<f64>::from_fn(2, |i| if i == 0 { 5.0 } else { 0.0 });
    let vref = v.as_ref();

    assert_eq!(get_voltage_diff(&vref, Some(0), None), 5.0);
}

#[test]
fn get_voltage_diff_ground_a() {
    let v = Col::<f64>::from_fn(2, |i| if i == 0 { 0.0 } else { 4.0 });
    let vref = v.as_ref();

    assert_eq!(get_voltage_diff(&vref, None, Some(1)), -4.0);
}

#[test]
fn get_voltage_diff_both_ground() {
    let v = Col::<f64>::from_fn(2, |_| 99.0);
    let vref = v.as_ref();

    assert_eq!(get_voltage_diff(&vref, None, None), 0.0);
}
