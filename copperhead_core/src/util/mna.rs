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
use crate::model::CircuitScalar;
use faer::{ColMut, ColRef, MatMut};

/// Helper to fetch a node voltage, defaulting to 0.0 if the node is Ground (None)
pub fn get_voltage<T: CircuitScalar>(voltages: &ColRef<T>, idx: Option<usize>) -> T {
    idx.map_or(T::zero(), |i| voltages[i])
}

/// Helper to calculate the voltage difference across a 2-terminal component (Va - Vb)
pub fn get_voltage_diff<T: CircuitScalar>(
    voltages: &ColRef<T>,
    idx_a: Option<usize>,
    idx_b: Option<usize>,
) -> T {
    get_voltage(voltages, idx_a) - get_voltage(voltages, idx_b)
}

/// Stamp a 2-terminal conductance (G) into the admittance matrix (Y-matrix)
pub fn stamp_conductance<T: CircuitScalar>(
    matrix: &mut MatMut<T>,
    idx_a: Option<usize>,
    idx_b: Option<usize>,
    g: T,
    offset: usize,
) {
    let t = |n: usize| n - offset;

    if let Some(i) = idx_a {
        matrix[(t(i), t(i))] += g;
    }
    if let Some(j) = idx_b {
        matrix[(t(j), t(j))] += g;
    }
    if let (Some(i), Some(j)) = (idx_a, idx_b) {
        matrix[(t(i), t(j))] -= g;
        matrix[(t(j), t(i))] -= g;
    }
}

/// Stamp a Voltage-Controlled Current Source (Transconductance / gm) into the matrix
pub fn stamp_transconductance<T: CircuitScalar>(
    matrix: &mut MatMut<T>,
    n_out_pos: Option<usize>,
    n_out_neg: Option<usize>,
    n_ctrl_pos: Option<usize>,
    n_ctrl_neg: Option<usize>,
    gm: T,
    offset: usize,
) {
    let t = |n: usize| n - offset;

    if let (Some(i), Some(j)) = (n_out_pos, n_ctrl_pos) {
        matrix[(t(i), t(j))] += gm;
    }
    if let (Some(i), Some(j)) = (n_out_pos, n_ctrl_neg) {
        matrix[(t(i), t(j))] -= gm;
    }
    if let (Some(i), Some(j)) = (n_out_neg, n_ctrl_pos) {
        matrix[(t(i), t(j))] -= gm;
    }
    if let (Some(i), Some(j)) = (n_out_neg, n_ctrl_neg) {
        matrix[(t(i), t(j))] += gm;
    }
}

/// Stamp an independent current source into the Right-Hand Side (RHS) vector
/// Current flows from node A to node B.
pub fn stamp_current_source<T: CircuitScalar>(
    rhs: &mut ColMut<T>,
    idx_a: Option<usize>,
    idx_b: Option<usize>,
    val: T,
    offset: usize,
) {
    let t = |n: usize| n - offset;

    if let Some(i) = idx_a {
        rhs[t(i)] -= val;
    }
    if let Some(j) = idx_b {
        rhs[t(j)] += val;
    }
}

/// Stamp a single value into the Matrix at (row, col)
pub fn stamp_matrix_element<T: CircuitScalar>(
    matrix: &mut MatMut<T>,
    row: Option<usize>,
    col: Option<usize>,
    val: T,
    offset: usize,
) {
    if let (Some(r), Some(c)) = (row, col) {
        matrix[(r - offset, c - offset)] += val;
    }
}

/// Stamp a single value into the RHS vector at (row)
pub fn stamp_vector_element<T: CircuitScalar>(
    rhs: &mut ColMut<T>,
    row: Option<usize>,
    val: T,
    offset: usize,
) {
    if let Some(r) = row {
        rhs[r - offset] += val;
    }
}
