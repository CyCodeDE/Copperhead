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
use num_traits::Float;

/// Computes the exponential of `x`, linearly extrapolating if `x` exceeds a safe threshold.
/// Prevents floating-point overflow during large voltage swings in NR iterations.
#[inline]
pub fn exp_safe<T: CircuitScalar>(x: T) -> T {
    let max_arg = T::from(80.0).unwrap();

    if x > max_arg {
        let exp_max = max_arg.exp();
        exp_max * (T::one() + (x - max_arg))
    } else {
        x.exp()
    }
}

/// Computes both the safe exponential and its derivative.
/// Returns: a tuple of `(Value, Derivative)`.
#[inline]
pub fn exp_safe_deriv<T: CircuitScalar>(x: T) -> (T, T) {
    let max_arg = T::from(80.0).unwrap();

    if x > max_arg {
        let exp_max = max_arg.exp();
        (exp_max * (T::one() + (x - max_arg)), exp_max)
    } else {
        let ex = x.exp();
        (ex, ex)
    }
}

/// Computes the Softplus function and its derivative safely.
/// Softplus: f(x) = ln(1 + e^x)
/// Derivative (Sigmoid): f'(x) = 1 / (1 + e^{-x})
///
/// Uses a threshold to prevent floating-point overflow and underflow,
/// returning the linear asymptotes for large positive or negative inputs.
#[inline]
pub fn softplus_safe_deriv<T: Float>(x: T) -> (T, T) {
    let limit = T::from(20.0).unwrap();
    let zero = T::zero();
    let one = T::one();

    if x > limit {
        (x, one)
    } else if x < -limit {
        (zero, zero)
    } else {
        let val = (one + x.exp()).ln();
        let deriv = one / (one + (-x).exp());
        (val, deriv)
    }
}

/// Standard SPICE `pnjlim` algorithm for limiting junction voltage steps.
/// Dampens voltage changes across PN junctions to prevent exponential overflow
/// and oscillation during Newton-Raphson iterations.
pub fn pn_junction_limit<T: CircuitScalar>(v_new: T, v_old: T, vt: T, v_crit: T) -> T {
    if v_new > v_crit && (v_new - v_old).abs() > (vt + vt) {
        if v_old > T::zero() {
            let arg = (v_new - v_old) / vt;
            if arg > T::zero() {
                v_old + vt * (T::one() + arg).ln()
            } else {
                v_new
            }
        } else {
            vt * (v_new / vt).ln()
        }
    } else {
        v_new
    }
}
