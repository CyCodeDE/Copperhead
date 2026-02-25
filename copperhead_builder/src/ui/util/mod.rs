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

use std::path::PathBuf;

/// Formats values with SI prefixes.
///
/// - `threshold`: Values below this (but > 0) will use lower prefixes.
///   Set to 1.0 for strict Engineering notation. Set to 0.1 to allow "0.5 H".
/// - `precision`: Maximum number of decimal places.
pub fn format_si(values: &[(f64, &str)], threshold: f64, precision: usize) -> String {
    let mut result = Vec::new();

    for &(val, unit) in values {
        if val.abs() < f64::EPSILON {
            if unit.is_empty() {
                result.push("0".to_string());
            } else {
                result.push(format!("0 {}", unit));
            }
            continue;
        }

        let log_val = val.abs().log10();
        let mut degree = (log_val / 3.0).floor() as i32;

        if degree < 0 {
            let scaled_base = val.abs();
            if scaled_base >= threshold {
                degree = 0;
            }
        }

        let mut factor = 10f64.powi(degree * 3);
        let mut scaled_val = val / factor;

        let multiplier = 10f64.powi(precision as i32);
        let rounded = (scaled_val * multiplier).round() / multiplier;

        if rounded.abs() >= 1000.0 {
            degree += 1;
            scaled_val /= 1000.0;
        } else {
            scaled_val = rounded;
        }

        // 6. Get Prefix
        let prefix = match degree {
            -5 => "f", // femto
            -4 => "p", // pico
            -3 => "n", // nano
            -2 => "µ", // micro
            -1 => "m", // milli
            0 => "",   // base
            1 => "k",  // kilo
            2 => "M",  // Mega
            3 => "G",  // Giga
            4 => "T",  // Tera
            5 => "P",  // Peta
            _ => "?",  // Out of range (or handle with "e" notation)
        };

        let final_str = if prefix == "?" {
            format!("{:e} {}", val, unit)
        } else {
            let s = format!("{0:.1$}", scaled_val, precision);
            let s = s.trim_end_matches('0').trim_end_matches('.');

            if prefix.is_empty() && unit.is_empty() {
                format!("{}", s)
            } else {
                format!("{} {}{}", s, prefix, unit)
            }
        };

        result.push(final_str);
    }

    result.join(", ")
}

pub fn parse_si(input: &str) -> Option<f64> {
    let input = input.trim();
    if input.is_empty() {
        return None;
    }

    let end_of_number = input
        .find(|c: char| !c.is_ascii_digit() && c != '.' && c != '-' && c != '+')
        .unwrap_or(input.len());

    let (num_part, mut suffix_part) = input.split_at(end_of_number);

    if suffix_part.starts_with('e') || suffix_part.starts_with('E') {
        let next_char = suffix_part.chars().nth(1);
        if let Some(c) = next_char {
            if c.is_ascii_digit() || c == '-' || c == '+' {
                return input.parse::<f64>().ok();
            }
        }
    }

    let value = num_part.parse::<f64>().ok()?;

    suffix_part = suffix_part.trim();

    // Handle special "Meg" case (insensitive) for SPICE users
    if suffix_part.to_lowercase().starts_with("meg") {
        return Some(value * 1e6);
    }

    if suffix_part.is_empty() {
        return Some(value);
    }

    let prefix = suffix_part.chars().next().unwrap();

    let multiplier = match prefix {
        'f' => 1e-15,      // femto
        'p' => 1e-12,      // pico
        'n' => 1e-9,       // nano
        'u' | 'µ' => 1e-6, // micro (allow 'u' or mu)
        'm' => 1e-3,       // milli
        'k' | 'K' => 1e3,  // kilo (allow uppercase K for convenience)
        'M' => 1e6,        // Mega (Standard SI)
        'G' => 1e9,        // Giga
        'T' => 1e12,       // Tera
        // 'R' is often used in resistor values like "10R", effectively x1
        'R' | 'r' => 1.0,
        _ => 1.0, // Unknown prefix, assume unit (e.g. "10V" -> 10.0)
    };

    Some(value * multiplier)
}

pub fn format_si_single(val: f64, precision: usize) -> String {
    if val.abs() < 1e-24 {
        return "0".to_string();
    }

    let log_val = val.abs().log10();
    let degree = (log_val / 3.0).floor() as i32;

    // Clamp degree to supported prefixes
    let degree = degree.clamp(-5, 5);

    let factor = 10f64.powi(degree * 3);
    let scaled_val = val / factor;

    let prefix = match degree {
        -5 => "f",
        -4 => "p",
        -3 => "n",
        -2 => "µ",
        -1 => "m",
        0 => "",
        1 => "k",
        2 => "M",
        3 => "G",
        4 => "T",
        5 => "P",
        _ => "",
    };

    // Format with precision, trim trailing zeros/dot
    let s = format!("{0:.1$}", scaled_val, precision);
    let s = s.trim_end_matches('0').trim_end_matches('.').to_string();

    format!("{}{}", s, prefix)
}

/// Returns the default path for the application (usually in home or the documents folder)
pub fn get_default_path() -> PathBuf {
    dirs::document_dir()
        .expect("Could not find the documents directory")
        .join("Copperhead")
}

pub fn get_config_path() -> PathBuf {
    dirs::config_dir()
        .expect("Could not find the config directory")
        .join("copperhead")
}
