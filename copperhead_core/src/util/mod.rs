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
pub mod math;
pub mod mna;

use serde::Deserializer;

/// Parses a number with an optional SI engineering prefix (e.g. "1k" → 1000.0, "10n" → 1e-8).
///
/// Supported prefixes: f (femto), p (pico), n (nano), u/µ (micro), m (milli),
/// k/K (kilo), M (mega), G (giga), T (tera). Also handles the SPICE "Meg" convention
/// and "R"/"r" (×1 — common in resistor values like "10R").
///
/// Returns `None` if the string cannot be parsed as a number with optional prefix.
pub fn parse_si(input: &str) -> Option<f64> {
    let input = input.trim();
    if input.is_empty() {
        return None;
    }

    let end_of_number = input
        .find(|c: char| !c.is_ascii_digit() && c != '.' && c != '-' && c != '+')
        .unwrap_or(input.len());

    let (num_part, mut suffix_part) = input.split_at(end_of_number);

    // Preserve standard scientific notation like "1e-6" or "1E3"
    if suffix_part.starts_with('e') || suffix_part.starts_with('E') {
        if let Some(c) = suffix_part.chars().nth(1) {
            if c.is_ascii_digit() || c == '-' || c == '+' {
                return input.parse::<f64>().ok();
            }
        }
    }

    let value = num_part.parse::<f64>().ok()?;
    suffix_part = suffix_part.trim();

    if suffix_part.to_lowercase().starts_with("meg") {
        return Some(value * 1e6);
    }

    if suffix_part.is_empty() {
        return Some(value);
    }

    let multiplier = match suffix_part.chars().next().unwrap() {
        'f' => 1e-15,
        'p' => 1e-12,
        'n' => 1e-9,
        'u' | 'µ' => 1e-6,
        'm' => 1e-3,
        'k' | 'K' => 1e3,
        'M' => 1e6,
        'G' => 1e9,
        'T' => 1e12,
        'R' | 'r' => 1.0, // "10R" convention
        _ => 1.0,         // unknown unit suffix — treat as ×1
    };

    Some(value * multiplier)
}

/// Deserializes a JSON number or a JSON string into `String`.
/// Allows `*Def` fields to be stored as strings in new saves while still
/// loading legacy saves where they were plain JSON numbers.
pub fn deserialize_number_or_string<'de, D: Deserializer<'de>>(d: D) -> Result<String, D::Error> {
    use serde::de::{self, Visitor};
    use std::fmt;

    struct V;
    impl<'de> Visitor<'de> for V {
        type Value = String;
        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "a number or a string")
        }
        fn visit_f64<E: de::Error>(self, v: f64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_u64<E: de::Error>(self, v: u64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_i64<E: de::Error>(self, v: i64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_str<E: de::Error>(self, v: &str) -> Result<String, E> {
            Ok(v.to_owned())
        }
        fn visit_string<E: de::Error>(self, v: String) -> Result<String, E> {
            Ok(v)
        }
    }
    d.deserialize_any(V)
}

/// Deserializes a JSON boolean, number, or string into `String`.
/// Maps `true` → `"1"`, `false` → `"0"`, numbers as-is, strings as-is.
pub fn deserialize_bool_or_string<'de, D: Deserializer<'de>>(d: D) -> Result<String, D::Error> {
    use serde::de::{self, Visitor};
    use std::fmt;

    struct V;
    impl<'de> Visitor<'de> for V {
        type Value = String;
        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "a boolean, number, or string")
        }
        fn visit_bool<E: de::Error>(self, v: bool) -> Result<String, E> {
            Ok(if v { "1".to_string() } else { "0".to_string() })
        }
        fn visit_f64<E: de::Error>(self, v: f64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_u64<E: de::Error>(self, v: u64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_i64<E: de::Error>(self, v: i64) -> Result<String, E> {
            Ok(v.to_string())
        }
        fn visit_str<E: de::Error>(self, v: &str) -> Result<String, E> {
            Ok(v.to_owned())
        }
        fn visit_string<E: de::Error>(self, v: String) -> Result<String, E> {
            Ok(v)
        }
    }
    d.deserialize_any(V)
}
