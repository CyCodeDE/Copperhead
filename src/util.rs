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
