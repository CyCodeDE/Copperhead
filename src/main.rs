#![feature(path_is_empty)]
/*
 * Copyright (c) 2026-2026 CyCode and the Copperhead contributors
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

mod audio;
pub(crate) mod circuit;
pub(crate) mod components;
pub(crate) mod model;
mod signals;
mod simulation;
pub mod ui;
mod util;

use crate::ui::app::CircuitApp;
use egui::ViewportBuilder;

fn main() -> eframe::Result {
    env_logger::init();

    let native_options = eframe::NativeOptions {
        viewport: ViewportBuilder::default(),
        ..Default::default()
    };

    // Before start, check if default project path and config folder exist
    // If not, create them
    let default_path = util::get_default_path();
    if !default_path.exists() {
        std::fs::create_dir_all(&default_path).expect("Could not create default project directory");
    }
    let config_path = util::get_config_path();
    if !config_path.exists() {
        std::fs::create_dir_all(&config_path).expect("Could not create config directory");
    }

    eframe::run_native(
        "Copperhead",
        native_options,
        Box::new(|cc| Ok(Box::new(CircuitApp::new(cc)))),
    )
}
