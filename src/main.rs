pub(crate) mod circuit;
pub(crate) mod components;
pub(crate) mod model;
mod signals;
mod simulation;
pub mod ui;
mod util;

use crate::circuit::Circuit;
use crate::components::ComponentDescriptor;
use crate::model::NodeId;
use crate::ui::app::CircuitApp;
use eframe::Frame;
use egui::{Context, ViewportBuilder};
use faer::prelude::default;
use std::f64::consts::PI;

fn main() -> eframe::Result {
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
