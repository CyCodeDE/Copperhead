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

    eframe::run_native(
        "Copperhead",
        native_options,
        Box::new(|cc| Ok(Box::new(CircuitApp::new(cc)))),
    )
}
