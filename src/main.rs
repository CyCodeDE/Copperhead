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
