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

use crate::ui::app::CircuitApp;
use crate::ui::components::definitions::ground::GroundDef;
use crate::ui::components::definitions::label::LabelDef;
use crate::ui::{ComponentDef, SimCommand};
use crossbeam::channel::Sender;
use egui::{Color32, Painter, Pos2, Rect, Stroke, StrokeKind, Ui, Vec2};

pub mod audio_probe;
pub mod bjt;
pub mod capacitor;
pub mod diode;
pub mod ground;
pub mod inductor;
pub mod label;
pub mod pentode;
pub mod potentiometer;
pub mod resistor;
pub mod switch;
pub mod triode;
pub mod voltage_source;

pub trait ComponentUIExt {
    /// E.g., "R" for Resistor, "C" for Capacitor
    fn prefix(&self) -> &'static str;

    /// E.g., "Resistor", "DC Source"
    fn ui_name(&self) -> &'static str;

    /// An optional comment that gets shown under the component name and as the name in the property panel
    fn comment(&self) -> Option<String> {
        None
    }

    /// Width and height of the component
    fn size(&self) -> (isize, isize) {
        let pins = self.local_pins();

        // Find the min and max for both X and Y in a single pass
        let (min_x, max_x, min_y, max_y) = pins.iter().fold(
            (isize::MAX, isize::MIN, isize::MAX, isize::MIN),
            |(min_x, max_x, min_y, max_y), &(x, y)| {
                (min_x.min(x), max_x.max(x), min_y.min(y), max_y.max(y))
            },
        );

        // Calculate extents
        let width = max_x - min_x;
        let height = max_y - min_y;

        (width, height)
    }

    /// Tells the UI where the "center" of the component is relative to the local pin coordinates.
    fn offset(&self) -> (f32, f32) {
        (0., 0.)
    }

    /// The un-rotated local coordinates of the pins.
    /// Order MUST match the `nodes` slice passed to `instantiate()` in the core.
    fn local_pins(&self) -> Vec<(isize, isize)>;

    /// Handles the drawing for the properties.
    fn draw_property_panel(
        &mut self,
        tx: &Sender<SimCommand>,
        ui: &mut Ui,
        id: Option<usize>,
        running: bool,
        name: &str,
    ) {
    }

    /// Draws the properties in the right-click modal.
    /// Returns whether the component was modified (and thus the schematic should be marked dirty)
    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool;

    fn get_parameters(&self) -> Vec<ParameterDefinition> {
        vec![]
    }

    /// Draws the component on the schematic grid.
    fn draw_icon(
        &self,
        painter: &Painter,
        center: Pos2,
        rotation: u8,
        zoom: f32,
        fill_color: Color32,
        stroke_color: Color32,
    ) {
        let (base_w, base_h) = (2.0, 1.0);

        let (w, h) = if rotation % 2 == 1 {
            (base_h, base_w)
        } else {
            (base_w, base_h)
        };

        let size = Vec2::new(w * zoom, h * zoom);
        let rect = Rect::from_center_size(center, size);

        painter.rect_filled(rect, 4.0, fill_color);
        painter.rect_stroke(
            rect,
            4.0,
            Stroke::new(1.5, stroke_color),
            StrokeKind::Outside,
        );

        painter.text(
            center,
            egui::Align2::CENTER_CENTER,
            "?",
            egui::FontId::proportional(zoom * 0.5),
            Color32::WHITE,
        );
    }

    /// Draws all text labels associated with the component (names, values, pin identifiers, model name, etc.)
    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {}
}

macro_rules! delegate_ui_ext {
    ( $( $variant:ident ),* $(,)? ) => {
        impl ComponentUIExt for ComponentDef {
            fn prefix(&self) -> &'static str {
                match self {
                    $( ComponentDef::$variant(inner) => inner.prefix() ),*
                }
            }

            fn ui_name(&self) -> &'static str {
                match self {
                    $( ComponentDef::$variant(inner) => inner.ui_name() ),*
                }
            }

            fn size(&self) -> (isize, isize) {
                match self {
                    $( ComponentDef::$variant(inner) => inner.size() ),*
                }
            }

            fn offset(&self) -> (f32, f32) {
                match self {
                    $( ComponentDef::$variant(inner) => inner.offset() ),*
                }
            }

            fn local_pins(&self) -> Vec<(isize, isize)> {
                match self {
                    $( ComponentDef::$variant(inner) => inner.local_pins() ),*
                }
            }

            fn draw_property_panel(&mut self, tx: &Sender<SimCommand>, ui: &mut Ui, id: Option<usize>, running: bool, name: &str) {
                match self {
                    $( ComponentDef::$variant(inner) => inner.draw_property_panel(tx, ui, id, running, name) ),*
                }
            }

            fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
                match self {
                    $( ComponentDef::$variant(inner) => inner.draw_modal(app, ui) ),*
                }
            }

            fn draw_icon(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, fill_color: Color32, stroke_color: Color32) {
                match self {
                    $( ComponentDef::$variant(inner) => inner.draw_icon(painter, center, rotation, zoom, fill_color, stroke_color) ),*
                }
            }

            fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
                match self {
                    $( ComponentDef::$variant(inner) => inner.draw_labels(painter, center, rotation, zoom, name) ),*
                }
            }
        }
    };
}

delegate_ui_ext! {
    Resistor,
    Capacitor,
    Inductor,
    Diode,
    VoltageSource,
    Bjt,
    Triode,
    Pentode,
    Potentiometer,
    AudioProbe,
    Switch,
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum SchematicElement {
    /// A physics component that will go to the Core simulator
    Core(ComponentDef),
    /// A visual-only ground constraint
    Ground(GroundDef),
    /// A visual-only node connection constraint
    Label(LabelDef),
}

impl ComponentUIExt for SchematicElement {
    fn prefix(&self) -> &'static str {
        match self {
            Self::Core(c) => c.prefix(),
            Self::Ground(g) => g.prefix(),
            Self::Label(l) => l.prefix(),
        }
    }

    fn ui_name(&self) -> &'static str {
        match self {
            Self::Core(c) => c.ui_name(),
            Self::Ground(g) => g.ui_name(),
            Self::Label(l) => l.ui_name(),
        }
    }

    fn size(&self) -> (isize, isize) {
        match self {
            Self::Core(c) => c.size(),
            Self::Ground(g) => g.size(),
            Self::Label(l) => l.size(),
        }
    }

    fn offset(&self) -> (f32, f32) {
        match self {
            Self::Core(c) => c.offset(),
            Self::Ground(g) => g.offset(),
            Self::Label(l) => l.offset(),
        }
    }

    fn local_pins(&self) -> Vec<(isize, isize)> {
        match self {
            Self::Core(c) => c.local_pins(),
            Self::Ground(g) => g.local_pins(),
            Self::Label(l) => l.local_pins(),
        }
    }

    fn draw_property_panel(
        &mut self,
        tx: &Sender<SimCommand>,
        ui: &mut Ui,
        id: Option<usize>,
        running: bool,
        name: &str,
    ) {
        match self {
            Self::Core(c) => c.draw_property_panel(tx, ui, id, running, name),
            Self::Ground(g) => g.draw_property_panel(tx, ui, id, running, name),
            Self::Label(l) => l.draw_property_panel(tx, ui, id, running, name),
        }
    }

    fn draw_modal(&mut self, app: &mut CircuitApp, ui: &mut Ui) -> bool {
        match self {
            Self::Core(c) => c.draw_modal(app, ui),
            Self::Ground(g) => g.draw_modal(app, ui),
            Self::Label(l) => l.draw_modal(app, ui),
        }
    }

    fn draw_icon(
        &self,
        painter: &Painter,
        center: Pos2,
        rotation: u8,
        zoom: f32,
        fill_color: Color32,
        stroke_color: Color32,
    ) {
        match self {
            Self::Core(c) => c.draw_icon(painter, center, rotation, zoom, fill_color, stroke_color),
            Self::Ground(g) => {
                g.draw_icon(painter, center, rotation, zoom, fill_color, stroke_color)
            }
            Self::Label(l) => {
                l.draw_icon(painter, center, rotation, zoom, fill_color, stroke_color)
            }
        }
    }

    fn draw_labels(&self, painter: &Painter, center: Pos2, rotation: u8, zoom: f32, name: &str) {
        match self {
            Self::Core(c) => c.draw_labels(painter, center, rotation, zoom, name),
            Self::Ground(g) => g.draw_labels(painter, center, rotation, zoom, name),
            Self::Label(l) => l.draw_labels(painter, center, rotation, zoom, name),
        }
    }
}

impl SchematicElement {
    pub fn is_same_category(&self, other: &Self) -> bool {
        match (self, other) {
            // Special case: Differentiate VoltageSource types (AC vs DC vs AudioSource)
            (
                SchematicElement::Core(ComponentDef::VoltageSource(v1)),
                SchematicElement::Core(ComponentDef::VoltageSource(v2)),
            ) => std::mem::discriminant(&v1.source_type) == std::mem::discriminant(&v2.source_type),
            // General case: Compare Core components
            (SchematicElement::Core(c1), SchematicElement::Core(c2)) => {
                std::mem::discriminant(c1) == std::mem::discriminant(c2)
            }
            // Fallback for Ground, Label, etc.
            (a, b) => std::mem::discriminant(a) == std::mem::discriminant(b),
        }
    }
}

/// A parameter definition describes what type of live parameter can be added to a component, and how it should behave in the UI.
/// For example, a potentiometer might have a "resistance" parameter that is a floating-point number with a certain range.
/// Or a switch might have a state parameter that can be toggled between "open" and "closed".
pub struct ParameterDefinition {
    pub name: String,
    pub description: String,
    pub param_type: ParameterType,
    /// Whether the parameter is pinned to the panel
    pub pinned: bool,
}

pub enum ParameterType {
    Float { min: f64, max: f64, step: f64 },
    Integer { min: i64, max: i64, step: i64 },
    Boolean,
    Enum { options: Vec<String> },
}
