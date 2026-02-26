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

use crate::simulation::run_simulation_loop;
use crate::ui::components::oscilloscope::ScopeState;
use crate::ui::netlist::compile_netlist;
use crate::ui::{
    CircuitMetadata, ComponentBuildData, GridPos, Netlist, Schematic, SimCommand, SimState,
    VisualWire,
};
use copperhead_core::descriptor::ComponentDescriptor;
use copperhead_core::model::{NodeId, SimBatchData};
use crossbeam::channel::{Receiver, Sender, unbounded};
use egui::style::{Selection, WidgetVisuals, Widgets};
use egui::{Color32, CornerRadius, Pos2, Stroke, TextStyle, Vec2, ViewportCommand, Visuals};
use serde::Serialize;
use std::collections::{HashMap, HashSet, VecDeque};
use std::path::PathBuf;

pub struct AppTheme {
    pub background: Color32,
    pub dot_color: Color32,
    pub panel_color: Color32,
    pub panel_border: Color32,
    pub modal_surface: Color32,
    pub text_color: Color32,
    pub secondary_text_color: Color32,
    pub disabled_text_color: Color32,
    pub modal_backdrop: Color32,
    pub selection_color: Color32,
    pub primary: Color32,
    pub primary_hover: Color32,
    pub error: Color32,
    pub success: Color32,
    pub warning: Color32,
    pub info: Color32,
    pub floating: Color32,
    pub wire_off: Color32,
    pub wire_on: Color32,
    pub component_body: Color32,
    pub inactive: Color32,
    pub hover: Color32,
    pub active: Color32,
    pub inactive_stroke: Color32,
    pub hover_stroke: Color32,
}

impl AppTheme {
    pub fn default_dark() -> Self {
        Self {
            background: Color32::from_hex("#111116").unwrap(),
            dot_color: Color32::from_hex("#232329").unwrap(),
            panel_color: Color32::from_hex("#1A1A1F").unwrap(),
            panel_border: Color32::from_hex("#2D2D35").unwrap(),
            modal_surface: Color32::from_hex("#212126").unwrap(),
            text_color: Color32::from_hex("#ebe4d6").unwrap(),
            secondary_text_color: Color32::from_hex("#8E8E98").unwrap(),
            disabled_text_color: Color32::from_hex("#54555B").unwrap(),
            modal_backdrop: Color32::from_rgba_unmultiplied(0, 0, 0, 128),
            selection_color: Color32::from_hex("#345197").unwrap(),
            primary: Color32::from_hex("#EF852E").unwrap(),
            primary_hover: Color32::from_hex("#FF9845").unwrap(),
            error: Color32::from_hex("#F13B2E").unwrap(),
            success: Color32::from_hex("#54B85B").unwrap(),
            warning: Color32::from_hex("#DCA400").unwrap(),
            info: Color32::from_hex("#2098DB").unwrap(),
            floating: Color32::from_hex("#8C6EBD").unwrap(),
            wire_off: Color32::from_hex("#464652").unwrap(),
            wire_on: Color32::from_hex("#EF852E").unwrap(),
            component_body: Color32::from_hex("#D6D7DE").unwrap(),
            inactive: Color32::from_hex("#47372c").unwrap(),
            hover: Color32::from_hex("#5f4738").unwrap(),
            active: Color32::from_hex("#3a2a1f").unwrap(),
            inactive_stroke: Color32::from_hex("#675040").unwrap(),
            hover_stroke: Color32::from_hex("#896a56").unwrap(),
        }
    }
}

pub struct UndoStack<T> {
    undo_queue: VecDeque<T>,
    redo_queue: VecDeque<T>,
    max_history: usize,
}

#[derive(Clone)]
pub struct ProjectState {
    pub schematic: Schematic,
    pub simulation_time: f64,
}

impl<T: Clone> UndoStack<T> {
    pub fn new(max_history: usize) -> Self {
        Self {
            undo_queue: VecDeque::new(),
            redo_queue: VecDeque::new(),
            max_history,
        }
    }

    /// Saves the state before making a change
    pub fn push(&mut self, state: T) {
        self.undo_queue.push_back(state);
        if self.undo_queue.len() > self.max_history {
            self.undo_queue.pop_front();
        }

        self.redo_queue.clear();
    }

    /// Returns the state to restore, or None if empty
    pub fn undo(&mut self, current_state: T) -> Option<T> {
        if let Some(prev_state) = self.undo_queue.pop_back() {
            self.redo_queue.push_back(current_state);
            Some(prev_state)
        } else {
            None
        }
    }

    /// Returns the state to restore, or None if empty
    pub fn redo(&mut self, current_state: T) -> Option<T> {
        if let Some(next_state) = self.redo_queue.pop_back() {
            self.undo_queue.push_back(current_state);
            Some(next_state)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.undo_queue.clear();
        self.redo_queue.clear();
    }
}

#[derive(Clone)]
pub enum Tool {
    Select,
    PlaceComponent(ComponentBuildData),
    PlaceWire(Option<GridPos>), // Optionally stores the starting point. None means waiting for 1st click.
    Erase,
}

pub struct CircuitApp {
    pub state: ProjectState,
    //pub schematic: Schematic,
    pub undo_stack: UndoStack<ProjectState>,
    pub selected_tool: Tool,
    pub pan: Vec2,
    pub zoom: f32,
    /// tracks rotation for the tool being placed
    pub current_rotation: u8,
    pub editing_component_id: Option<usize>,
    pub plotting_node_voltage: Option<NodeId>,
    pub plotting_component_current: Option<(usize, usize)>, // (component_idx, pin_idx)
    pub plotting_observable: Option<(usize, usize)>,        // (component_idx, observable_idx)
    //pub simulation_time: f64, // in seconds, for how long to simulate
    pub scope_state: ScopeState,
    pub realtime_mode: bool,
    pub tx_command: Sender<SimCommand>,
    //pub shared_state: Arc<RwLock<SimState>>,
    pub sim_state: SimState,
    pub state_receiver: Receiver<StateUpdate>,
    pub recycle_tx: Sender<SimBatchData>,

    pub temp_state_snapshot: Option<ProjectState>,
    pub active_netlist: Option<Netlist>,

    pub is_initialized: bool,
    pub theme: AppTheme,
    pub keybinds_locked: bool,
    pub file_receiver: Receiver<Option<PathBuf>>,
    pub file_sender: Sender<Option<PathBuf>>,
    pub file_dialog_state: FileDialogState,
    pub current_file: Option<PathBuf>,
    pub temp_audio_path: Option<PathBuf>,
    pub wire_color_cache: HashMap<NodeId, Color32>,
}

pub enum StateUpdate {
    CircuitLoaded(CircuitMetadata),
    SendHistory(SimBatchData, usize),
    UpdateRunning(bool),
    ClearHistory,
}

#[derive(PartialEq)]
pub enum FileDialogState {
    SaveSchem,
    LoadSchem,
    LoadAudio,
    SaveAudio,
    SaveNetlist,
    Closed,
}

impl CircuitApp {
    pub fn new(cc: &eframe::CreationContext) -> Self {
        // Create channels and shared state
        let (tx, rx) = unbounded();
        let sim_state = SimState {
            history: Vec::new(),
            running: false,
            current_sample: 0,
            lookup_map: HashMap::new(),
            metadata: None,
        };

        // Spawn simulation thread
        let (state_sender, state_receiver) = unbounded();
        let (recycle_tx, recycle_rx) = unbounded::<SimBatchData>();

        std::thread::spawn(move || {
            run_simulation_loop(rx, state_sender, recycle_rx);
        });

        let theme = AppTheme::default_dark();

        cc.egui_ctx.style_mut(|style| {
            style.visuals = Visuals {
                dark_mode: true,
                window_fill: theme.background,
                panel_fill: theme.panel_color,
                widgets: Widgets {
                    noninteractive: WidgetVisuals {
                        bg_stroke: Stroke::NONE,
                        corner_radius: CornerRadius::ZERO,
                        bg_fill: theme.primary,
                        expansion: 0.0,
                        fg_stroke: Stroke::new(1.0, theme.text_color),
                        weak_bg_fill: Color32::TRANSPARENT,
                    },
                    inactive: WidgetVisuals {
                        bg_stroke: Stroke::new(1.0, theme.inactive_stroke),
                        corner_radius: CornerRadius::same(4),
                        bg_fill: theme.inactive,
                        expansion: 0.0,
                        fg_stroke: Stroke::new(1.0, theme.text_color),
                        weak_bg_fill: theme.inactive,
                    },
                    hovered: WidgetVisuals {
                        bg_stroke: Stroke::new(1.0, theme.hover_stroke),
                        corner_radius: CornerRadius::same(4),
                        bg_fill: theme.hover,
                        expansion: 0.0,
                        fg_stroke: Stroke::new(1.0, theme.text_color),
                        weak_bg_fill: theme.hover,
                    },
                    active: WidgetVisuals {
                        bg_stroke: Stroke::NONE,
                        corner_radius: CornerRadius::same(4),
                        bg_fill: theme.active,
                        expansion: 0.0,
                        fg_stroke: Stroke::new(1.0, theme.text_color),
                        weak_bg_fill: theme.active,
                    },

                    ..Default::default()
                },
                selection: Selection {
                    bg_fill: theme.active,
                    stroke: Stroke::new(1.0, theme.text_color),
                },

                ..Default::default()
            };

            style.text_styles.get_mut(&TextStyle::Button).unwrap().size = 13.0;
            style.text_styles.get_mut(&TextStyle::Body).unwrap().size = 13.0;

            style.spacing.button_padding = Vec2::new(8.0, 4.0);
            style.spacing.item_spacing = Vec2::new(4.0, 4.0);
        });

        cc.egui_ctx
            .send_viewport_cmd(ViewportCommand::Title("Copperhead - Untitled".to_string()));

        let (file_sender, file_receiver) = unbounded();
        Self {
            state: ProjectState {
                schematic: Schematic::default(),
                simulation_time: 1.0,
            },
            temp_state_snapshot: None,
            //schematic: Schematic::default(),
            undo_stack: UndoStack::new(50),
            selected_tool: Tool::Select,
            pan: Vec2::ZERO,
            is_initialized: false,
            zoom: 30.0, // pixels per grid unit
            current_rotation: 0,
            editing_component_id: None,
            plotting_node_voltage: None,
            plotting_component_current: None,
            plotting_observable: None,
            scope_state: ScopeState::default(),
            realtime_mode: false,
            theme,
            //simulation_time: -1.0,
            tx_command: tx,
            sim_state: sim_state,
            state_receiver,
            recycle_tx,
            active_netlist: None,

            keybinds_locked: false, // to prevent keybinds when typing in text fields
            file_receiver,
            file_sender,
            file_dialog_state: FileDialogState::Closed,
            current_file: None,
            temp_audio_path: None,
            wire_color_cache: HashMap::new(),
        }
    }

    pub fn save_to_path(&self, path: PathBuf) {
        let mut centered_schematic = self.state.schematic.clone();
        centered_schematic.recenter_schematic();
        let serialized = serde_json::to_string(&centered_schematic).unwrap();
        let real_time = self.realtime_mode;
        let sim_time = self.state.simulation_time;
        let save_data = (serialized, real_time, sim_time);
        std::fs::write(path, serde_json::to_string(&save_data).unwrap()).unwrap();
    }

    pub fn load_from_path(&mut self, path: PathBuf) {
        let data = std::fs::read_to_string(&path).unwrap();
        let (serialized, real_time, sim_time): (String, bool, f64) =
            serde_json::from_str(&data).unwrap();
        let schematic: Schematic = serde_json::from_str(&serialized).unwrap();
        self.state.schematic = schematic;
        self.realtime_mode = real_time;
        self.state.simulation_time = sim_time;
        self.is_initialized = false; // force re-initialization
        self.zoom = 30.0;
        self.selected_tool = Tool::Select;
        self.current_file = Some(path);
    }

    pub fn save_netlist(&self, path: PathBuf) {
        #[derive(Serialize)]
        struct ShortenedNetlist {
            pub instructions: Vec<ComponentDescriptor>,
        }

        let netlist = compile_netlist(&self);
        let shortened = ShortenedNetlist {
            instructions: netlist.instructions,
        };

        let serialized = serde_json::to_string(&shortened).unwrap();
        std::fs::write(path, serialized).unwrap();
    }

    /// Converts a Grid Position (logical) to Screen Position (pixels)
    pub(crate) fn to_screen(&self, pos: GridPos) -> Pos2 {
        let world_x = pos.x as f32 * self.zoom;
        let world_y = pos.y as f32 * self.zoom;
        Pos2::new(world_x, world_y) + Vec2::from(self.pan)
    }

    /// Converts a Screen Position (pixels) to the nearest Grid Position (logical)
    pub(crate) fn to_grid(&self, pos: Pos2) -> GridPos {
        let local = pos - Vec2::from(self.pan);
        let gx = (local.x / self.zoom).round() as i32;
        let gy = (local.y / self.zoom).round() as i32;
        GridPos {
            x: gx as isize,
            y: gy as isize,
        }
    }

    /// Helper to transform raw float coordinates
    fn world_to_screen(&self, x: f32, y: f32) -> Pos2 {
        Pos2::new(x * self.zoom, y * self.zoom) + Vec2::from(self.pan)
    }

    pub fn get_normalized_wires(&self) -> Vec<VisualWire> {
        let mut final_wires = self.state.schematic.wires.clone();

        // Collect all "interesting" points that must create nodes.
        // These are component pins and endpoints of all wires.
        let mut points_of_interest: HashSet<GridPos> = HashSet::new();

        for comp in &self.state.schematic.components {
            for pin in comp.get_pin_locations() {
                points_of_interest.insert(pin);
            }
        }

        // We also need wire endpoints to split other wires
        for wire in &self.state.schematic.wires {
            points_of_interest.insert(wire.start);
            points_of_interest.insert(wire.end);
        }

        // Iterative splitting
        // We keep looping until no splits happen (to handle cases where a split creates new points)
        let mut changed = true;
        while changed {
            changed = false;
            let mut new_wire_list = Vec::new();

            for wire in &final_wires {
                // Find a point that splits this wire
                let mut split_point: Option<GridPos> = None;

                for &pt in &points_of_interest {
                    // If point is on the wire, but is not the start or end
                    if wire.contains(pt) && pt != wire.start && pt != wire.end {
                        split_point = Some(pt);
                        break;
                    }
                }

                if let Some(pt) = split_point {
                    // Split the wire
                    new_wire_list.push(VisualWire {
                        start: wire.start,
                        end: pt,
                    });
                    new_wire_list.push(VisualWire {
                        start: pt,
                        end: wire.end,
                    });
                    changed = true;
                } else {
                    new_wire_list.push(wire.clone());
                }
            }
            final_wires = new_wire_list;
        }

        final_wires
    }

    /// Checks if a screen position (in pixels) is hovering over a wire.
    /// Returns the NodeId of the wire if found within the given tolerance (in pixels).
    /// The tolerance should match or be slightly larger than the wire stroke width (e.g., 2.0-4.0).
    pub fn wire_at_screen_pos(&self, screen_pos: Pos2, tolerance: f32) -> Option<NodeId> {
        for wire in &self.state.schematic.wires {
            let start = self.to_screen(wire.start);
            let end = self.to_screen(wire.end);

            let distance = point_to_segment_distance(screen_pos, start, end);

            if distance <= tolerance {
                // Found a wire under the cursor, look up its NodeId
                if let Some(netlist) = &self.active_netlist {
                    // Both start and end belong to the same node, so we can use either
                    if let Some(&node_id) = netlist.node_map.get(&wire.start) {
                        return Some(node_id);
                    }
                    if let Some(&node_id) = netlist.node_map.get(&wire.end) {
                        return Some(node_id);
                    }
                }
                // Wire found but no netlist available or node not mapped
                return None;
            }
        }
        None
    }
}

/// Calculates the shortest distance from a point to a line segment.
fn point_to_segment_distance(point: Pos2, seg_start: Pos2, seg_end: Pos2) -> f32 {
    let v = seg_end - seg_start;
    let w = point - seg_start;

    let c1 = w.dot(v);
    if c1 <= 0.0 {
        // Point is before the segment start
        return point.distance(seg_start);
    }

    let c2 = v.dot(v);
    if c2 <= c1 {
        // Point is after the segment end
        return point.distance(seg_end);
    }

    // Point projects onto the segment
    let t = c1 / c2;
    let projection = seg_start + v * t;
    point.distance(projection)
}
