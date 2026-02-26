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
use crate::ui::app::{CircuitApp, FileDialogState, StateUpdate, Tool};
use crate::ui::{SimStepData, handle_circuit_loaded};
use egui::{Key, Modifiers, ViewportCommand};
use log::debug;

impl CircuitApp {
    pub fn process_messages(&mut self, ctx: &egui::Context) {
        while let Ok(msg) = self.state_receiver.try_recv() {
            match msg {
                StateUpdate::UpdateRunning(is_running) => {
                    self.sim_state.running = is_running;
                }
                StateUpdate::SendHistory(mut batch, current_sample) => {
                    let nodes = batch.nodes_per_step;
                    let terms = batch.terminals_per_step;
                    let obs = batch.observables_per_step;

                    // Reconstruct SimStepData
                    for i in 0..batch.times.len() {
                        let step = SimStepData {
                            time: batch.times[i],
                            voltages: batch.voltages[i * nodes..(i + 1) * nodes].to_vec(),
                            currents: batch.currents[i * terms..(i + 1) * terms].to_vec(),
                            observables: batch.observables[i * obs..(i + 1) * obs].to_vec(),
                        };
                        self.sim_state.history.push(step);
                    }

                    self.sim_state.current_sample = current_sample;

                    batch.clear_for_reuse();
                    let _ = self.recycle_tx.try_send(batch);
                }
                StateUpdate::ClearHistory => {
                    self.sim_state.history.clear();
                }
                StateUpdate::CircuitLoaded(metadata) => {
                    self.sim_state.lookup_map = handle_circuit_loaded(&metadata);
                    self.sim_state.metadata = Some(metadata);
                }
            }
        }

        if let Ok(result) = self.file_receiver.try_recv() {
            if let Some(path) = result {
                match self.file_dialog_state {
                    FileDialogState::LoadSchem => {
                        self.load_from_path(path);
                        self.active_netlist = None;
                        self.undo_stack.clear();
                        ctx.send_viewport_cmd(ViewportCommand::Title(match self.current_file {
                            Some(ref p) => format!(
                                "Copperhead - {}",
                                // get file name only (no path, no extension)
                                p.file_stem().and_then(|s| s.to_str()).unwrap_or("Untitled")
                            ),
                            None => "Copperhead - Untitled".to_string(),
                        }));
                    }
                    FileDialogState::SaveSchem => {
                        self.save_to_path(path);
                    }
                    FileDialogState::SaveNetlist => {
                        self.save_netlist(path);
                    }
                    FileDialogState::LoadAudio => {
                        self.temp_audio_path = Some(path);
                    }
                    FileDialogState::SaveAudio => {
                        self.temp_audio_path = Some(path);
                    }
                    _ => {}
                }
            } else {
                // User cancelled the dialog
                debug!("File dialog was cancelled by the user.");
            }

            self.file_dialog_state = FileDialogState::Closed;
        }
    }

    pub fn handle_global_shortcuts(&mut self, ctx: &egui::Context) {
        if !self.sim_state.running {
            // Handle undo and redo only if not running
            if ctx.input_mut(|i| i.consume_key(Modifiers::COMMAND, Key::Z)) && !self.keybinds_locked
            {
                if let Some(prev) = self.undo_stack.undo(self.state.clone()) {
                    self.state = prev;
                    // TODO: maybe show a notification that undo was performed
                }
            }
            if ctx.input_mut(|i| i.consume_key(Modifiers::COMMAND, Key::Y)) && !self.keybinds_locked
            {
                if let Some(next) = self.undo_stack.redo(self.state.clone()) {
                    self.state = next;
                }
            }
        }

        // Handle Escape to cancel tool
        if ctx.input(|i| i.key_pressed(Key::Escape)) {
            self.selected_tool = Tool::Select;
        }

        // Handle Rotate (CTRL + R)
        if ctx.input(|i| i.modifiers.command && i.key_pressed(Key::R)) {
            self.current_rotation = (self.current_rotation + 1) % 4;
        }
    }
}
