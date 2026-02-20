/*
 * Copyright (c) 2026-2026 Patrice Wehnemann and the Copperhead contributors
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

use crate::circuit::{Circuit, CircuitElement};
use crate::ui::app::StateUpdate;
use crate::ui::{
    CircuitMetadata, ComponentMetadata, SimCommand,
};
use crossbeam::channel::{Receiver, Sender};

pub fn run_simulation_loop(rx: Receiver<SimCommand>, state: Sender<StateUpdate>) {
    let mut realtime_mode = false;
    let sample_rate = 96000.0;
    let mut circuit: Option<Circuit<f64>> = None;
    let mut running = false;
    state.send(StateUpdate::UpdateRunning(false));
    let dt = 1.0 / sample_rate;

    // Batch size: Push data to UI roughly at 60fps
    let steps_per_batch = (sample_rate / 60.0f64).ceil() as usize;

    let mut current_step: usize = 0;
    let mut max_steps: usize = usize::MAX;

    // Buffer to accumulate data when UI is busy reading
    let mut pending_data = Vec::with_capacity(steps_per_batch * 4);

    loop {
        while let Ok(cmd) = rx.try_recv() {
            match cmd {
                SimCommand::Pause => {
                    running = false;
                    state.send(StateUpdate::UpdateRunning(false));
                }
                SimCommand::Resume => {
                    running = true;
                    state.send(StateUpdate::UpdateRunning(true));
                }
                SimCommand::LoadCircuit(netlist) => {
                    //let mut s = state.write();
                    state.send(StateUpdate::UpdateRunning(false));
                    running = false;
                    current_step = 0;
                    pending_data.clear();

                    let mut new_ckt = Circuit::<f64>::new();
                    for instr in netlist.instructions {
                        //new_ckt.add_component(instr.build(dt));
                        new_ckt.add_component(instr.build(dt));
                    }

                    match new_ckt.calculate_dc_operating_point(1e-6, 100, dt) {
                        Ok(_) => println!("Initial state calculated successfully."),
                        Err(e) => println!("Warning: Failed to calculate initial state: {}", e),
                    }

                    new_ckt.prepare(dt, false);

                    let mut comp_meta = Vec::new();
                    let mut total_terminals = 0;
                    let mut total_observables = 0;

                    for (ui_idx, &graph_idx) in new_ckt.component_order.iter().enumerate() {
                        if let CircuitElement::Device(comp) = &new_ckt.graph[graph_idx] {
                            let probes = comp.probe_definitions();
                            let num_terms = comp.ports().len();

                            total_observables += probes.len();
                            total_terminals += num_terms;

                            comp_meta.push(ComponentMetadata {
                                id: ui_idx,
                                probe_definitions: probes,
                                num_terminals: num_terms,
                            });
                        }
                    }

                    state.send(StateUpdate::ClearHistory);
                    state.send(StateUpdate::CircuitLoaded(CircuitMetadata {
                        components: comp_meta,
                    }));

                    circuit = Some(new_ckt);
                }
                SimCommand::UpdateValue {
                    component_id: usize,
                    updated: ComponentBuildData,
                } => {
                    if let Some(ref mut ckt) = circuit {
                        if !running {
                            // NOT IMPLEMENTED
                        }
                    }
                }
                SimCommand::SetRunTime(run_time) => {
                    if !realtime_mode {
                        // sets how long to simulate (called before the next LoadCircuit)
                        let total_steps = (run_time * sample_rate) as usize;
                        max_steps = total_steps;
                    }
                }
                SimCommand::SetRealtime(realtime) => {
                    realtime_mode = realtime;
                }
            }
        }

        // Run Simulation Batch
        if running {
            // clear pending data
            pending_data.clear();
            let batch_start = std::time::Instant::now();

            if current_step >= max_steps {
                running = false;
                state.send(StateUpdate::UpdateRunning(false));
            } else if let Some(ref mut ckt) = circuit {
                let mut steps_performed = 0;
                while steps_performed < steps_per_batch && current_step < max_steps {
                    ckt.solve_step(dt);
                    current_step += 1;
                    steps_performed += 1;
                }

                // Extract History
                if steps_performed > 0 {
                    let new_data = ckt.extract_history();
                    pending_data.extend(new_data);
                }

                // Try to flush to UI
                if !pending_data.is_empty() {
                    state.send(StateUpdate::SendHistory(pending_data.clone(), current_step));
                }

                if realtime_mode && steps_performed > 0 {
                    let elapsed = batch_start.elapsed();
                    let target_duration =
                        std::time::Duration::from_secs_f64(steps_performed as f64 * dt);
                    if target_duration > elapsed {
                        std::thread::sleep(target_duration - elapsed);
                    }
                }

                if current_step >= max_steps {
                    running = false;
                    state.send(StateUpdate::UpdateRunning(false));
                }
            }
        } else {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }
}
