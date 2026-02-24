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

use crate::audio::write_to_wav;
use crate::circuit::{Circuit, CircuitElement};
use crate::components::{ComponentDescriptor, add_probe_to_circuit};
use crate::ui::app::StateUpdate;
use crate::ui::{CircuitMetadata, ComponentMetadata, SimBatchData, SimCommand, SimStepData};
use crossbeam::channel::{Receiver, Sender};
use log::info;
use tracy_client::Client;

pub fn run_simulation_loop(
    rx: Receiver<SimCommand>,
    state: Sender<StateUpdate>,
    recycled_rx: Receiver<SimBatchData>,
) {
    let tracy = Client::start();

    tracy.set_thread_name("Simulation Loop");

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
    let mut current_start = std::time::Instant::now();

    // Buffer to accumulate data when UI is busy reading
    let mut active_batch = SimBatchData::default();

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
                    current_start = std::time::Instant::now();
                }
                SimCommand::LoadCircuit(netlist) => {
                    assert_ne!(
                        max_steps,
                        usize::MAX,
                        "usize overflow protection: Did you forget to set the maximum run time before loading the circuit?"
                    );
                    //let mut s = state.write();
                    state.send(StateUpdate::UpdateRunning(false));
                    running = false;
                    current_step = 0;

                    let mut new_ckt = Circuit::<f64>::new();
                    for instr in netlist.instructions {
                        if matches!(instr, ComponentDescriptor::AudioProbe { .. }) {
                            add_probe_to_circuit(instr, dt, &mut new_ckt, max_steps);
                        } else {
                            instr.add_to_circuit(dt, &mut new_ckt);
                        }
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
                        if let CircuitElement::Device(comp_id) = &new_ckt.graph[graph_idx] {
                            let probes = new_ckt.components.get_probe_definitions(*comp_id);
                            let num_terms = new_ckt.components.get_num_terminals(*comp_id);

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

                    active_batch = SimBatchData {
                        times: Vec::with_capacity(0),
                        voltages: Vec::new(),
                        currents: Vec::new(),
                        observables: Vec::new(),
                        nodes_per_step: new_ckt.num_nodes + 1,
                        terminals_per_step: total_terminals,
                        observables_per_step: total_observables,
                    };

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
            let _batch_span = tracing::info_span!("sim_batch").entered();

            let batch_start = if realtime_mode {
                Some(std::time::Instant::now())
            } else {
                None
            };

            if current_step >= max_steps {
                running = false;
                state.send(StateUpdate::UpdateRunning(false));
            } else if let Some(ref mut ckt) = circuit {
                let mut steps_performed = 0;
                {
                    while steps_performed < steps_per_batch && current_step < max_steps {
                        //let _solve_span = tracing::info_span!("solve_call").entered();
                        ckt.solve_step(dt);
                        current_step += 1;
                        steps_performed += 1;
                    }
                }

                // Extract History
                if steps_performed > 0 {
                    let _extract_span = tracing::info_span!("extract_history").entered();
                    ckt.extract_history(&mut active_batch);
                }

                // Try to flush to UI
                if !active_batch.times.is_empty() {
                    let _flush_span = tracing::info_span!("flush_to_ui").entered();

                    let nodes = active_batch.nodes_per_step;
                    let terms = active_batch.terminals_per_step;
                    let obs = active_batch.observables_per_step;

                    let empty_recycled_batch = recycled_rx
                        .try_recv()
                        .map(|mut b| {
                            b.clear_for_reuse();
                            b
                        })
                        .unwrap_or_else(|_| SimBatchData {
                            times: Vec::with_capacity(steps_per_batch),
                            voltages: Vec::with_capacity(steps_per_batch * nodes),
                            currents: Vec::with_capacity(steps_per_batch * terms),
                            observables: Vec::with_capacity(steps_per_batch * obs),
                            nodes_per_step: nodes,
                            terminals_per_step: terms,
                            observables_per_step: obs,
                        });

                    let data_to_send = std::mem::replace(&mut active_batch, empty_recycled_batch);
                    state.send(StateUpdate::SendHistory(data_to_send, current_step));
                }

                if realtime_mode && steps_performed > 0 {
                    if let Some(start) = batch_start {
                        let elapsed = start.elapsed();
                        let target_duration =
                            std::time::Duration::from_secs_f64(steps_performed as f64 * dt);
                        if target_duration > elapsed {
                            std::thread::sleep(target_duration - elapsed);
                        }
                    }
                }

                if current_step >= max_steps {
                    running = false;
                    state.send(StateUpdate::UpdateRunning(false));
                    let elapsed = current_start.elapsed();
                    println!("Finished after: {:?}", elapsed);
                    println!("Average time per step: {:?}", elapsed / current_step as u32);

                    for probe in &ckt.components.audio_probes {
                        if probe.buffer.is_empty() || probe.filepath.is_empty() {
                            info!("Skipping WAV export for probe due to empty buffer or filepath");
                            continue;
                        }
                        write_to_wav(
                            probe.filepath.clone(),
                            &probe.buffer.as_slice(),
                            probe.target_sample_rate,
                        );
                    }
                }
            }

            drop(_batch_span);
            tracy.frame_mark();
        } else {
            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }
}
