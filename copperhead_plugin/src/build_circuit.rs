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
use copperhead_core::circuit::Circuit;
use copperhead_core::components::ComponentId;
use copperhead_core::components::voltage_source::VoltageSource;
use copperhead_core::descriptor::ComponentDescriptor;
use copperhead_core::model::NodeId;
use copperhead_core::processor::CircuitProcessor;
use copperhead_core::signals::{RealtimeInputSignal, SignalType};
use serde::Deserialize;
use std::io::BufReader;
use std::path::PathBuf;

#[derive(Deserialize, Debug)]
struct ShortenedNetlist {
    pub instructions: Vec<ComponentDescriptor>,
}

pub fn build_circuit(
    path: PathBuf,
    desired_sample_rate: f64,
) -> Result<(CircuitProcessor<f64>, ComponentId, NodeId), Box<dyn std::error::Error>> {
    let file = std::fs::File::open(path)?;
    let reader = BufReader::new(file);
    let netlist: ShortenedNetlist = serde_json::from_reader(reader)?;

    let mut circuit: Circuit<f64> = Circuit::new();
    let dt = 1.0 / desired_sample_rate as f64;

    // the voltage probe
    let mut probe: Option<NodeId> = None;

    for instr in netlist.instructions {
        if let ComponentDescriptor::AudioProbe { node, .. } = instr {
            if probe.is_some() {
                return Err("Multiple audio probes found in netlist".into());
            }

            probe = Some(NodeId(node));
        } else if let ComponentDescriptor::AudioSource {
            pos,
            neg,
            file_path,
        } = instr
        {
            let signal = SignalType::RealtimeInput(RealtimeInputSignal { current_value: 0.0 });
            let comp = VoltageSource::new(NodeId(pos), NodeId(neg), signal);

            circuit.add_component(comp);
        } else {
            instr.add_to_circuit(dt, &mut circuit);
        }
    }

    let processor = CircuitProcessor::new(circuit, desired_sample_rate, dt)?;

    // Find the ComponentId of the AudioSource
    let realtime_input_ids: Vec<ComponentId> = processor
        .get_circuit_ref()
        .components
        .voltage_sources
        .iter()
        .enumerate()
        .filter(|(_idx, vs)| matches!(vs.signal, SignalType::RealtimeInput(_)))
        .map(|(idx, _vs)| ComponentId::VoltageSource(idx))
        .collect();
    // For now we just assume that there's only one audio source
    let input = realtime_input_ids
        .first()
        .ok_or("No audio source found in netlist")?;
    let output = probe.ok_or("No audio probe found in netlist")?;

    Ok((processor, *input, output))
}
