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

pub mod build_circuit;
pub mod editor;

use crossbeam_channel::{Receiver, Sender};
use nih_plug::editor::Editor;
use nih_plug::prelude::{
    AsyncExecutor, AtomicF32, AudioIOLayout, AuxiliaryBuffers, Buffer, BufferConfig, InitContext,
    MidiConfig, Params, Plugin, PortNames, ProcessContext, ProcessStatus,
};
use std::num::NonZeroU32;
use std::path::PathBuf;
use std::sync::atomic::Ordering;
use std::sync::{Arc, RwLock};

#[cfg(feature = "profiling")]
use tracing::info_span;
#[cfg(feature = "profiling")]
use tracy_client::Client;

use crate::build_circuit::build_circuit;
use copperhead_core::circuit::Circuit;
use copperhead_core::components::ComponentId;
use copperhead_core::model::{NodeId, SimulationContext};
use copperhead_core::processor::CircuitProcessor;
use vizia_plug::ViziaState;

/// Default oversample factor. The internal sample rate is plugin_sample_rate * oversample_factor.
const DEFAULT_OVERSAMPLE_FACTOR: usize = 4;

pub struct CopperheadPlugin {
    params: Arc<CopperheadPluginParams>,
    processor: Option<CircuitProcessor<f64>>,
    processor_rx: Receiver<CircuitProcessor<f64>>,

    /// Factor by which the internal rate exceeds the plugin rate.
    oversample_factor: usize,
    /// The host / plugin sample rate.
    plugin_sample_rate: f32,
    /// The internal (oversampled) sample rate used by the circuit.
    internal_sample_rate: f64,

    /// Buffer holding upsampled input samples (first channel only) at the internal rate.
    upsample_buffer: Vec<f64>,
    /// Buffer holding processed samples at the internal rate, to be downsampled back.
    downsample_buffer: Vec<f64>,
}

#[derive(Params)]
struct CopperheadPluginParams {
    #[persist = "editor-state"]
    editor_state: Arc<ViziaState>,
    pub circuit_tx: Sender<CircuitProcessor<f64>>,
    pub sample_rate: AtomicF32,
    pub selected_path: RwLock<Option<PathBuf>>,
    pub circuit_info: RwLock<Option<(ComponentId, NodeId)>>,
}

impl Default for CopperheadPlugin {
    fn default() -> Self {
        let (circuit_tx, circuit_rx) = crossbeam_channel::unbounded();

        Self {
            params: Arc::new(CopperheadPluginParams {
                circuit_tx,
                sample_rate: AtomicF32::new(48000.0),
                selected_path: RwLock::new(None),
                circuit_info: RwLock::new(None),
                editor_state: editor::default_state(),
            }),
            processor: None,
            processor_rx: circuit_rx,
            oversample_factor: DEFAULT_OVERSAMPLE_FACTOR,
            plugin_sample_rate: 48000.0,
            internal_sample_rate: 48000.0 * DEFAULT_OVERSAMPLE_FACTOR as f64,
            upsample_buffer: Vec::new(),
            downsample_buffer: Vec::new(),
        }
    }
}

impl CopperheadPlugin {
    /// Recalculate the internal sample rate and update `dummy_ctx.dt`.
    fn update_internal_sample_rate(&mut self, plugin_sr: f32) {
        self.plugin_sample_rate = plugin_sr;
        self.internal_sample_rate = plugin_sr as f64 * self.oversample_factor as f64;
    }

    /// Ensure internal buffers are large enough for a given host buffer size.
    fn resize_buffers(&mut self, max_buffer_size: u32) {
        let internal_len = max_buffer_size as usize * self.oversample_factor;
        self.upsample_buffer.resize(internal_len, 0.0);
        self.downsample_buffer.resize(internal_len, 0.0);
    }

    /// Linearly upsample the first channel of `buffer` into `self.upsample_buffer`.
    /// Returns the number of internal samples produced.
    fn upsample_first_channel(&mut self, buffer: &Buffer) -> usize {
        let host_samples = buffer.samples();
        let factor = self.oversample_factor;
        let internal_len = host_samples * factor;

        // Ensure capacity
        if self.upsample_buffer.len() < internal_len {
            self.upsample_buffer.resize(internal_len, 0.0);
        }

        let channel = &buffer.as_slice_immutable()[0];

        for i in 0..host_samples {
            let current = channel[i] as f64;
            let next = if i + 1 < host_samples {
                channel[i + 1] as f64
            } else {
                current
            };
            for j in 0..factor {
                let t = j as f64 / factor as f64;
                self.upsample_buffer[i * factor + j] = current + (next - current) * t;
            }
        }

        internal_len
    }

    /// Downsample `self.downsample_buffer` back into the first channel of `buffer`
    /// by picking every `oversample_factor`-th sample (decimation).
    fn downsample_first_channel(&self, buffer: &mut Buffer) {
        let host_samples = buffer.samples();
        let factor = self.oversample_factor;

        let channel = &mut buffer.as_slice()[0]; // first channel (mutable slice)

        for i in 0..host_samples {
            channel[i] = self.downsample_buffer[i * factor] as f32;
        }
    }
}

/// Returns an iterator over pairs `(&f64 /* input */, &mut f64 /* output */)` from the
/// provided upsampled and downsampled slices, without borrowing `self`.
fn iter_internal_samples<'a>(
    upsample_buffer: &'a [f64],
    downsample_buffer: &'a mut [f64],
    internal_len: usize,
) -> InternalSampleIter<'a> {
    InternalSampleIter {
        upsample: upsample_buffer[..internal_len].iter(),
        downsample: downsample_buffer[..internal_len].iter_mut(),
    }
}

/// Iterator yielding `(&f64 /* input */, &mut f64 /* output */)` for each internal sample.
pub struct InternalSampleIter<'a> {
    upsample: std::slice::Iter<'a, f64>,
    downsample: std::slice::IterMut<'a, f64>,
}

impl<'a> Iterator for InternalSampleIter<'a> {
    /// (input_sample, output_sample)
    type Item = (&'a f64, &'a mut f64);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.upsample.next(), self.downsample.next()) {
            (Some(input), Some(output)) => Some((input, output)),
            _ => None,
        }
    }
}

impl Plugin for CopperheadPlugin {
    const NAME: &'static str = "Copperhead Plugin";
    const VENDOR: &'static str = "CyCode";
    const URL: &'static str = env!("CARGO_PKG_HOMEPAGE");
    const EMAIL: &'static str = "cycode@cycode.eu";

    const VERSION: &'static str = env!("CARGO_PKG_VERSION");

    const AUDIO_IO_LAYOUTS: &'static [AudioIOLayout] = &[AudioIOLayout {
        main_input_channels: NonZeroU32::new(1),
        main_output_channels: NonZeroU32::new(2),

        aux_input_ports: &[],
        aux_output_ports: &[],

        names: PortNames::const_default(),
    }];

    const MIDI_INPUT: MidiConfig = MidiConfig::None;
    const MIDI_OUTPUT: MidiConfig = MidiConfig::None;

    const SAMPLE_ACCURATE_AUTOMATION: bool = true;

    type SysExMessage = ();
    type BackgroundTask = ();

    fn params(&self) -> Arc<dyn Params> {
        self.params.clone()
    }

    fn initialize(
        &mut self,
        _audio_io_layout: &AudioIOLayout,
        buffer_config: &BufferConfig,
        _context: &mut impl InitContext<Self>,
    ) -> bool {
        #[cfg(feature = "profiling")]
        let tracy = Client::start();

        #[cfg(feature = "profiling")]
        tracy.set_thread_name("Simulation Loop");

        let sample_rate = buffer_config.sample_rate;
        self.params
            .sample_rate
            .store(sample_rate, Ordering::Relaxed);

        // Update internal sample rate and pre-allocate buffers
        self.update_internal_sample_rate(sample_rate);
        self.resize_buffers(buffer_config.max_buffer_size);

        // Rebuild the circuit if a file was previously selected
        let path_guard = self.params.selected_path.read().unwrap();
        if let Some(path) = &*path_guard {
            if let Ok((processor, input, output)) =
                build_circuit(path.clone(), self.internal_sample_rate)
            {
                self.processor = Some(processor);

                // Update the info for the UI
                if let Ok(mut info_guard) = self.params.circuit_info.write() {
                    *info_guard = Some((input, output));
                }
            }
        }
        true
    }

    fn reset(&mut self) {
        for s in self.upsample_buffer.iter_mut() {
            *s = 0.0;
        }
        for s in self.downsample_buffer.iter_mut() {
            *s = 0.0;
        }
    }

    fn process(
        &mut self,
        buffer: &mut Buffer,
        _aux: &mut AuxiliaryBuffers,
        _context: &mut impl ProcessContext<Self>,
    ) -> ProcessStatus {
        #[cfg(feature = "profiling")]
        let bfr_start_span = info_span!("Buffer start").entered();
        while let Ok(new_processor) = self.processor_rx.try_recv() {
            self.processor = Some(new_processor);
        }

        if self.processor.is_some() {
            let (input, output) = match self.params.circuit_info.read() {
                Ok(info_guard) => match *info_guard {
                    Some((input, output)) => (input, output),
                    None => return ProcessStatus::Normal,
                },
                Err(_) => return ProcessStatus::Normal,
            };

            // Upsample first channel into internal buffer
            let internal_len = self.upsample_first_channel(buffer);

            // Process each internal sample
            let proc = self.processor.as_mut().unwrap();
            let iter = iter_internal_samples(
                &self.upsample_buffer,
                &mut self.downsample_buffer,
                internal_len,
            );
            for (input_sample, output_sample) in iter {
                proc.set_input_voltage(input, *input_sample);

                #[cfg(feature = "profiling")]
                let tick_start_span = info_span!("Tick start").entered();
                proc.tick();

                #[cfg(feature = "profiling")]
                tick_start_span.exit();
                *output_sample = proc.get_circuit_ref().get_node_voltage(output);
            }

            //  Downsample processed data back into first channel
            self.downsample_first_channel(buffer);
        }

        #[cfg(feature = "profiling")]
        bfr_start_span.exit();

        ProcessStatus::Normal
    }

    fn editor(&mut self, async_executor: AsyncExecutor<Self>) -> Option<Box<dyn Editor>> {
        editor::create(self.params.clone(), self.params.editor_state.clone())
    }
}
