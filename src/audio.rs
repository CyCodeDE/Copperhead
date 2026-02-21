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

use num_traits::NumCast;
use std::fs::File;
use std::path::PathBuf;
use audioadapter_buffers::direct::InterleavedSlice;
use rubato::{Fft, FixedSync, Resampler};
use symphonia::core::audio::SampleBuffer;
use symphonia::core::codecs::{DecoderOptions, CODEC_TYPE_NULL};
use symphonia::core::formats::FormatOptions;
use symphonia::core::io::MediaSourceStream;
use symphonia::core::meta::MetadataOptions;
use symphonia::core::probe::Hint;

/// Reads an audio file (WAV, MP3, FLAC), converts to mono f32,
/// and resamples to the target simulation sample rate.
pub fn load_and_resample_audio<T: NumCast>(
    file_path: &PathBuf,
    target_sample_rate: u32,
) -> Vec<T> {
    // Open file and probe format
    let src = File::open(file_path).expect("Failed to open audio file");
    let mss = MediaSourceStream::new(Box::new(src), Default::default());
    let hint = Hint::new();

    let probed = symphonia::default::get_probe()
        .format(&hint, mss, &FormatOptions::default(), &MetadataOptions::default())
        .expect("Unsupported audio format");

    let mut format = probed.format;

    // Find the primary audio track
    let track = format
        .tracks()
        .iter()
        .find(|t| t.codec_params.codec != CODEC_TYPE_NULL)
        .expect("No supported audio track found");

    let track_id = track.id;
    let source_sample_rate = track.codec_params.sample_rate.unwrap_or(44100);
    let channels = track.codec_params.channels.unwrap().count();

    let mut decoder = symphonia::default::get_codecs()
        .make(&track.codec_params, &DecoderOptions::default())
        .expect("Unsupported codec");

    let mut raw_samples_f32 = Vec::new();

    // Decode packets and convert to f32
    while let Ok(packet) = format.next_packet() {
        if packet.track_id() != track_id {
            continue;
        }
        match decoder.decode(&packet) {
            Ok(audio_buf) => {
                let mut sample_buf = SampleBuffer::<f32>::new(
                    audio_buf.capacity() as u64,
                    *audio_buf.spec(),
                );
                sample_buf.copy_interleaved_ref(audio_buf);

                // Convert to mono by taking the first channel (e.g., Left) if multiple channels exist
                for chunk in sample_buf.samples().chunks_exact(channels) {
                    raw_samples_f32.push(chunk[0]);
                }
            }
            Err(symphonia::core::errors::Error::DecodeError(_)) => (), // Skip bad packets
            Err(_) => break, // EOF or fatal error
        }
    }

    // Resample if sample rates differ
    let final_f32_samples = if source_sample_rate != target_sample_rate {
        println!("Resampling from {} Hz to {} Hz...", source_sample_rate, target_sample_rate);

        let mut resampler = Fft::<f32>::new(
            source_sample_rate as usize,
            target_sample_rate as usize,
            raw_samples_f32.len(),
            1,
            1, // mono
            FixedSync::Input
        ).expect("Failed to initialize resampler");

        let input_buffers = InterleavedSlice::new(&raw_samples_f32, channels, raw_samples_f32.len()).expect("Failed to interlain raw samples");
        let output_buffers = resampler.process(&input_buffers, 0, None)
            .expect("Failed to resample audio");

        output_buffers.take_data()
    } else {
        raw_samples_f32
    };

    println!("Audio loaded. Final Samples: {}", final_f32_samples.len());

    final_f32_samples
        .into_iter()
        .map(|s| num_traits::cast(s).expect("Failed to cast audio sample to circuit scalar"))
        .collect()
}