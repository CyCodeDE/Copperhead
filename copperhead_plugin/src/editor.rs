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

use crate::CopperheadPluginParams;
use crate::build_circuit::build_circuit;
use nih_plug::prelude::Editor;
use rfd::{AsyncFileDialog, FileDialog};
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::Ordering;
use vizia_plug::vizia::prelude::*;
use vizia_plug::{ViziaState, ViziaTheming, create_vizia_editor};

#[derive(Lens)]
struct Data {
    params: Arc<CopperheadPluginParams>,
}

enum FilePickerEvent {
    Open,
    Picked(PathBuf),
    Canceled,
}

impl Model for Data {
    fn event(&mut self, cx: &mut EventContext, event: &mut Event) {
        event.map(|app_event, _| match app_event {
            FilePickerEvent::Open => {
                cx.spawn(|cx| {
                    let file = FileDialog::new().set_title("Select a File").pick_file();

                    if let Some(file) = file {
                        cx.emit(FilePickerEvent::Picked(file.to_path_buf()));
                    }
                });
            }
            FilePickerEvent::Picked(path) => {
                let sr = self.params.sample_rate.load(Ordering::Relaxed);

                match build_circuit(path.clone(), sr as f64) {
                    Ok((processor, comp_id, node_id)) => {
                        *self.params.selected_path.write().unwrap() = Some(path.clone());
                        *self.params.circuit_info.write().unwrap() = Some((comp_id, node_id));

                        let _ = self.params.circuit_tx.send(processor);
                    }
                    Err(e) => {
                        eprintln!("Error building circuit: {}", e);
                    }
                }
            }
            FilePickerEvent::Canceled => {}
        });
    }
}

pub(crate) fn default_state() -> Arc<ViziaState> {
    ViziaState::new(|| (200, 150))
}

pub(crate) fn create(
    params: Arc<CopperheadPluginParams>,
    editor_state: Arc<ViziaState>,
) -> Option<Box<dyn Editor>> {
    create_vizia_editor(editor_state, ViziaTheming::Custom, move |cx, _| {
        Data {
            params: params.clone(),
        }
        .build(cx);

        VStack::new(cx, |cx| {
            Label::new(cx, "Copperhead")
                .font_family(vec![FamilyOwned::Named(String::from("Noto Sans"))])
                .font_weight(FontWeightKeyword::Light)
                .font_size(30.0)
                .height(Pixels(50.0))
                .alignment(Alignment::BottomCenter);

            Label::new(cx, "Choose a netlist to load");

            Button::new(cx, |cx| Label::new(cx, "Load Netlist")).on_press(|ctx| {
                ctx.emit(FilePickerEvent::Open);
            });
        })
        .alignment(Alignment::TopCenter);
    })
}
