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

use crate::circuit::Circuit;
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::parameter::{resolve_param_value, ComponentEvalCtx, ParamValue, RebuildKind};
use crate::util::deserialize_number_or_string;
use crate::util::mna::stamp_conductance;
use faer::{ColRef, MatMut};
use std::collections::HashMap;

/// Taper curve applied to the raw parameter value in [0, 1].
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize, Default)]
pub enum PotScale {
    #[default]
    Linear,
    /// Standard audio A-curve: (10^(2p) − 1) / 99
    /// Gives 0.0 at p=0, ≈0.091 at p=0.5, 1.0 at p=1.0.
    AudioTaper,
}

impl PotScale {
    pub fn apply(&self, raw: f64) -> f64 {
        let p = raw.clamp(0.0, 1.0);
        match self {
            PotScale::Linear => p,
            PotScale::AudioTaper => (10_f64.powf(2.0 * p) - 1.0) / 99.0,
        }
    }

    pub fn label(&self) -> &'static str {
        match self {
            PotScale::Linear => "Linear",
            PotScale::AudioTaper => "Audio Taper",
        }
    }
}

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct PotentiometerDef {
    #[serde(deserialize_with = "deserialize_number_or_string")]
    pub resistance: String,
    /// Name of the global parameter that drives wiper position in [0, 1].
    pub param_name: String,
    #[serde(default)]
    pub scale: PotScale,
    /// Optional formula override. When set, bypasses `param_name` entirely.
    /// Scale is NOT applied in formula mode — the formula result is used as-is.
    #[serde(default)]
    pub position_formula: Option<String>,
    pub comment: Option<String>,
    /// Runtime-only: last known wiper position for icon rendering. Not serialized.
    #[serde(skip, default = "PotentiometerDef::default_position")]
    pub current_position: f64,
}

impl PotentiometerDef {
    fn default_position() -> f64 {
        0.5
    }

    pub fn new(resistance: f64, param_name: String) -> Self {
        Self {
            resistance: resistance.to_string(),
            param_name,
            scale: PotScale::Linear,
            position_formula: None,
            comment: None,
            current_position: 0.5,
        }
    }

    pub fn is_formula_mode(&self) -> bool {
        self.position_formula.is_some()
    }

    fn position_source(&self) -> &str {
        match &self.position_formula {
            Some(f) => f.as_str(),
            None => self.param_name.as_str(),
        }
    }
}

impl<T: CircuitScalar> Instantiable<T> for PotentiometerDef {
    fn instantiate(&self, nodes: &[NodeId], _dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let rpv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(&self.resistance, ps))
            .unwrap_or_else(|| {
                ParamValue::Constant(self.resistance.trim().parse::<f64>().unwrap_or(0.0))
            });

        let pos_src = self.position_source();
        let ppv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(pos_src, ps))
            .unwrap_or_else(|| {
                ParamValue::Constant(pos_src.trim().parse::<f64>().unwrap_or(0.5))
            });

        // Scale only applies when driven by the parameter, not by an explicit formula.
        let scale = if self.is_formula_mode() {
            PotScale::Linear
        } else {
            self.scale.clone()
        };

        circuit.add_component(Potentiometer::new(nodes[0], nodes[1], nodes[2], rpv, ppv, scale));
    }
}

pub struct Potentiometer<T: CircuitScalar> {
    /// One end of the resistive track
    pub node_a: NodeId,
    /// Other end of the resistive track
    pub node_b: NodeId,
    /// Wiper (sliding contact)
    pub node_w: NodeId,

    resistance: ParamValue,
    position: ParamValue,
    scale: PotScale,

    cached_total_resistance: T,
    cached_position: T,

    /// Conductance between node A and wiper: G_aw = 1 / (pos * R_total)
    pub conductance_aw: T,
    /// Conductance between node B and wiper: G_bw = 1 / ((1 - pos) * R_total)
    pub conductance_bw: T,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,
    cached_idx_w: Option<usize>,

    /// When true, this component reports `LinearStatic` linearity so the
    /// solver can move its nodes into the pre-inverted L-block.
    frozen: bool,
}

impl<T: CircuitScalar> Potentiometer<T> {
    pub fn new(
        a: NodeId,
        b: NodeId,
        w: NodeId,
        resistance: ParamValue,
        position: ParamValue,
        scale: PotScale,
    ) -> Self {
        let r0 = resistance.as_constant().unwrap_or(1000.0);
        let p0 = scale.apply(position.as_constant().unwrap_or(0.5));

        let r0t = T::from(r0).unwrap();
        let p0t = T::from(p0).unwrap();
        let (g_aw, g_bw) = Self::compute_conductances(r0t, p0t);

        Self {
            node_a: a,
            node_b: b,
            node_w: w,
            resistance,
            position,
            scale,
            cached_total_resistance: r0t,
            cached_position: p0t,
            conductance_aw: g_aw,
            conductance_bw: g_bw,
            cached_idx_a: None,
            cached_idx_b: None,
            cached_idx_w: None,
            frozen: false,
        }
    }

    fn compute_conductances(total_resistance: T, position: T) -> (T, T) {
        let min_r = T::from(1e-12).unwrap();
        let max_g = T::from(1.0e12).unwrap();

        let r_aw = position * total_resistance;
        let r_bw = (T::one() - position) * total_resistance;

        let g_aw = if r_aw.abs() < min_r { max_g } else { T::one() / r_aw };
        let g_bw = if r_bw.abs() < min_r { max_g } else { T::one() / r_bw };

        (g_aw, g_bw)
    }

    fn update_conductances(&mut self) {
        let (g_aw, g_bw) =
            Self::compute_conductances(self.cached_total_resistance, self.cached_position);
        self.conductance_aw = g_aw;
        self.conductance_bw = g_bw;
    }

    fn get_voltage(&self, node: NodeId, solution: &ColRef<T>) -> T {
        if node.0 == 0 {
            T::zero()
        } else {
            solution[node.0 - 1]
        }
    }

    fn scaled_position(&self, raw: f64) -> T {
        T::from(self.scale.apply(raw)).unwrap()
    }
}

impl<T: CircuitScalar> Component<T> for Potentiometer<T> {
    fn linearity(&self) -> ComponentLinearity {
        if self.resistance.depends_on_voltage() || self.position.depends_on_voltage() {
            ComponentLinearity::NonLinear
        } else if self.frozen {
            // Frozen: conductances are baked into the L-block for this session.
            ComponentLinearity::LinearStatic
        } else {
            // Default: wiper can move between samples.
            ComponentLinearity::TimeVariant
        }
    }

    fn is_freeze_eligible(&self) -> bool {
        self.resistance.is_freeze_eligible() && self.position.is_freeze_eligible()
    }

    fn set_frozen(&mut self, frozen: bool) {
        self.frozen = frozen;
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = node_map.get(&self.node_a).copied();
        self.cached_idx_b = node_map.get(&self.node_b).copied();
        self.cached_idx_w = node_map.get(&self.node_w).copied();
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b, self.node_w]
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, _ctx: &SimulationContext<T>) {
        if self.frozen {
            // Bake the frozen conductances directly into the L-block.
            stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_w, self.conductance_aw, 0);
            stamp_conductance(matrix, self.cached_idx_b, self.cached_idx_w, self.conductance_bw, 0);
        }
    }

    fn stamp_time_variant(
        &self,
        matrix: &mut MatMut<T>,
        _ctx: &SimulationContext<T>,
        offset: usize,
    ) {
        stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_w, self.conductance_aw, offset);
        stamp_conductance(matrix, self.cached_idx_b, self.cached_idx_w, self.conductance_bw, offset);
    }

    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        matrix: &mut MatMut<T>,
        _rhs: &mut faer::ColMut<T>,
        _ctx: &SimulationContext<T>,
        l_size: usize,
    ) {
        stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_w, self.conductance_aw, l_size);
        stamp_conductance(matrix, self.cached_idx_b, self.cached_idx_w, self.conductance_bw, l_size);
    }

    fn refresh_per_step(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if self.frozen {
            return;
        }
        let any_voltage =
            self.resistance.depends_on_voltage() || self.position.depends_on_voltage();
        if (self.resistance.is_dynamic() || self.position.is_dynamic()) && !any_voltage {
            self.cached_total_resistance = T::from(self.resistance.eval(eval)).unwrap();
            let raw = self.position.eval(eval);
            self.cached_position = self.scaled_position(raw);
            self.update_conductances();
        }
    }

    fn refresh_per_iter(&mut self, eval: &ComponentEvalCtx, _sim: &SimulationContext<T>) {
        if self.frozen {
            return;
        }
        if self.resistance.depends_on_voltage() || self.position.depends_on_voltage() {
            self.cached_total_resistance = T::from(self.resistance.eval(eval)).unwrap();
            let raw = self.position.eval(eval);
            self.cached_position = self.scaled_position(raw);
            self.update_conductances();
        }
    }

    fn set_parameter(&mut self, name: &str, value: T, _ctx: &SimulationContext<T>) -> bool {
        match name {
            "resistance" => {
                self.cached_total_resistance = value;
                self.update_conductances();
                true
            }
            // Direct position set (bypass scale — used for internal/legacy updates).
            "position" => {
                self.cached_position = value;
                self.update_conductances();
                true
            }
            _ => false,
        }
    }

    fn set_param_value(&mut self, name: &str, pv: ParamValue) -> RebuildKind {
        let (was_dyn, was_volt) = match name {
            "resistance" => (
                self.resistance.is_dynamic(),
                self.resistance.depends_on_voltage(),
            ),
            "position" => (
                self.position.is_dynamic(),
                self.position.depends_on_voltage(),
            ),
            _ => return RebuildKind::None,
        };

        match name {
            "resistance" => {
                if let Some(v) = pv.as_constant() {
                    self.cached_total_resistance = T::from(v).unwrap();
                    self.update_conductances();
                }
                self.resistance = pv;
            }
            "position" => {
                if let Some(v) = pv.as_constant() {
                    self.cached_position = self.scaled_position(v);
                    self.update_conductances();
                }
                self.position = pv;
            }
            _ => unreachable!(),
        }

        let now_dyn = match name {
            "resistance" => self.resistance.is_dynamic(),
            _ => self.position.is_dynamic(),
        };
        let now_volt = match name {
            "resistance" => self.resistance.depends_on_voltage(),
            _ => self.position.depends_on_voltage(),
        };

        if was_dyn != now_dyn || was_volt != now_volt {
            RebuildKind::Repartition
        } else if !now_dyn {
            RebuildKind::Restamp
        } else {
            RebuildKind::None
        }
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe { name: "V_aw".into(), unit: "V".into() },
            ComponentProbe { name: "V_bw".into(), unit: "V".into() },
            ComponentProbe { name: "I_aw".into(), unit: "A".into() },
            ComponentProbe { name: "I_bw".into(), unit: "A".into() },
            ComponentProbe { name: "Power".into(), unit: "W".into() },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        let v_a = self.get_voltage(self.node_a, node_voltages);
        let v_b = self.get_voltage(self.node_b, node_voltages);
        let v_w = self.get_voltage(self.node_w, node_voltages);

        let v_aw = v_a - v_w;
        let v_bw = v_b - v_w;
        let i_aw = v_aw * self.conductance_aw;
        let i_bw = v_bw * self.conductance_bw;

        out_observables[0] = v_aw;
        out_observables[1] = v_bw;
        out_observables[2] = i_aw;
        out_observables[3] = i_bw;
        out_observables[4] = v_aw * i_aw + v_bw * i_bw;
    }

    fn terminal_currents(
        &self,
        sol: &ColRef<T>,
        _ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v_a = self.get_voltage(self.node_a, sol);
        let v_b = self.get_voltage(self.node_b, sol);
        let v_w = self.get_voltage(self.node_w, sol);

        let i_a = (v_a - v_w) * self.conductance_aw;
        let i_b = (v_b - v_w) * self.conductance_bw;
        let i_w = -(i_a + i_b);

        out_currents[0] = i_a;
        out_currents[1] = i_b;
        out_currents[2] = i_w;
    }
}
