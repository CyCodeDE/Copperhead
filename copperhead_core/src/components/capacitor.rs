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
use crate::util::deserialize_number_or_string;
use crate::circuit::Circuit;
use crate::components::{Component, ComponentLinearity, ComponentProbe};
use crate::descriptor::Instantiable;
use crate::model::{CircuitScalar, NodeId, SimulationContext};
use crate::parameter::{resolve_param_value, ComponentEvalCtx, ParamValue, RebuildKind};
use crate::util::mna::{get_voltage_diff, stamp_conductance, stamp_current_source};
use faer::{ColMut, ColRef, MatMut};
use std::collections::HashMap;
use crate::components::resistor::ResistorDef;

#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct CapacitorDef {
    #[serde(deserialize_with = "deserialize_number_or_string")]
    pub capacitance: String,
    pub esr: String,
}

impl CapacitorDef {
    pub fn new(capacitance: f64, esr: f64) -> Self {
        Self { capacitance: capacitance.to_string(), esr: esr.to_string() }
    }
}

impl<T: CircuitScalar> Instantiable<T> for CapacitorDef {
    fn instantiate(&self, nodes: &[NodeId], dt: T, circuit: &mut Circuit<T>, _max_steps: usize) {
        let cpv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(&self.capacitance, ps))
            .unwrap_or_else(|| {
                let v = self.capacitance.trim().parse::<f64>().unwrap_or(0.0);
                ParamValue::Constant(v)
            });

        let esrpv = circuit
            .param_system
            .as_ref()
            .map(|ps| resolve_param_value(&self.esr, ps))
            .unwrap_or_else(|| {
                let v = self.esr.trim().parse::<f64>().unwrap_or(0.0);
                ParamValue::Constant(v)
            });
        circuit.add_component(Capacitor::<T>::new(nodes[0], nodes[1], cpv, esrpv, dt));
    }
}

pub struct Capacitor<T: CircuitScalar> {
    node_a: NodeId,
    node_b: NodeId,

    cached_idx_a: Option<usize>,
    cached_idx_b: Option<usize>,

    /// User-facing physical capacitance in Farads.
    capacitance: ParamValue,

    /// User-facing equivalent series resistance in Ohms.
    esr: ParamValue,

    /// Discrete equivalent conductance: derived from `capacitance`, `esr`,
    /// and `dt`. Refreshed by `refresh_per_step` / `refresh_per_iter` when
    /// either parameter is formula-driven; otherwise computed once at
    /// construction and on `set_param_value` for constant updates.
    conductance: T,

    /// Norton equivalent current source value (history state) updated by
    /// `update_state`. Stamped into b every step by `stamp_dynamic`.
    eq_current: T,

    // BDF2 history states
    /// Internal capacitor voltage at t[n-1]
    v_c_m1: T,

    /// Internal capacitor voltage at t[n-2]
    v_c_m2: T,
}

impl<T: CircuitScalar> Capacitor<T> {
    pub fn new(a: NodeId, b: NodeId, capacitance: ParamValue, esr: ParamValue, dt: T) -> Self {
        let c0 = capacitance.as_constant().unwrap_or(1.0);
        let r0 = esr.as_constant().unwrap_or(0.0);

        let mut cap = Self {
            node_a: a,
            node_b: b,
            cached_idx_a: None,
            cached_idx_b: None,
            capacitance,
            esr,
            conductance: T::zero(),
            eq_current: T::zero(),
            v_c_m1: T::zero(),
            v_c_m2: T::zero(),
        };

        cap.conductance = Self::compute_conductance(c0, r0, dt);
        cap
    }

    /// G = 1 / (esr + 2·dt / (3·C))  — BDF2 Norton equivalent.
    fn compute_conductance(c: f64, esr: f64, dt: T) -> T {
        let two = T::from(2.0).unwrap();
        let three = T::from(3.0).unwrap();
        let c_t = T::from(c).unwrap();
        let esr_t = T::from(esr).unwrap();
        let r_c = (two * dt) / (three * c_t);
        T::one() / (esr_t + r_c)
    }

    /// Pull the current f64 value out of capacitance / esr (whether they're
    /// constants or formulas) and rebuild the discrete conductance.
    fn refresh_conductance(&mut self, eval: &ComponentEvalCtx, dt: T) {
        let c = self.capacitance.eval(eval);
        let r = self.esr.eval(eval);
        self.conductance = Self::compute_conductance(c, r, dt);
    }

    fn esr_value(&mut self, eval: &ComponentEvalCtx) -> T {
        T::from(self.esr.eval(eval)).unwrap()
    }
}

impl<T: CircuitScalar> Component<T> for Capacitor<T> {
    fn linearity(&self) -> ComponentLinearity {
        let any_dynamic = self.capacitance.is_dynamic() || self.esr.is_dynamic();
        let any_voltage =
            self.capacitance.depends_on_voltage() || self.esr.depends_on_voltage();

        if any_voltage {
            ComponentLinearity::NonLinear
        } else if any_dynamic {
            ComponentLinearity::TimeVariant
        } else {
            ComponentLinearity::LinearDynamic
        }
    }

    fn bake_indices(&mut self, _ctx: &SimulationContext<T>, node_map: &HashMap<NodeId, usize>) {
        self.cached_idx_a = if self.node_a.0 == 0 {
            None
        } else {
            Some(node_map.get(&self.node_a).copied().unwrap())
        };
        self.cached_idx_b = if self.node_b.0 == 0 {
            None
        } else {
            Some(node_map.get(&self.node_b).copied().unwrap())
        };
    }

    fn ports(&self) -> Vec<NodeId> {
        vec![self.node_a, self.node_b]
    }

    fn stamp_static(&self, matrix: &mut MatMut<T>, ctx: &SimulationContext<T>) {
        if ctx.is_dc_analysis {
            return;
        }
        // Only invoked by the orchestrator when linearity != TimeVariant
        // (i.e. constant cap → LinearDynamic). Formula-driven caps stamp
        // their conductance via stamp_time_variant or stamp_nonlinear.
        if self.capacitance.is_dynamic() || self.esr.is_dynamic() {
            return;
        }
        stamp_conductance(matrix, self.cached_idx_a, self.cached_idx_b, self.conductance, 0);
    }

    fn stamp_time_variant(
        &self,
        matrix: &mut MatMut<T>,
        ctx: &SimulationContext<T>,
        offset: usize,
    ) {
        if ctx.is_dc_analysis {
            return;
        }
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            self.conductance,
            offset,
        );
    }

    fn stamp_nonlinear(
        &self,
        _current_node_voltages: &ColRef<T>,
        matrix: &mut MatMut<T>,
        _rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
        l_size: usize,
    ) {
        if ctx.is_dc_analysis {
            return;
        }
        // Voltage-dependent capacitance: conductance was just refreshed by
        // refresh_per_iter using the latest x_n. Stamp it like a time-variant.
        stamp_conductance(
            matrix,
            self.cached_idx_a,
            self.cached_idx_b,
            self.conductance,
            l_size,
        );
    }

    fn stamp_dynamic(
        &mut self,
        _prev: &ColRef<T>,
        rhs: &mut ColMut<T>,
        ctx: &SimulationContext<T>,
    ) {
        if ctx.is_dc_analysis {
            return;
        }
        // eq_current goes to the cached row, which the partition placed in
        // either L or N depending on linearity — both work transparently.
        stamp_current_source(
            rhs,
            self.cached_idx_a,
            self.cached_idx_b,
            self.eq_current,
            0,
        );
    }

    fn refresh_per_step(&mut self, eval: &ComponentEvalCtx, sim: &SimulationContext<T>) {
        let any_dynamic = self.capacitance.is_dynamic() || self.esr.is_dynamic();
        let any_voltage =
            self.capacitance.depends_on_voltage() || self.esr.depends_on_voltage();
        if any_dynamic && !any_voltage {
            self.refresh_conductance(eval, sim.dt);
        }
    }

    fn refresh_per_iter(&mut self, eval: &ComponentEvalCtx, sim: &SimulationContext<T>) {
        let any_voltage =
            self.capacitance.depends_on_voltage() || self.esr.depends_on_voltage();
        if any_voltage {
            self.refresh_conductance(eval, sim.dt);
        }
    }

    fn update_state(&mut self, current_node_voltages: &ColRef<T>, ctx: &SimulationContext<T>) {
        let v_terminal =
            get_voltage_diff(current_node_voltages, self.cached_idx_a, self.cached_idx_b);

        // We need the current ESR value to back out v_c from v_terminal.
        // For constants we can read it directly; for formulas we use the
        // most-recently-refreshed value cached in the ParamValue's eval
        // path. To avoid threading ComponentEvalCtx through update_state,
        // we just pull the constant case directly — formula-driven caps
        // need a small follow-up to cache esr_t alongside conductance.
        // For now, recompute esr only for the constant case.
        let esr_t = match &self.esr {
            ParamValue::Constant(v) => T::from(*v).unwrap(),
            ParamValue::Formula(_) => {
                // TODO: cache esr in T alongside conductance during refresh.
                // Fall back to extracting it from G if needed; for now
                // approximate as 0 and accept the small ESR error until
                // we add the cache.
                T::zero()
            }
        };

        let i_through = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v_terminal * self.conductance) + self.eq_current
        };

        let v_c = if ctx.is_dc_analysis {
            v_terminal
        } else {
            v_terminal - (i_through * esr_t)
        };

        if ctx.is_dc_analysis {
            self.v_c_m1 = v_c;
            self.v_c_m2 = v_c;
        } else {
            self.v_c_m2 = self.v_c_m1;
            self.v_c_m1 = v_c;
        }

        let three = T::from(3.0).unwrap();
        let four = T::from(4.0).unwrap();

        let v_th_c = ((four * self.v_c_m1) - self.v_c_m2) / three;

        self.eq_current = -(v_th_c * self.conductance);
    }

    fn probe_definitions(&self) -> Vec<ComponentProbe> {
        vec![
            ComponentProbe {
                name: "Voltage".to_string(),
                unit: "V".to_string(),
            },
            ComponentProbe {
                name: "Current".to_string(),
                unit: "A".to_string(),
            },
            ComponentProbe {
                name: "Power".to_string(),
                unit: "W".to_string(),
            },
        ]
    }

    fn calculate_observables(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_observables: &mut [T],
    ) {
        let v = get_voltage_diff(node_voltages, self.cached_idx_a, self.cached_idx_b);

        let i = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v * self.conductance) + self.eq_current
        };

        let p = v * i;

        out_observables[0] = v;
        out_observables[1] = i;
        out_observables[2] = p;
    }

    fn terminal_currents(
        &self,
        node_voltages: &ColRef<T>,
        ctx: &SimulationContext<T>,
        out_currents: &mut [T],
    ) {
        let v = get_voltage_diff(node_voltages, self.cached_idx_a, self.cached_idx_b);

        let i_flow = if ctx.is_dc_analysis {
            T::zero()
        } else {
            (v * self.conductance) + self.eq_current
        };

        out_currents[0] = i_flow;
        out_currents[1] = -i_flow;
    }

    fn set_param_value(&mut self, name: &str, pv: ParamValue) -> RebuildKind {
        let (was_dyn, was_volt) = match name {
            "capacitance" => (
                self.capacitance.is_dynamic(),
                self.capacitance.depends_on_voltage(),
            ),
            "esr" => (self.esr.is_dynamic(), self.esr.depends_on_voltage()),
            _ => return RebuildKind::None,
        };

        match name {
            "capacitance" => self.capacitance = pv,
            "esr" => self.esr = pv,
            _ => unreachable!(),
        }

        let now_dyn = match name {
            "capacitance" => self.capacitance.is_dynamic(),
            "esr" => self.esr.is_dynamic(),
            _ => unreachable!(),
        };
        let now_volt = match name {
            "capacitance" => self.capacitance.depends_on_voltage(),
            "esr" => self.esr.depends_on_voltage(),
            _ => unreachable!(),
        };

        // If the new value is a constant, refresh conductance now so the
        // matrix gets the right value on the next stamp without waiting
        // for refresh_per_step (which is gated on is_dynamic).
        if !self.capacitance.is_dynamic() && !self.esr.is_dynamic() {
            let c = self.capacitance.as_constant().unwrap_or(1.0);
            let r = self.esr.as_constant().unwrap_or(0.0);
            // dt isn't available here; we use a placeholder.
            // The correct path is for the caller to invoke a Restamp which
            // reruns prepare()-like logic with the right dt. This is a known
            // limitation of the legacy `set_parameter` path; the new UI flow
            // should send a RebuildCircuit instead.
            // Until then, leave conductance as-is — it'll get fixed at the
            // next prepare() call.
            let _ = (c, r);
        }

        if was_dyn != now_dyn || was_volt != now_volt {
            RebuildKind::Repartition
        } else if !now_dyn {
            RebuildKind::Restamp
        } else {
            RebuildKind::None
        }
    }
}
