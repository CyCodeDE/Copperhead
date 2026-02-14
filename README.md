# Copperhead

> **Note:** Copperhead is currently in a **pre-alpha** state. It is currently not recommended for general use. The software is under active development and currently tested on Linux only.

Copperhead is a circuit simulator written in Rust, designed specifically for modeling guitar amplifiers and effects pedals. It aims to provide a more intuitive user experience than traditional tools like LTspice by focusing on audio electronics.

## Goals

The primary goal of Copperhead is to bridge the gap between circuit design and audio testing.
*   **Intuitive Design:** Explicitly removes the need for SPICE directives.
*   **Audio Focus:** Built from the ground up to handle the specific needs of amp and pedal designers.
*   **Live Testing:** Future versions will allow for real-time simulation, enabling users to "play" through the circuit as they design it.

## Features

### Current Implementation
*   **Solver:** "Naive" MNA (Modified Nodal Analysis) solver using trapezoidal integration and Newton-Raphson iteration.
*   **Basic Components:**
    *   Resistors
    *   Inductors (uncoupled)
    *   Capacitors
    *   Diodes (currently the 1N4148 is the only preset available, more coming soon)
    *   Bipolar junction transistors
    *   Voltage Sources (AC and DC)
    *   Net labels
*   **GUI:** Immediate mode interface built with `egui`.
*   **Oscilloscope:** Allows measuring voltages between nodes and currents on components and scaling correctly.

### Planned Features
*   **Realtime Solver:** A JIT-compiled solver to test amps and pedals live with instrument input.
*   **Audio I/O:** Support for reading voltage sources from WAV files and rendering simulation output to WAV.
*   **Component Library:** A suite of prebuilt basic components and common audio circuit blocks.
*   **Data Export:** Export voltage readings and simulation data to text files.

### Running for Performance

For development iteration, the standard `cargo run` is sufficient. However, simulation performance might be improved by using the custom profile configured in `Cargo.toml`.

To run the simulator with Fat LTO (Link Time Optimization) and reduced codegen units:

```bash
cargo run --profile release-performance
```

This profile is optimized for solver speed compared to the default release or dev builds.

## License

Copperhead is licensed under [MIT](https://github.com/CyCodeDE/Copperhead/blob/main/LICENSE)







