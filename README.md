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
*   **Solver:** Modified Nodal Analysis solver using Newton-Raphson iteration and matrix partitioning.
*   **Components:**
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
*   **Realtime Solver:** A JIT-compiled solver with lookup tables to test amps and pedals live with instrument input.
*   **Audio I/O:** Support for reading voltage sources from WAV files and rendering simulation output to WAV.
*   **Component Library:** A suite of prebuilt basic components and common audio circuit blocks.
*   **Data Export:** Export voltage readings and simulation data to text files.

### Installation & Usage
Clone the repository and run the simulator:
```bash
git clone https://github.com/CyCodeDE/Copperhead.git
cd Copperhead
cargo run --profile release-performance
```

### Increasing Performance
For development iteration, the standard `cargo run` is sufficient. However, simulation performance *might* be improved by using the custom profile configured in `Cargo.toml`.

```bash
cargo run --profile release-performance
```

The `--profile release-performance` flag optimizes for solver speed using Fat Link Time Optimization and reduced codegen units compared to the default release or dev builds.

## Contributing
We welcome community contributions!

Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file for detailed guidelines on how to contribute, including coding standards and testing procedures.

## License & Commercial Use

The core simulation engine is dual-licensed:
* **Open Source**: Available under the **GPL-3.0** license for free, open-source projects.
* **Indie Commercial (Free)**: If you are an independent creator or small business generating less than €20,000 EUR/year in gross revenue, you may use this engine in closed-source commercial software plugins for free. Please contact me to register your indie License.
* **Enterprise & Hardware (Paid)**: If your revenue exceeds €20,000 EUR/year, or if you intend to use the engine in commercial hardware products, reach out to cycode@cycode.eu to negotiate terms.

**Models / Schematics**
The ability to create and sell your own compiled schematics is completely unrestricted. You own your data.

However, the pre-built schematics included in this repository are licensed under **CC BY-NC 4.0**. You may not sell or commercially distribute the built-in schematics without explicit written permission.