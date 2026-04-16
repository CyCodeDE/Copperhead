# Project Context

Ask clarifying questions before making architectural changes.

## About This Project

Copperhead is a MNA and Newton-Raphson based circuit simulator for building and simulating guitar amplifiers and effect pedals in real-time.
It includes a schematic builder, a VST/ClAP Plugin, and the core solver.
It is not SPICE based and is written in Rust.

## Key Directories

- `copperhead_core`: The core solver and circuit simulation engine.
  - `/src/components/`: Contains the definitions and implementations of various circuit components (resistors, capacitors, transistors, etc.).
  - `/src/circuit.rs`: Contains the solver
- `copperhead_builder`: The schematic builder and user interface for designing circuits.
  - `/src/ui/`: Contains the user interface components and logic for the schematic builder.
  - `/src/simulation.rs`: Contains the logic for the simulation thread and interaction with the core solver.
- `copperhead_plugin`: The VST/ClAP plugin for real-time audio processing

## Standards

- Follow Rust's best practices and idiomatic code style.
- Ensure cleanliness and maintainability of code. Abstract complex logic into well-defined functions and modules.
- Write comments, but avoid over-commenting. Code should be self-explanatory where possible.
- Use descriptive variable and function names to enhance readability.