# Contributing to Copperhead

First off, thank you for your interest in Copperhead!

Whether you are fixing a bug, adding a new component model (like a new diode or vacuum tube), optimizing the Newton-Raphson solver, or improving the interface, your help is deeply appreciated.

Please read through this brief guide to understand how we manage contributions, licensing, and our development workflow.

---

## The Dual-License Model & CLA

To ensure the long-term sustainability of Copperhead, the project operates under a **Dual-License Model**:
1. **Open Source:** The engine is freely available under the **GPLv3** for open-source projects and hobbyists.
2. **Commercial:** We offer commercial licenses for companies embedding the engine into closed-source software or physical hardware.

### The Contributor License Agreement (CLA)
Because of this dual-license structure, we legally need your permission to distribute your code under both the open-source and commercial licenses.

Before we can merge your first Pull Request, you must sign our **Contributor License Agreement (CLA)**.
* **You still own your code:** Signing the CLA does *not* transfer your copyright. You still own your exact contributions and can use them anywhere else. You are simply granting Copperhead a permanent, irrevocable license to use and distribute your work.
* **It is fully automated:** You do not need to print or email any PDFs.


**How it works:**
1. You open a Pull Request.
2. Our `@cla-assistant` bot will automatically comment on your PR asking you to sign the CLA.
3. Click the link provided by the bot, sign in with your GitHub account, and click "I Agree".
4. The bot will turn green, and we can merge your code! You only have to do this once.

---

## How to Contribute

We use a standard GitHub flow for contributions.

### 1. Fork and Branch
* Fork the Copperhead repository to your own GitHub account.
* Create a new branch for your feature or bugfix:
  ```bash
  git checkout -b feature/added-xxx-tube-model
  ```

### 2. Make Your Changes
Copperhead is written in Rust. We ask that you adhere to standard Rust formatting and linting guidelines to keep the codebase clean.
* Use `cargo fmt` to format your code.
* Use `cargo clippy` to check for common mistakes and improve code quality.
* If you are adding a new solver optimization or component, please ensure you test it against standard circuit behaviours (e.g. by simulating the same circuit in LTspice and comparing results).
* In the future we will add unit tests for components and solvers, but for now, manual testing and validation is sufficient.

### 3. Open a Pull Request
* Push your branch to your fork.
* Open a PR against the `main` branch of the Copperhead repository.
* In your PR description, clearly explain what you changed, why you changed it, and any issues it fixes (e.g. "Fixes #123" to link to an issue). You don't have to write a novel.
* Sign the CLA when prompted by the bot.