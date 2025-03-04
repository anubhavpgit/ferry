# Ferry

A scaled-down C compiler simulator targeting RISC-V, demonstrated on Apple Silicon (Arm64).  
This project aims to introduce core compiler design concepts through:

- Parsing a subset of C
- Generating RISC-V instructions
- Simulating those instructions on Arm64

## Project Structure
- src/  
  - main.rs (entry point)  
  - parser.rs (handles parsing)  
  - codegen.rs (generates instructions)  
  - simulator.rs (executes RISC-V instructions on Arm64)  
- tests/ (contains integration tests)  
- Cargo.toml (project configuration)  

## Usage
1. Build: `cargo build`  
2. Run: `cargo run`  
3. Test: `cargo test`
