# Ferry - A Simple C to RISC-V Compiler

## Overview

Ferry is a basic C compiler written in Rust that compiles a subset of the C programming language to RISC-V assembly. It serves as an educational tool for understanding how compilers work by implementing the fundamental stages of compilation: lexical analysis, parsing, semantic analysis, and code generation.

## Installation

### Prerequisites

- Rust and Cargo (latest stable version recommended)
- For running the compiled assembly: RISC-V toolchain (optional)

### Building the Compiler

```bash
# Clone the repository
git clone https://github.com/yourusername/ferry.git
cd ferry

# Build the project
cargo build --release
```

## Usage

```bash
# Compile a C file to RISC-V assembly
./target/release/ferry input_file.c

# This will generate input_file.s in the same directory
```

### Running the Generated Assembly

For ARM64/macOS environments, you can use the included runner:

```bash
# Compile C to assembly
cargo run --release --bin ferry -- input_file.c

# Run the assembly using the provided runner
cargo run --release --bin runner -- input_file.s
```

Alternatively, you can use an external assembler like `riscv64-unknown-elf-gcc` or an online assembler like [RISC-V Online Assembler](https://riscvasm.lucasteske.dev/).

## Supported C Features

Ferry supports a subset of C language features:

- Basic arithmetic operations (+, -, *, /, %)
- Variable declarations and assignment
- Basic types (int, char, float, double)
- Control flow statements (if/else, while, for, do-while)
- Function declarations and calls
- Arrays (fixed-size only)
- Basic I/O functions
- Comments
- Pointers (basic operations)
- Type casting (implicit and explicit)
- Structs (without unions)
- String manipulation (basic operations)
- Basic preprocessor (#include)

## Project Structure

```
src/
├── main.rs              # Entry point and command-line handling
├── lib.rs               # Library exports
├── codegen.rs           # RISC-V assembly generation
├── ir/                  # Intermediate Representation
│   ├── mod.rs           # IR module exports
│   ├── generator.rs     # Generates IR from AST
│   ├── optimiser.rs     # IR optimization passes
│   └── types.rs         # IR node type definitions
├── semantic/            # Semantic analysis
│   ├── mod.rs           # Semantic analysis module exports
│   ├── preprocessor.rs  # Preprocessor directive handling
│   ├── symbol_table.rs  # Symbol table for variable tracking
│   └── type_checker.rs  # Type checking and validation
└── parser/              # Parser implementation
    ├── mod.rs           # Module declarations and exports
    ├── tokenizer.rs     # Lexical analyzer (tokenizer)
    ├── ast.rs           # Abstract Syntax Tree definitions
    ├── parser_impl.rs   # Core parser implementation
    ├── types.rs         # Type definitions
    ├── preprocessor.rs  # Preprocessor directive handling
    ├── expressions.rs   # Expression parsing
    ├── declarations.rs  # Declaration parsing
    └── statements.rs    # Statement parsing
```

## Compilation Pipeline

Ferry follows a standard compilation pipeline:

1. **Lexical Analysis**: Converts source code into tokens using `tokenizer.rs`
2. **Syntax Analysis**: Processes tokens to build an Abstract Syntax Tree (AST)
3. **Semantic Analysis**: Performs type checking and semantic validation
4. **Intermediate Representation**: Generates an optimizable IR from the AST
5. **Code Generation**: Generates RISC-V assembly code from the IR

Example flow for a simple "Hello, World!" program:

```c
#include <stdio.h>

int main() {
  printf("Hello, World!\n");
  return 0;
}
```

This goes through:
- Tokenization → Tokens like `Type(Int)`, `Identifier("main")`, `String("Hello, World!\n")`, etc.
- Parsing → AST with nodes for the function declaration, function call, and return statement
- Semantic Analysis → Validates the function call, type correctness, etc.
- IR Generation → Creates intermediate representation for optimization
- Code generation → RISC-V assembly code

## Contributing

Contributions are welcome! Ferry is designed as an educational tool, so improvements that make it more complete or easier to understand are particularly appreciated.

Potential areas for contribution:
- Additional C language features
- Improved error reporting
- Performance optimizations
- Better code generation
- Documentation improvements

## Future Work

Potential areas for enhancement:
- Supporting more C features
- Adding more advanced optimizations to the intermediate code
- Improving error reporting and recovery
- Integrating with LLVM for more robust code generation
- Extended platform support

## License

[MIT License]

## Acknowledgments

This project is created for educational purposes to understand compiler construction and RISC-V assembly. It draws inspiration from established compiler projects and educational resources.