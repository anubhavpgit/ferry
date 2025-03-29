mod codegen;
mod ir;
mod parser;
mod semantic;

use parser::parse_source;
use std::env;
use std::fs;
use std::process;
/*
   The main function serves as the entry point for the program.
   It checks if the user has provided a valid C file and processes it.
*/
fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];

    match filename.ends_with(".c") {
        true => match fs::read_to_string(filename) {
            Ok(contents) => match compile(contents, filename) {
                Ok(true) => {
                    process::exit(0);
                }
                Ok(false) => {
                    println!("Compilation failed");
                    process::exit(1);
                }
                Err(e) => {
                    println!("Compile error: {}", e);
                    process::exit(1);
                }
            },
            Err(e) => {
                eprintln!("Error reading file {}: {}", filename, e);
                process::exit(1);
            }
        },
        false => {
            println!("Ferry is a C compiler. Please provide a .c file.");
            process::exit(1);
        }
    }
}

/*
   Primary entry point for the compilation process.
   This function is responsible for compiling the provided C file.
   It takes the contents of the C file as input and returns a Result indicating success or failure.

   Arguments:
   - _file: The contents of the C file to be compiled.

   Returns:
   - Result<bool, String>: Indicates success or failure of the compilation process.

*/

fn compile(file: String, filename: &str) -> Result<bool, String> {
    // Parse the source code
    let mut ast = parse_source(&file)?; // Parse the source code into an AST
                                        // Perform semantic analysis
    ast = semantic::analyze_semantics(ast)?; // Perform semantic analysis on the AST
                                             // Generate code from the AST
    let ir = ir::generate_ir(&ast)?;
    // Assembly generation
    let assembly = codegen::generate_riscv_assembly(&ir)?;
    // Write the assembly to a file
    let output_file = format!("{}.s", &filename[..filename.len() - 2]);
    fs::write(&output_file, assembly).map_err(|e| format!("Error writing to file: {}", e))?;

    Ok(true) // Return true to indicate successful compilation
             // Note: In a real-world scenario, you would also handle linking and outputting the final executable.
}
