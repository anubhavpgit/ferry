use crate::ir::types::{IRNode, IRNodeType, IRType};
use std::collections::{HashMap, HashSet};

// Generates RISC-V assembly from intermediate representation
pub fn generate_riscv_assembly(ir: &IRNode) -> Result<String, String> {
    let mut context = CodeGenContext::new();

    // Start with rodata section for string literals
    context.append(".section .rodata\n");

    // Generate data for string literals
    generate_string_literals(&mut context, ir)?;

    // Switch to text section for code
    context.append(".section .text\n");
    context.append(".global main\n");
    context.append(".extern printf\n\n");

    // Generate code for the IR tree
    generate_code(&mut context, ir)?;
    println!("\nAssembly code generated successfully\n");

    Ok(context.finalize())
}

// Context to keep track of code generation state
struct CodeGenContext {
    asm_code: String,
    temp_counter: usize,
    label_counter: usize,
    current_function: Option<String>,
    // Stack offset for local variables
    stack_offset: i32,
    // Map of variable names to stack offsets
    var_offsets: HashMap<String, i32>,
    // Loop context tracking
    loop_exit_labels: Vec<String>,
    // String literal tracking
    string_literals: HashMap<String, String>,
    // Register management
    reg_in_use: [bool; 7], // t0-t6
    // Track used labels to avoid generating duplicates or references to nonexistent labels
    used_labels: HashSet<String>,
}

impl CodeGenContext {
    fn new() -> Self {
        CodeGenContext {
            asm_code: String::new(),
            temp_counter: 0,
            label_counter: 0,
            current_function: None,
            stack_offset: 0,
            var_offsets: HashMap::new(),
            loop_exit_labels: Vec::new(),
            string_literals: HashMap::new(),
            reg_in_use: [false; 7],
            used_labels: HashSet::new(),
        }
    }

    fn push_loop_exit_label(&mut self, label: String) {
        self.loop_exit_labels.push(label.clone());
        self.used_labels.insert(label); // Track that we'll be using this label
    }

    fn pop_loop_exit_label(&mut self) -> Option<String> {
        self.loop_exit_labels.pop()
    }

    fn current_loop_exit_label(&self) -> Option<&String> {
        self.loop_exit_labels.last()
    }

    fn append(&mut self, code: &str) {
        self.asm_code.push_str(code);
    }

    fn finalize(self) -> String {
        self.asm_code
    }

    // Get an available register
    fn get_register(&mut self) -> usize {
        for i in 0..7 {
            if !self.reg_in_use[i] {
                self.reg_in_use[i] = true;
                return i;
            }
        }
        // If all registers are in use, we'll use t0 (could implement spilling here)
        0
    }

    // Release a register when done with it
    fn release_register(&mut self, reg: usize) {
        if reg < 7 {
            self.reg_in_use[reg] = false;
        }
    }

    fn get_new_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        self.used_labels.insert(label.clone()); // Track that we'll be generating this label
        label
    }
    // Allocate space relative to s0, returns the offset from s0.
    // Stack grows downwards, so offsets relative to s0 will be negative.
    // NOTE: This function now PRIMARILY manages the offset map.
    // The actual stack pointer adjustment is done ONCE in the Function handler.
    fn allocate_var(&mut self, name: &str, size: i32) -> i32 {
        // Calculate the next available offset. Offsets are negative relative to s0.
        // Find the current lowest offset used.
        let current_lowest_offset = self.var_offsets.values().min().copied().unwrap_or(0);
        // Place the new variable below the current lowest point.
        let new_offset = current_lowest_offset - size;
        // We might want alignment here too, depending on the variable type and ABI.
        // For simplicity, just stacking them now. Revisit alignment if needed.
        // Example alignment: new_offset = (current_lowest_offset - size) & !(size - 1);

        self.var_offsets.insert(name.to_string(), new_offset);

        // This function no longer directly updates self.stack_offset used for SP adjustment.
        // That calculation is done separately in the Function handler based on these offsets.
        new_offset
    }

    fn get_var_offset(&self, name: &str) -> Option<i32> {
        self.var_offsets.get(name).copied()
    }

    // Get or create string literal label
    fn get_string_literal(&mut self, content: &str) -> String {
        if let Some(label) = self.string_literals.get(content) {
            return label.clone();
        }

        let label = format!(".LC{}", self.label_counter);
        self.label_counter += 1;

        // Escape the string content
        let escaped_content = content
            .replace('\\', "\\\\")
            .replace('\n', "\\n")
            .replace('\r', "\\r")
            .replace('\t', "\\t")
            .replace('\"', "\\\"");

        // Add to .rodata section
        self.append(&format!(
            "{}:\n    .string \"{}\"\n",
            label, escaped_content
        ));

        // Remember this literal
        self.string_literals
            .insert(content.to_string(), label.clone());

        label
    }
}

// First pass: collect all string literals in the IR
fn generate_string_literals(context: &mut CodeGenContext, ir: &IRNode) -> Result<(), String> {
    match ir.node_type {
        IRNodeType::Constant => {
            if let Some(value) = &ir.value {
                if value.starts_with('\"') && value.ends_with('\"') {
                    // Extract content without quotes
                    let content = &value[1..value.len() - 1];

                    // Just register the string, actual generation happens at context creation
                    context.get_string_literal(content);
                }
            }
        }
        _ => {
            // Recursively process all children
            for child in &ir.children {
                generate_string_literals(context, child)?;
            }
        }
    }
    Ok(())
}

// Main code generation function
fn generate_code(context: &mut CodeGenContext, ir: &IRNode) -> Result<Option<String>, String> {
    match &ir.node_type {
        IRNodeType::Module => {
            for child in &ir.children {
                generate_code(context, child)?;
            }
            Ok(None)
        }

        IRNodeType::Function => {
            // Store function name in context
            let name = ir.value.clone().unwrap_or_else(|| "unknown".to_string());
            context.current_function = Some(name.clone());
            // Reset stack state for this function
            context.stack_offset = 0; // Reset offset calculation
            context.var_offsets.clear();

            // Function label
            context.append(&format!("{}:\n", name));

            // Function prologue (save ra, s0, set up s0)
            context.append("    # Prologue\n");
            context.append("    addi sp, sp, -8\n"); // Reserve space for ra, s0
            context.append("    sw ra, 4(sp)\n"); // Save ra
            context.append("    sw s0, 0(sp)\n"); // Save caller's s0
            context.append("    addi s0, sp, 8\n"); // Set s0 to point to base of current frame

            // --- FIX: Calculate local variable space FIRST ---
            let mut total_local_size = 0;
            // Assuming Alloca instructions are in the first basic block
            if let Some(entry_block) = ir
                .children
                .iter()
                .find(|c| c.node_type == IRNodeType::BasicBlock)
            {
                for instruction in &entry_block.children {
                    if instruction.node_type == IRNodeType::Alloca {
                        if let Some(var_name) = &instruction.value {
                            let var_size = match &instruction.ty {
                                Some(IRType::Int32) | Some(IRType::Float) => 4,
                                Some(IRType::Int64) | Some(IRType::Double) => 8,
                                Some(IRType::Pointer(_)) => 4, // For RV32
                                _ => 4,                        // Default size
                            };
                            // Calculate offset but DON'T adjust sp here yet
                            context.allocate_var(var_name, var_size);
                            // Keep track of the total size needed, based on the lowest offset
                            total_local_size = total_local_size.max(-context.stack_offset);
                        }
                    }
                }
            }
            // Reset stack_offset after calculation, as allocate_var modifies it
            // The var_offsets map now holds the correct offsets relative to s0
            context.stack_offset = 0; // Reset for actual stack pointer adjustment

            // --- FIX: Adjust stack pointer for ALL locals ---
            if total_local_size > 0 {
                // Align the total size to 16 bytes (typical stack alignment)
                let aligned_offset = (total_local_size + 15) & !15;
                context.append(&format!("    addi sp, sp, -{}\n", aligned_offset));
                // Update the stack_offset tracker to reflect the actual SP adjustment
                context.stack_offset = -aligned_offset;
            }
            context.append("\n"); // Add a newline for readability

            // Process parameters (store from argument registers/caller stack to our stack frame)
            // Note: Parameter handling might also need adjustment if they need stack space
            // distinct from locals calculated above, or if offsets need recalculating based
            // on the final frame layout. Assuming current parameter logic is okay for now.
            let mut param_offset = 0; // a0-a7 registers
            for child in &ir.children {
                if child.node_type == IRNodeType::Parameter {
                    if let Some(param_name) = &child.value {
                        // Determine size based on param type, default to 4 bytes
                        let param_size = match &child.ty {
                            Some(IRType::Int32) | Some(IRType::Float) => 4,
                            Some(IRType::Int64) | Some(IRType::Double) => 8,
                            Some(IRType::Pointer(_)) => 4, // For RV32
                            _ => 4,                        // Default size
                        };

                        // ***Revisiting Parameter Allocation***
                        // Parameters should ideally be allocated *before* local variables.
                        // Let's allocate parameter stack space *relative to s0* if needed,
                        // separate from the local variable stack pointer adjustment.
                        // Or, adjust the local variable allocation to account for parameters.
                        // For simplicity *now*, let's assume parameters are handled via
                        // registers or caller stack, and we just copy them to *our* allocated
                        // local variable space using `allocate_var`. The previous calculation
                        // of `total_local_size` should include space for these copies if
                        // `allocate_var` was called for parameters *before* the calculation.
                        // Let's ensure parameters also call allocate_var if they need stack space.

                        // *** MODIFICATION: Ensure parameters are associated with an offset ***
                        // We need space for parameters if we store them. Let's assume Alloca
                        // was generated for them too, and handled in the calculation above.
                        // If not, the `total_local_size` calculation needs to include parameters.

                        // Get the pre-calculated offset for the parameter storage
                        if let Some(stack_loc_offset) = context.get_var_offset(param_name) {
                            // Move parameter from argument register (a0-a7) or caller stack to our frame
                            if param_offset < 8 {
                                // Passed in registers a0-a7
                                context.append(&format!(
                                    "    # Store parameter {} from a{} to stack offset {}\n",
                                    param_name, param_offset, stack_loc_offset
                                ));
                                // Use sw for 32-bit, sd for 64-bit based on param_size
                                let store_instr = if param_size == 8 { "sd" } else { "sw" };
                                context.append(&format!(
                                    "    {} a{}, {}(s0)\n",
                                    store_instr, param_offset, stack_loc_offset
                                ));
                            } else {
                                // Parameters passed on the caller's stack (above our frame)
                                // s0 points to base of our frame. Saved ra is at s0-4, saved s0 is at s0-8.
                                // Caller's stack arguments start at s0+0, s0+4, etc. (or s0+0, s0+8 for 64-bit)
                                // Let's assume 32-bit arguments for simplicity here.
                                let caller_arg_offset = (param_offset - 8) * 4; // Offset relative to caller's frame base passed args

                                context.append(&format!(
                                     "    # Load parameter {} from caller stack (+{}) to stack offset {}\n",
                                     param_name, 8 + caller_arg_offset, stack_loc_offset // Offset from s0 (our frame base)
                                 ));
                                let reg = context.get_register();
                                // Use lw for 32-bit, ld for 64-bit
                                let load_instr = if param_size == 8 { "ld" } else { "lw" };
                                let store_instr = if param_size == 8 { "sd" } else { "sw" };
                                // Load from caller frame (relative to our s0)
                                context.append(&format!(
                                    "    {} t{}, {}(s0)\n",
                                    load_instr,
                                    reg,
                                    8 + caller_arg_offset
                                ));
                                // Store to our frame
                                context.append(&format!(
                                    "    {} t{}, {}(s0)\n",
                                    store_instr, reg, stack_loc_offset
                                ));
                                context.release_register(reg);
                            }
                        } else {
                            // This case should ideally not happen if Alloca was generated for parameters
                            return Err(format!(
                                "Parameter '{}' has no allocated stack space.",
                                param_name
                            ));
                        }
                        param_offset += 1;
                    }
                }
            }

            // Find and generate code for function body blocks
            let mut found_body = false;
            for child in &ir.children {
                if child.node_type == IRNodeType::BasicBlock {
                    found_body = true;
                    // Generate code for the block (Load, Store, Ops, etc.)
                    // Alloca nodes will be encountered again but only update the map (harmless)
                    generate_code(context, child)?;
                }
            }
            let has_return = ir.children.iter().any(|child| {
                if let IRNodeType::BasicBlock = child.node_type {
                    child
                        .children
                        .iter()
                        .any(|node| matches!(node.node_type, IRNodeType::Return))
                } else {
                    false
                }
            });
            if found_body && !has_return {
                // Generate a default epilogue
                context.append("\n    # Default Epilogue\n");

                // For main, set return value to 0
                if name == "main" {
                    context.append("    li a0, 0\n");
                }

                // Correct epilogue sequence
                context.append("    mv sp, s0\n");
                context.append("    addi sp, sp, -8\n");
                context.append("    lw ra, 4(sp)\n");
                context.append("    lw s0, 0(sp)\n");
                context.append("    addi sp, sp, 8\n");
                context.append("    ret\n");
            }

            // --- FIX: Remove redundant epilogue generation ---
            // The epilogue should *only* be generated by the Return node handler.
            // Ensure the IR guarantees a Return node terminates all function paths.
            // If function has no explicit return, add epilogue (REMOVED)
            // if found_body {
            //     let has_return = ir.children.iter().any(|child| {
            //         // ... (check logic) ...
            //     });
            //     if !has_return {
            //          context.append("\n    # Default Epilogue (Removed)\n");
            //          // ... epilogue code removed ...
            //     }
            // }

            context.current_function = None;
            Ok(None)
        }
        IRNodeType::BasicBlock => {
            // Check if this is a loop-related block
            if let Some(label) = &ir.value {
                // Only output the label if it hasn't been generated yet
                context.append(&format!("{}:\n", label));

                // If this is a loop body, set up exit label for break statements
                if label.contains("for.body")
                    || label.contains("while.body")
                    || label.contains("do.body")
                {
                    // Generate a label for the loop exit point
                    let exit_label = context.get_new_label();

                    // Track this label
                    context.push_loop_exit_label(exit_label.clone());

                    // Store reference to the exit label for later use
                    // We'll add this label after the loop
                }
            }

            // Process block statements
            for child in &ir.children {
                generate_code(context, child)?;
            }

            // If this was a loop body block, handle its end
            if let Some(label) = &ir.value {
                if label.contains("for.body")
                    || label.contains("while.body")
                    || label.contains("do.body")
                {
                    // Get the exit label we saved earlier
                    if let Some(exit_label) = context.current_loop_exit_label().cloned() {
                        // Pop the label from the stack before we finish with this block
                        context.pop_loop_exit_label();

                        // Define the exit label in the assembly - this is where break jumps to
                        context.append(&format!("{}:\n", exit_label));
                    }
                }
            }

            Ok(None)
        }

        IRNodeType::Variable => {
            // Return the variable name for use by parent nodes
            Ok(ir.value.clone())
        }

        IRNodeType::Load => {
            if let Some(var_node) = ir.children.first() {
                // Get the variable name (or intermediate result register)
                if let Some(name_or_reg) = generate_code(context, var_node)? {
                    // Check if it's a known local/parameter variable offset
                    if let Some(offset) = context.get_var_offset(&name_or_reg) {
                        let reg = context.get_register();
                        // Determine load instruction based on type (default to lw)
                        let load_instr = match var_node.ty {
                            Some(IRType::Int64) | Some(IRType::Double) => "ld", // Assuming RV64 target if used
                            _ => "lw",
                        };
                        context.append(&format!("    {} t{}, {}(s0)\n", load_instr, reg, offset));
                        Ok(Some(format!("t{}", reg)))
                    } else if name_or_reg.starts_with('t') || name_or_reg.starts_with('a') {
                        // If it's already a register (e.g., result of a previous op used as address), handle pointer load
                        // This part needs careful handling based on how pointer arithmetic is represented in IR.
                        // Assuming 'name_or_reg' holds the address:
                        let addr_reg = name_or_reg;
                        let value_reg = context.get_register();
                        let load_instr = match ir.ty {
                            // Load type depends on the Load instruction's type
                            Some(IRType::Int64) | Some(IRType::Double) => "ld",
                            _ => "lw",
                        };
                        context.append(&format!(
                            "    {} t{}, 0({})\n",
                            load_instr, value_reg, addr_reg
                        ));
                        // Maybe release addr_reg if it was temporary? Needs careful register lifetime tracking.
                        Ok(Some(format!("t{}", value_reg)))
                    } else {
                        // Assume global variable
                        let addr_reg = context.get_register();
                        context.append(&format!("    la t{}, {}\n", addr_reg, name_or_reg));
                        let value_reg = context.get_register();
                        let load_instr = match ir.ty {
                            Some(IRType::Int64) | Some(IRType::Double) => "ld",
                            _ => "lw",
                        };
                        context.append(&format!(
                            "    {} t{}, 0(t{})\n",
                            load_instr, value_reg, addr_reg
                        ));
                        context.release_register(addr_reg);
                        Ok(Some(format!("t{}", value_reg)))
                    }
                } else {
                    Err("Invalid variable reference in load operation".to_string())
                }
            } else {
                Err("Missing variable in load operation".to_string())
            }
        }

        IRNodeType::Store => {
            if ir.children.len() < 2 {
                return Err(
                    "Store operation requires value and destination address/variable".to_string(),
                );
            }

            // First child is the value to store
            let value_reg =
                generate_code(context, &ir.children[0])?.ok_or("Missing value for store")?;

            // Second child is the destination (variable or address)
            let dest_node = &ir.children[1];
            if let Some(dest_name_or_reg) = generate_code(context, dest_node)? {
                // Check if it's a known local/parameter variable offset
                if let Some(offset) = context.get_var_offset(&dest_name_or_reg) {
                    let store_instr = match ir.children[0].ty {
                        // Type of value being stored
                        Some(IRType::Int64) | Some(IRType::Double) => "sd",
                        _ => "sw",
                    };
                    context.append(&format!(
                        "    {} {}, {}(s0)\n",
                        store_instr, value_reg, offset
                    ));
                } else if dest_name_or_reg.starts_with('t') || dest_name_or_reg.starts_with('a') {
                    // Storing to an address held in a register (pointer store)
                    let addr_reg = dest_name_or_reg;
                    let store_instr = match ir.children[0].ty {
                        Some(IRType::Int64) | Some(IRType::Double) => "sd",
                        _ => "sw",
                    };
                    context.append(&format!(
                        "    {} {}, 0({})\n",
                        store_instr, value_reg, addr_reg
                    ));
                    // Maybe release addr_reg if temporary?
                } else {
                    // Assume global variable store
                    let addr_reg = context.get_register();
                    context.append(&format!("    la t{}, {}\n", addr_reg, dest_name_or_reg));
                    let store_instr = match ir.children[0].ty {
                        Some(IRType::Int64) | Some(IRType::Double) => "sd",
                        _ => "sw",
                    };
                    context.append(&format!(
                        "    {} {}, 0(t{})\n",
                        store_instr, value_reg, addr_reg
                    ));
                    context.release_register(addr_reg);
                }
            } else {
                return Err("Invalid destination for store operation".to_string());
            }

            // Release the value register if it's one of our t-registers
            if let Some(num_str) = value_reg.strip_prefix('t') {
                if let Ok(reg_num) = num_str.parse::<usize>() {
                    if reg_num < 7 {
                        context.release_register(reg_num);
                    }
                }
            }
            Ok(None)
        }

        IRNodeType::BinaryOp => {
            // ... (previous logic seems mostly ok, but ensure correct opcodes for types if needed)
            // Example: Use addw for 32-bit add on RV64 if types indicate Int32
            if ir.children.len() < 2 {
                return Err("Binary operation requires two operands".to_string());
            }

            let left_reg_str =
                generate_code(context, &ir.children[0])?.ok_or("Missing left operand")?;
            let right_reg_str =
                generate_code(context, &ir.children[1])?.ok_or("Missing right operand")?;

            let result_reg_idx = context.get_register();
            let result_reg_str = format!("t{}", result_reg_idx);
            let op = ir.value.as_deref().unwrap_or("+");

            // TODO: Consider using type information (ir.ty) to select appropriate instructions
            // (e.g., add vs addw, fadd.s vs fadd.d) if targeting RV64 or supporting floats.
            // Assuming integer operations for now.

            match op {
                "+" => context.append(&format!(
                    "    add {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )),
                "-" => context.append(&format!(
                    "    sub {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )),
                "*" => context.append(&format!(
                    "    mul {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )),
                "/" => context.append(&format!(
                    "    div {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )), // Signed division
                "%" => context.append(&format!(
                    "    rem {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )), // Signed remainder
                // Comparisons (result is 0 or 1)
                "==" => {
                    context.append(&format!(
                        "    sub {}, {}, {}\n",
                        result_reg_str, left_reg_str, right_reg_str
                    ));
                    context.append(&format!(
                        "    seqz {}, {}\n",
                        result_reg_str, result_reg_str
                    )); // Set if zero (equal)
                }
                "!=" => {
                    context.append(&format!(
                        "    sub {}, {}, {}\n",
                        result_reg_str, left_reg_str, right_reg_str
                    ));
                    context.append(&format!(
                        "    snez {}, {}\n",
                        result_reg_str, result_reg_str
                    )); // Set if not zero (not equal)
                }
                "<" => context.append(&format!(
                    "    slt {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )), // Set if less than (signed)
                ">" => context.append(&format!(
                    "    sgt {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )), // Set if greater than (signed) -> slt swapped operands
                "<=" => {
                    // a <= b is !(a > b) or (b >= a)
                    context.append(&format!(
                        "    sgt {}, {}, {}\n",
                        result_reg_str, left_reg_str, right_reg_str
                    )); // result = a > b
                    context.append(&format!(
                        "    xori {}, {}, 1\n",
                        result_reg_str, result_reg_str
                    )); // result = !(a > b)
                }
                ">=" => {
                    // a >= b is !(a < b)
                    context.append(&format!(
                        "    slt {}, {}, {}\n",
                        result_reg_str, left_reg_str, right_reg_str
                    )); // result = a < b
                    context.append(&format!(
                        "    xori {}, {}, 1\n",
                        result_reg_str, result_reg_str
                    )); // result = !(a < b)
                }
                // Bitwise operations
                "&" => context.append(&format!(
                    "    and {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )),
                "|" => context.append(&format!(
                    "    or {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )),
                "^" => context.append(&format!(
                    "    xor {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )),
                "<<" => context.append(&format!(
                    "    sll {}, {}, {}\n",
                    result_reg_str, left_reg_str, right_reg_str
                )), // Shift left logical
                ">>" => {
                    // Choose arithmetic (sra) or logical (srl) shift based on type?
                    // Defaulting to arithmetic shift for signed types, logical for unsigned?
                    // Assuming signed for now based on C behavior for >> on signed ints.
                    // If type info is available use it. Assuming sra for now.
                    context.append(&format!(
                        "    sra {}, {}, {}\n",
                        result_reg_str, left_reg_str, right_reg_str
                    )) // Shift right arithmetic
                }
                _ => return Err(format!("Unsupported binary operator: {}", op)),
            }

            // Release operand registers if temporary
            if let Some(num_str) = left_reg_str.strip_prefix('t') {
                if let Ok(reg_num) = num_str.parse::<usize>() {
                    if reg_num < 7 {
                        context.release_register(reg_num);
                    }
                }
            }
            if let Some(num_str) = right_reg_str.strip_prefix('t') {
                if let Ok(reg_num) = num_str.parse::<usize>() {
                    if reg_num < 7 {
                        context.release_register(reg_num);
                    }
                }
            }

            Ok(Some(result_reg_str))
        }
        IRNodeType::UnaryOp => {
            if ir.children.is_empty() {
                return Err("Unary operation requires an operand".to_string());
            }

            let operand_reg = generate_code(context, &ir.children[0])?.ok_or("Missing operand")?;
            let result_reg = context.get_register();
            let op = ir.value.as_deref().unwrap_or("!");

            match op {
                "-" => context.append(&format!("    neg t{}, {}\n", result_reg, operand_reg)),
                "!" => {
                    context.append(&format!("    seqz t{}, {}\n", result_reg, operand_reg));
                }
                "~" => context.append(&format!("    not t{}, {}\n", result_reg, operand_reg)),
                _ => return Err(format!("Unsupported unary operator: {}", op)),
            }

            // Release operand register if it's a temporary
            if let Some(num_str) = operand_reg.strip_prefix('t') {
                if let Ok(reg_num) = num_str.parse::<usize>() {
                    if reg_num < 7 {
                        context.release_register(reg_num);
                    }
                }
            }

            Ok(Some(format!("t{}", result_reg)))
        }

        IRNodeType::Call => {
            // Save all currently used registers
            let mut saved_regs = Vec::new();
            for i in 0..7 {
                if context.reg_in_use[i] {
                    saved_regs.push(i);
                }
            }

            // Push registers to save them
            if !saved_regs.is_empty() {
                for (i, reg) in saved_regs.iter().enumerate() {
                    context.append(&format!("    sw t{}, -{}(sp)\n", reg, (i + 1) * 4));
                }
                context.append(&format!("    addi sp, sp, -{}\n", saved_regs.len() * 4));
            }

            // Get function name
            let func_name = ir.value.clone().ok_or("Missing function name in call")?;

            // Special handling for printf/scanf
            if func_name == "printf" || func_name == "scanf" {
                // Reset register allocation for clean evaluation
                for i in 0..7 {
                    context.reg_in_use[i] = false;
                }

                // Process each argument
                for (i, child) in ir.children.iter().enumerate() {
                    if i == 0 && matches!(child.node_type, IRNodeType::Constant) {
                        // First argument - format string
                        if let Some(value) = &child.value {
                            if value.starts_with('\"') && value.ends_with('\"') {
                                let content = &value[1..value.len() - 1];
                                let label = context
                                    .string_literals
                                    .get(content)
                                    .cloned()
                                    .unwrap_or_else(|| context.get_string_literal(content));

                                context.append(&format!("    la a0, {}\n", label));
                                continue;
                            }
                        }
                    }

                    // Process other arguments
                    if let Some(reg) = generate_code(context, child)? {
                        context.append(&format!("    mv a{}, {}\n", i, reg));

                        // Release register if temporary
                        if let Some(num_str) = reg.strip_prefix('t') {
                            if let Ok(reg_num) = num_str.parse::<usize>() {
                                if reg_num < 7 {
                                    context.release_register(reg_num);
                                }
                            }
                        }
                    }
                }
            } else {
                // Normal function - evaluate arguments and move to argument registers
                for (i, child) in ir.children.iter().enumerate() {
                    if let Some(reg) = generate_code(context, child)? {
                        if i < 8 {
                            context.append(&format!("    mv a{}, {}\n", i, reg));
                        } else {
                            // Arguments beyond the 8th go on the stack
                            context.append("    addi sp, sp, -4\n");
                            context.append(&format!("    sw {}, 0(sp)\n", reg));
                        }

                        // Release register if temporary
                        if let Some(num_str) = reg.strip_prefix('t') {
                            if let Ok(reg_num) = num_str.parse::<usize>() {
                                if reg_num < 7 {
                                    context.release_register(reg_num);
                                }
                            }
                        }
                    }
                }
            }

            // Make the call
            context.append(&format!("    call {}\n", func_name));

            // Clean up stack if needed for arguments beyond the 8th
            if ir.children.len() > 8 {
                context.append(&format!(
                    "    addi sp, sp, {}\n",
                    (ir.children.len() - 8) * 4
                ));
            }

            // Restore saved registers
            if !saved_regs.is_empty() {
                context.append(&format!("    addi sp, sp, {}\n", saved_regs.len() * 4));
                for (i, reg) in saved_regs.iter().enumerate() {
                    context.append(&format!(
                        "    lw t{}, -{}(sp)\n",
                        reg,
                        (saved_regs.len() - i) * 4
                    ));
                    context.reg_in_use[*reg] = true; // Mark as in-use again
                }
            }

            // Return value is in a0
            Ok(Some("a0".to_string()))
        }

        IRNodeType::Branch => {
            if ir.children.len() < 2 {
                return Err("Branch requires condition and at least one target block".to_string());
            }

            let cond_reg = generate_code(context, &ir.children[0])?.ok_or("Missing condition")?;

            let else_label = context.get_new_label();
            let end_label = if ir.children.len() > 2 {
                context.get_new_label()
            } else {
                // If there's no else block, we don't need an end label
                String::new()
            };

            // Branch if condition is false
            context.append(&format!("    beqz {}, {}\n", cond_reg, else_label));

            // Release condition register
            if let Some(num_str) = cond_reg.strip_prefix('t') {
                if let Ok(reg_num) = num_str.parse::<usize>() {
                    if reg_num < 7 {
                        context.release_register(reg_num);
                    }
                }
            }

            // Then block (second child)
            generate_code(context, &ir.children[1])?;

            // Only add jump to end if we have an else block
            if ir.children.len() > 2 && !end_label.is_empty() {
                context.append(&format!("    j {}\n", end_label));
            }

            // Else block (third child, if present)
            context.append(&format!("{}:\n", else_label));
            if ir.children.len() > 2 {
                generate_code(context, &ir.children[2])?;
                if !end_label.is_empty() {
                    context.append(&format!("{}:\n", end_label));
                }
            }

            Ok(None)
        }

        IRNodeType::Jump => {
            let target = ir.value.clone().ok_or("Missing jump target")?;

            // Check if this is a break statement within a loop
            if target == "loop.exit" {
                if let Some(exit_label) = context.current_loop_exit_label() {
                    // Jump to the loop's exit label (where a break should go)
                    context.append(&format!("    j {}\n", exit_label));
                } else {
                    return Err("Break statement outside of loop".to_string());
                }
            } else if target == "for.header" || target.contains("header") {
                // This is a normal loop iteration - don't add unnecessary jumps
                context.append(&format!("    j {}\n", target));
            } else {
                // Regular jump
                context.append(&format!("    j {}\n", target));
            }

            Ok(None)
        }

        IRNodeType::Return => {
            // If there's a return value, move it to a0
            if let Some(value_node) = ir.children.first() {
                if let Some(reg) = generate_code(context, value_node)? {
                    if reg != "a0" {
                        // Avoid unnecessary move
                        context.append(&format!("    mv a0, {}\n", reg));
                    }
                    // Release register if temporary
                    if let Some(num_str) = reg.strip_prefix('t') {
                        if let Ok(reg_num) = num_str.parse::<usize>() {
                            if reg_num < 7 {
                                context.release_register(reg_num);
                            }
                        }
                    }
                }
            }

            // --- FIX: Generate EPILOGUE here ---
            context.append("\n    # Epilogue\n");
            // Use frame pointer to correctly restore stack pointer
            context.append("    mv sp, s0\n"); // Restore sp to frame base
            context.append("    addi sp, sp, -8\n"); // Point to saved registers
            context.append("    lw ra, 4(sp)\n"); // Load return address
            context.append("    lw s0, 0(sp)\n"); // Load saved frame pointer
            context.append("    addi sp, sp, 8\n"); // Final adjustment
            context.append("    ret\n"); // Return

            Ok(None)
        }

        IRNodeType::Constant => {
            if let Some(value) = &ir.value {
                let reg = context.get_register();

                if value.starts_with('\"') && value.ends_with('\"') {
                    // String constant
                    let content = &value[1..value.len() - 1];
                    let label = context
                        .string_literals
                        .get(content)
                        .cloned()
                        .unwrap_or_else(|| context.get_string_literal(content));

                    context.append(&format!("    la t{}, {}\n", reg, label));
                } else {
                    // Numeric constant
                    context.append(&format!("    li t{}, {}\n", reg, value));
                }

                Ok(Some(format!("t{}", reg)))
            } else {
                Err("Constant missing value".to_string())
            }
        }

        IRNodeType::Alloca => {
            // Handle stack allocation tracking FOR LOCAL VARIABLES
            if let Some(var_name) = &ir.value {
                // This check might be redundant if we pre-calculate in Function handler,
                // but it's harmless to update the map again.
                if !context.var_offsets.contains_key(var_name) {
                    let var_size = match &ir.ty {
                        Some(IRType::Int32) | Some(IRType::Float) => 4,
                        Some(IRType::Int64) | Some(IRType::Double) => 8,
                        Some(IRType::Pointer(_)) => 4, // For RV32
                        _ => 4,                        // Default size
                    };
                    // Just calculate and record the offset relative to s0.
                    // The actual 'sp' adjustment happens once in the Function prologue.
                    context.allocate_var(var_name, var_size);
                }
                // No actual assembly code is generated for Alloca itself.
                Ok(None)
            } else {
                Err("Alloca instruction missing variable name".to_string())
            }
        }

        // Other node types
        _ => Err(format!("Unsupported IR node type: {:?}", ir.node_type)),
    }
}
