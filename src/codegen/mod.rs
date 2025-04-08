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
        // This should be extremely rare and is a fallback for extreme cases
        println!("WARNING: Register allocation exhausted, forcing use of t0");
        self.reg_in_use[0] = true;  // Mark t0 as in use to avoid double allocation
        0
    }

    // Release a register when done with it
    fn release_register(&mut self, reg: usize) {
        if reg < 7 {
            self.reg_in_use[reg] = false;
        }
    }
    
    // Helper to attempt to release a register from a register string
    fn try_release_register_str(&mut self, reg_str: &str) {
        if let Some(num_str) = reg_str.strip_prefix('t') {
            if let Ok(reg_num) = num_str.parse::<usize>() {
                if reg_num < 7 {
                    self.release_register(reg_num);
                }
            }
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
        // Find the current lowest offset used
        let current_lowest_offset = self.var_offsets.values().min().copied().unwrap_or(0);

        // Basic Alignment (adjust as needed for ABI)
        let alignment = match size {
            8 => 8, // Align 8-byte values to 8 bytes (for RV64 double/int64)
            _ => 4, // Align others to 4 bytes
        };
        // Ensure alignment is power of 2
        debug_assert!(alignment > 0 && (alignment & (alignment - 1) == 0));

        let new_offset_unaligned = current_lowest_offset - size;
        // Align downwards: (address) & !(alignment - 1)
        let new_offset = new_offset_unaligned & !(alignment - 1);

        self.var_offsets.insert(name.to_string(), new_offset);
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
            let name = ir.value.clone().unwrap_or_else(|| "unknown".to_string());
            context.current_function = Some(name.clone());
            context.stack_offset = 0;
            context.var_offsets.clear();

            context.append(&format!("{}:\n", name));

            // Prologue
            context.append("    # Prologue\n");
            context.append("    addi sp, sp, -8\n");
            context.append("    sw ra, 4(sp)\n");
            context.append("    sw s0, 0(sp)\n");
            context.append("    addi s0, sp, 8\n"); // s0 = Frame Base (old_sp)

            // Calculate local variable space
            let mut min_local_offset = 0;
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
                                Some(IRType::Pointer(_)) => 4,
                                _ => 4,
                            };
                            // Use allocate_var which handles alignment
                            let offset = context.allocate_var(var_name, var_size);
                            min_local_offset = min_local_offset.min(offset);
                        }
                    }
                }
            }

            let total_local_size = -min_local_offset; // Convert to positive size

            // Adjust stack pointer for local variables with proper alignment
            let mut aligned_offset = 0;
            if total_local_size > 0 {
                // Align to 16 bytes
                aligned_offset = (total_local_size + 15) & !15;
                if aligned_offset > 0 {
                    context.append(&format!("    addi sp, sp, -{}\n", aligned_offset));
                }
            }
            // Record the stack adjustment
            context.stack_offset = -aligned_offset;

            context.append("\n");

            // Process parameters
            let mut param_offset = 0;
            for child in &ir.children {
                if child.node_type == IRNodeType::Parameter {
                    // Existing parameter handling code
                }
            }

            // Generate code for function body blocks
            let mut found_body = false;
            for child in &ir.children {
                if child.node_type == IRNodeType::BasicBlock {
                    found_body = true;
                    generate_code(context, child)?;
                }
            }

            // Add default epilogue if no return was found
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
                context.append("\n    # Default Epilogue (No explicit return)\n");
                if name == "main" {
                    context.append("    li a0, 0\n"); // Implicit return 0 for main
                }
                
                // Standard Epilogue Sequence
                // Reset all temporary registers to ensure clean state
                for i in 0..7 {
                    context.reg_in_use[i] = false;
                }
                
                // Reset the stack frame properly
                context.append("    mv sp, s0\n");  // Set stack pointer back to frame pointer
                context.append("    addi sp, sp, -8\n");  // Adjust to access saved registers
                
                // Now restore saved registers
                context.append("    lw ra, 4(sp)\n"); // Restore ra
                context.append("    lw s0, 0(sp)\n"); // Restore s0
                context.append("    addi sp, sp, 8\n"); // Final adjustment
                context.append("    ret\n");
            }

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
                            Some(IRType::Int64) | Some(IRType::Double) => "ld", // For RV64
                            _ => "lw",
                        };
                        context.append(&format!("    {} t{}, {}(s0)\n", load_instr, reg, offset));
                        Ok(Some(format!("t{}", reg)))
                    } else if name_or_reg.starts_with('t') || name_or_reg.starts_with('a') {
                        // Loading from an address in a register
                        let addr_reg = name_or_reg;
                        let value_reg = context.get_register();
                        let load_instr = match ir.ty {
                            Some(IRType::Int64) | Some(IRType::Double) => "ld",
                            _ => "lw",
                        };
                        context.append(&format!(
                            "    {} t{}, 0({})\n",
                            load_instr, value_reg, addr_reg
                        ));
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
                        Some(IRType::Int64) | Some(IRType::Double) => "sd",
                        _ => "sw",
                    };
                    context.append(&format!(
                        "    {} {}, {}(s0)\n",
                        store_instr, value_reg, offset
                    ));
                } else if dest_name_or_reg.starts_with('t') || dest_name_or_reg.starts_with('a') {
                    // Storing to an address held in a register
                    let addr_reg = dest_name_or_reg;
                    let store_instr = match ir.children[0].ty {
                        Some(IRType::Int64) | Some(IRType::Double) => "sd",
                        _ => "sw",
                    };
                    context.append(&format!(
                        "    {} {}, 0({})\n",
                        store_instr, value_reg, addr_reg
                    ));
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

            // Release the value register if it's one of our temporary registers
            context.try_release_register_str(&value_reg);
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
            context.try_release_register_str(&left_reg_str);
            context.try_release_register_str(&right_reg_str);

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
            context.try_release_register_str(&operand_reg);

            Ok(Some(format!("t{}", result_reg)))
        }

        IRNodeType::Call => {
            // Clear any existing register tracking before we make the call
            // This prevents leaked registers from affecting other parts of the code
            context.append("    # Preparing for function call\n");
            
            // Save all currently used registers
            let mut saved_regs = Vec::new();
            for i in 0..7 {
                if context.reg_in_use[i] {
                    saved_regs.push(i);
                }
            }

            // Push registers to save them
            if !saved_regs.is_empty() {
                context.append("    # Save used registers\n");
                // First allocate space on stack
                context.append(&format!("    addi sp, sp, -{}\n", saved_regs.len() * 4));
                
                // Then store registers
                for (i, reg) in saved_regs.iter().enumerate() {
                    context.append(&format!("    sw t{}, {}(sp)\n", reg, i * 4));
                }
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
                        context.try_release_register_str(&reg);
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
                        context.try_release_register_str(&reg);
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
                context.append("    # Restore saved registers\n");
                
                // Load the registers back
                for (i, reg) in saved_regs.iter().enumerate() {
                    context.append(&format!("    lw t{}, {}(sp)\n", reg, i * 4));
                    context.reg_in_use[*reg] = true; // Mark as in-use again
                }
                
                // Free the stack space
                context.append(&format!("    addi sp, sp, {}\n", saved_regs.len() * 4));
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

            // Release condition register if it's a temporary
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
            if target == "loop.exit" {
                if let Some(exit_label) = context.current_loop_exit_label() {
                    context.append(&format!("    j {}\n", exit_label)); // Jump to specific loop exit label
                } else {
                    return Err("Break statement outside of loop".to_string());
                }
            } else {
                context.append(&format!("    j {}\n", target)); // Normal jump
            }
            Ok(None)
        }

        IRNodeType::Return => {
            // If there's a return value, move it to a0
            if let Some(value_node) = ir.children.first() {
                if let Some(reg) = generate_code(context, value_node)? {
                    // Avoid unnecessary move if already in a0
                    if reg != "a0" {
                        context.append(&format!("    mv a0, {}\n", reg));
                    }

                    // Release register if temporary
                    context.try_release_register_str(&reg);
                }
            }

            // Generate proper epilogue
            context.append("\n    # Epilogue\n");
            
            // Reset all temporary registers to ensure clean state
            for i in 0..7 {
                context.reg_in_use[i] = false;
            }
            
            // Reset the stack frame to where it was in the prologue
            context.append("    mv sp, s0\n");  // Set stack pointer back to frame pointer
            context.append("    addi sp, sp, -8\n");  // Adjust to access saved registers
            
            // Now restore the saved registers
            context.append("    lw ra, 4(sp)\n"); // Restore ra
            context.append("    lw s0, 0(sp)\n"); // Restore s0
            context.append("    addi sp, sp, 8\n"); // Final adjustment for saved regs
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
