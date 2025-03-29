use crate::ir::types::{IRNode, IRNodeType, IRType};

// Generates RISC-V assembly from intermediate representation
pub fn generate_riscv_assembly(ir: &IRNode) -> Result<String, String> {
    let mut context = CodeGenContext::new();

    // Start with data section for global variables and constants
    context.append(".section .data\n");

    // Generate code for global variables and constants
    generate_globals(&mut context, ir)?;

    // Switch to text section for code
    context.append(".section .text\n");
    context.append(".global main\n\n");
    context.append(".extern printf\n"); // Declare printf as external
    context.append(".global main\n\n"); // Ensure main is global

    // Generate code for the IR tree
    generate_code(&mut context, ir)?;

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
    var_offsets: std::collections::HashMap<String, i32>,
}

impl CodeGenContext {
    fn new() -> Self {
        CodeGenContext {
            asm_code: String::new(),
            temp_counter: 0,
            label_counter: 0,
            current_function: None,
            stack_offset: 0,
            var_offsets: std::collections::HashMap::new(),
        }
    }

    fn append(&mut self, code: &str) {
        self.asm_code.push_str(code);
    }

    fn finalize(self) -> String {
        self.asm_code
    }

    fn get_new_temp(&mut self) -> usize {
        let temp = self.temp_counter;
        self.temp_counter += 1;
        temp
    }

    fn get_new_label(&mut self) -> String {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    fn allocate_var(&mut self, name: &str, size: i32) -> i32 {
        self.stack_offset -= size;
        self.var_offsets.insert(name.to_string(), self.stack_offset);
        self.stack_offset
    }

    fn get_var_offset(&self, name: &str) -> Option<i32> {
        self.var_offsets.get(name).copied()
    }
}

// Generate code for global variables and constants
fn generate_globals(context: &mut CodeGenContext, ir: &IRNode) -> Result<(), String> {
    match ir.node_type {
        IRNodeType::Module => {
            for child in &ir.children {
                generate_globals(context, child)?;
            }
        }
        IRNodeType::Constant => {
            if let Some(value) = &ir.value {
                if value.starts_with("\"") && value.ends_with("\"") {
                    // Extract content without quotes
                    let content = &value[1..value.len() - 1];

                    // Create a properly escaped version for assembly
                    // We need to escape both actual newlines and any other special chars
                    let escaped_content = content
                        .replace('\\', "\\\\") // Escape backslashes first
                        .replace('\n', "\\n")
                        .replace('\r', "\\r")
                        .replace('\t', "\\t")
                        .replace('\"', "\\\""); // Escape quotes

                    let label = format!(".LC{}", context.label_counter);
                    context.label_counter += 1;

                    context.append(&format!(
                        "\n.section .rodata\n{}:\n    .string \"{}\"\n.section .text\n",
                        label, escaped_content
                    ));
                    context.append(&format!("    la {}, {}\n", label, label));
                }
            }
        }
        // Skip other node types during globals pass
        _ => {}
    }
    Ok(())
}

// Main code generation function that recursively processes the IR tree
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
            context.stack_offset = 0;
            context.var_offsets.clear();

            // Function label
            context.append(&format!("{}:\n", name));

            // Function prologue
            context.append("    # Prologue\n");
            context.append("    addi sp, sp, -8\n");
            context.append("    sw ra, 4(sp)\n");
            context.append("    sw s0, 0(sp)\n");
            context.append("    addi s0, sp, 8\n");

            // Process parameters
            let mut param_offset = 0;
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

                        context.allocate_var(param_name, param_size);

                        // Move parameter from argument register to stack
                        if param_offset < 8 {
                            context.append(&format!(
                                "    sw a{}, {}(s0)\n",
                                param_offset,
                                context.get_var_offset(param_name).unwrap()
                            ));
                        } else {
                            // Parameters after the 8th are passed on the stack
                            let caller_offset = (param_offset - 8) * 4;
                            context.append(&format!("    lw t0, {}(s0)\n", 16 + caller_offset));
                            context.append(&format!(
                                "    sw t0, {}(s0)\n",
                                context.get_var_offset(param_name).unwrap()
                            ));
                        }
                        param_offset += 1;
                    }
                }
            }

            // Find function body (should be a BasicBlock)
            for child in &ir.children {
                if child.node_type == IRNodeType::BasicBlock {
                    // Allocate stack space for local variables
                    if context.stack_offset < 0 {
                        let aligned_offset = ((-context.stack_offset + 15) & !15); // Align to 16 bytes
                        context.append(&format!("    addi sp, sp, -{}\n\n", aligned_offset));
                    }

                    // Generate code for function body
                    generate_code(context, child)?;
                    break;
                }
            }

            // Function epilogue (if not already generated by return statements)
            context.append("\n    # Epilogue\n");
            context.append("    lw ra, 4(sp)\n");
            context.append("    lw s0, 0(sp)\n");
            context.append("    addi sp, sp, 8\n");
            context.append("    ret\n\n");

            context.current_function = None;
            Ok(None)
        }

        IRNodeType::BasicBlock => {
            if let Some(label) = &ir.value {
                context.append(&format!("{}:\n", label));
            }

            for child in &ir.children {
                generate_code(context, child)?;
            }

            Ok(None)
        }

        IRNodeType::Variable => {
            // Handle variable reference - return the name for use by parent nodes
            Ok(ir.value.clone())
        }

        IRNodeType::Load => {
            if let Some(var_node) = ir.children.first() {
                if let Some(name) = generate_code(context, var_node)? {
                    if let Some(offset) = context.get_var_offset(&name) {
                        // Load local variable from stack to temporary register
                        let temp_reg = context.get_new_temp();
                        context.append(&format!("    lw t{}, {}(s0)\n", temp_reg, offset));
                        // Return the temporary register number as string
                        Ok(Some(format!("t{}", temp_reg)))
                    } else {
                        // Could be a global variable
                        let temp_reg = context.get_new_temp();
                        context.append(&format!("    la t{}, {}\n", temp_reg, name));
                        context.append(&format!("    lw t{}, 0(t{})\n", temp_reg, temp_reg));
                        Ok(Some(format!("t{}", temp_reg)))
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
                return Err("Store operation requires source and destination".to_string());
            }

            // First child is the value to store
            let value_reg =
                generate_code(context, &ir.children[0])?.ok_or("Missing value for store")?;

            // Second child is the destination variable
            if let Some(name) = generate_code(context, &ir.children[1])? {
                if let Some(offset) = context.get_var_offset(&name) {
                    // Store to local variable on stack
                    context.append(&format!("    sw {}, {}(s0)\n", value_reg, offset));
                } else {
                    // Store to global variable
                    let addr_reg = context.get_new_temp();
                    context.append(&format!("    la t{}, {}\n", addr_reg, name));
                    context.append(&format!("    sw {}, 0(t{})\n", value_reg, addr_reg));
                }
            } else {
                return Err("Invalid destination for store operation".to_string());
            }

            Ok(None)
        }

        IRNodeType::BinaryOp => {
            if ir.children.len() < 2 {
                return Err("Binary operation requires two operands".to_string());
            }

            let left_reg =
                generate_code(context, &ir.children[0])?.ok_or("Missing left operand")?;
            let right_reg =
                generate_code(context, &ir.children[1])?.ok_or("Missing right operand")?;

            let result_reg = context.get_new_temp();
            let op = ir.value.as_deref().unwrap_or("+");

            match op {
                "+" => context.append(&format!(
                    "    add t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "-" => context.append(&format!(
                    "    sub t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "*" => context.append(&format!(
                    "    mul t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "/" => context.append(&format!(
                    "    div t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "%" => context.append(&format!(
                    "    rem t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "==" => {
                    context.append(&format!(
                        "    xor t{}, {}, {}\n",
                        result_reg, left_reg, right_reg
                    ));
                    context.append(&format!("    seqz t{}, t{}\n", result_reg, result_reg));
                }
                "!=" => {
                    context.append(&format!(
                        "    xor t{}, {}, {}\n",
                        result_reg, left_reg, right_reg
                    ));
                    context.append(&format!("    snez t{}, t{}\n", result_reg, result_reg));
                }
                "<" => context.append(&format!(
                    "    slt t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                ">" => context.append(&format!(
                    "    slt t{}, {}, {}\n",
                    result_reg, right_reg, left_reg
                )),
                "<=" => {
                    context.append(&format!(
                        "    slt t{}, {}, {}\n",
                        result_reg, right_reg, left_reg
                    ));
                    context.append(&format!("    xori t{}, t{}, 1\n", result_reg, result_reg));
                }
                ">=" => {
                    context.append(&format!(
                        "    slt t{}, {}, {}\n",
                        result_reg, left_reg, right_reg
                    ));
                    context.append(&format!("    xori t{}, t{}, 1\n", result_reg, result_reg));
                }
                "&" => context.append(&format!(
                    "    and t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "|" => context.append(&format!(
                    "    or t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "^" => context.append(&format!(
                    "    xor t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                "<<" => context.append(&format!(
                    "    sll t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                ">>" => context.append(&format!(
                    "    srl t{}, {}, {}\n",
                    result_reg, left_reg, right_reg
                )),
                _ => return Err(format!("Unsupported binary operator: {}", op)),
            }

            Ok(Some(format!("t{}", result_reg)))
        }

        IRNodeType::UnaryOp => {
            if ir.children.is_empty() {
                return Err("Unary operation requires an operand".to_string());
            }

            let operand_reg = generate_code(context, &ir.children[0])?.ok_or("Missing operand")?;
            let result_reg = context.get_new_temp();
            let op = ir.value.as_deref().unwrap_or("!");

            match op {
                "-" => context.append(&format!("    neg t{}, {}\n", result_reg, operand_reg)),
                "!" => {
                    context.append(&format!("    seqz t{}, {}\n", result_reg, operand_reg));
                }
                "~" => context.append(&format!("    not t{}, {}\n", result_reg, operand_reg)),
                _ => return Err(format!("Unsupported unary operator: {}", op)),
            }

            Ok(Some(format!("t{}", result_reg)))
        }

        IRNodeType::Call => {
            // Push temporary registers that might be modified by the call
            let mut pushed_regs = 0;
            if context.temp_counter > 0 {
                for i in 0..context.temp_counter {
                    context.append(&format!("    sw t{}, -{}(sp)\n", i, (pushed_regs + 1) * 4));
                    pushed_regs += 1;
                }
                context.append(&format!("    addi sp, sp, -{}\n", pushed_regs * 4));
            }

            // Get function name
            let func_name = ir.value.clone().ok_or("Missing function name in call")?;

            // Evaluate and pass arguments (skip first child if it's the callee)
            let mut arg_regs = Vec::new();

            // Process all children as arguments
            for child in &ir.children {
                if let Some(reg) = generate_code(context, child)? {
                    arg_regs.push(reg);
                }
            }

            // Move arguments to parameter registers
            for (i, reg) in arg_regs.iter().enumerate() {
                if i < 8 {
                    context.append(&format!("    mv a{}, {}\n", i, reg));
                } else {
                    // Arguments beyond the 8th go on the stack
                    context.append(&format!("    addi sp, sp, -4\n"));
                    context.append(&format!("    sw {}, 0(sp)\n", reg));
                }
            }

            // Make the call
            context.append(&format!("    call {}\n", func_name));

            // Clean up stack if needed for arguments beyond the 8th
            if arg_regs.len() > 8 {
                context.append(&format!("    addi sp, sp, {}\n", (arg_regs.len() - 8) * 4));
            }

            // Restore temporary registers
            if pushed_regs > 0 {
                context.append(&format!("    addi sp, sp, {}\n", pushed_regs * 4));
                for i in 0..context.temp_counter {
                    context.append(&format!("    lw t{}, -{}(sp)\n", i, (pushed_regs - i) * 4));
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
            let end_label = context.get_new_label();

            // Branch if condition is false
            context.append(&format!("    beqz {}, {}\n", cond_reg, else_label));

            // Then block (second child)
            generate_code(context, &ir.children[1])?;
            context.append(&format!("    j {}\n", end_label));

            // Else block (third child, if present)
            context.append(&format!("{}:\n", else_label));
            if ir.children.len() > 2 {
                generate_code(context, &ir.children[2])?;
            }

            context.append(&format!("{}:\n", end_label));

            Ok(None)
        }

        IRNodeType::Jump => {
            let target = ir.value.clone().ok_or("Missing jump target")?;
            context.append(&format!("    j {}\n", target));
            Ok(None)
        }

        IRNodeType::Return => {
            // If there's a return value, move it to a0
            if let Some(value_node) = ir.children.first() {
                if let Some(reg) = generate_code(context, value_node)? {
                    // Move return value to a0
                    if reg != "a0" {
                        context.append(&format!("    mv a0, {}\n", reg));
                    }
                }
            }

            // Function epilogue (simplify to one consistent approach)
            if let Some(current_func) = &context.current_function {
                // Restore sp to frame pointer base
                context.append("    mv sp, s0\n");
                context.append("    addi sp, sp, -8\n"); // Point to saved registers

                // Restore saved registers
                context.append("    lw ra, 4(sp)\n");
                context.append("    lw s0, 0(sp)\n");

                // Final stack adjustment and return
                context.append("    addi sp, sp, 8\n");
                context.append("    ret\n");
            }

            Ok(None)
        }

        IRNodeType::Constant => {
            // Handle immediate constant
            let temp_reg = context.get_new_temp();
            if let Some(value) = &ir.value {
                if value.starts_with("\"") && value.ends_with("\"") {
                    // Extract content without quotes
                    let content = &value[1..value.len() - 1];

                    // Create a properly escaped version for assembly
                    // We need to escape both actual newlines and any other special chars
                    let escaped_content = content
                        .replace('\\', "\\\\") // Escape backslashes first
                        .replace('\n', "\\n")
                        .replace('\r', "\\r")
                        .replace('\t', "\\t")
                        .replace('\"', "\\\""); // Escape quotes

                    let label = format!(".LC{}", context.label_counter);
                    context.label_counter += 1;

                    context.append(&format!(
                        "\n.section .rodata\n{}:\n    .string \"{}\"\n.section .text\n",
                        label, escaped_content
                    ));

                    context.append(&format!("    la t{}, {}\n", temp_reg, label));
                } else {
                    // Numeric constant
                    context.append(&format!("    li t{}, {}\n", temp_reg, value));
                }
                Ok(Some(format!("t{}", temp_reg)))
            } else {
                Err("Constant missing value".to_string())
            }
        }

        IRNodeType::Alloca => {
            // Handle stack allocation for local variables
            if let Some(var_name) = &ir.value {
                // Determine size based on variable type, default to 4 bytes
                let var_size = match &ir.ty {
                    Some(IRType::Int32) | Some(IRType::Float) => 4,
                    Some(IRType::Int64) | Some(IRType::Double) => 8,
                    Some(IRType::Pointer(_)) => 4, // For RV32
                    _ => 4,                        // Default size
                };

                context.allocate_var(var_name, var_size);
                // No code generation needed for alloca itself - just track the variable
                Ok(None)
            } else {
                Err("Alloca instruction missing variable name".to_string())
            }
        }

        // Handle other IR node types as needed
        _ => Err(format!("Unsupported IR node type: {:?}", ir.node_type)),
    }
}
