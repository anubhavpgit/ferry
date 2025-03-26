use crate::ir::types::{IRNode, IRNodeType, IRType};
use std::collections::{HashMap, HashSet};

pub fn optimise_ir(ir: IRNode) -> Result<IRNode, String> {
    println!("\n---------Starting Optimization---------");

    // Create optimization pipeline
    let mut optimized = ir;
    let mut changed = true;
    let max_iterations = 5; // Prevent infinite loops in fixed-point iteration
    let mut iteration = 0;

    // Apply optimizations repeatedly until no more changes
    while changed && iteration < max_iterations {
        iteration += 1;
        println!("Optimization iteration {}", iteration);
        changed = false;

        // 1. Constant Folding and Propagation
        let (new_ir, fold_changed) = constant_folding_and_propagation(optimized)?;
        optimized = new_ir;
        changed |= fold_changed;

        // 2. Dead Code Elimination
        let (new_ir, dce_changed) = eliminate_dead_code(optimized)?;
        optimized = new_ir;
        changed |= dce_changed;

        // 3. Common Subexpression Elimination
        let (new_ir, cse_changed) = common_subexpression_elimination(optimized)?;
        optimized = new_ir;
        changed |= cse_changed;

        // 4. Loop Optimizations
        let (new_ir, loop_changed) = optimize_loops(optimized)?;
        optimized = new_ir;
        changed |= loop_changed;
    }

    println!("Optimization complete after {} iterations", iteration);
    Ok(optimized)
}

// Constant folding and propagation combined for better results
fn constant_folding_and_propagation(node: IRNode) -> Result<(IRNode, bool), String> {
    // Track constants and changed status
    let mut const_map: HashMap<String, Option<i32>> = HashMap::new();
    let mut changed = false;

    // Don't propagate constants in loops
    let loop_vars = identify_loop_variables(&node);

    // First pass: Collect constant assignments outside loops
    collect_constants(&node, &mut const_map, &loop_vars);

    // Second pass: Fold constants and propagate values
    let result = fold_and_propagate(node, &const_map, &mut changed)?;

    Ok((result, changed))
}

// Identify variables modified in loops to avoid treating them as constants
fn identify_loop_variables(node: &IRNode) -> HashSet<String> {
    let mut loop_vars = HashSet::new();

    // Look for loop blocks
    match node.node_type {
        IRNodeType::BasicBlock => {
            if let Some(name) = &node.value {
                if name.contains("for") || name.contains("while") {
                    // Found a loop - collect all variables modified in it
                    find_modified_vars(node, &mut loop_vars);
                }
            }
        }
        _ => {}
    }

    // Recursively check children
    for child in &node.children {
        let child_loop_vars = identify_loop_variables(child);
        loop_vars.extend(child_loop_vars);
    }

    loop_vars
}

// Find variables that are modified (written to) in a node and its children
fn find_modified_vars(node: &IRNode, vars: &mut HashSet<String>) {
    match node.node_type {
        IRNodeType::Store => {
            // Find the variable being written to
            if node.children.len() > 1 {
                if let IRNodeType::Variable = node.children[1].node_type {
                    if let Some(var_name) = &node.children[1].value {
                        vars.insert(var_name.clone());
                    }
                }
            }
        }
        _ => {}
    }

    // Recursively check children
    for child in &node.children {
        find_modified_vars(child, vars);
    }
}

// Collect constant assignments throughout the code
fn collect_constants(
    node: &IRNode,
    const_map: &mut HashMap<String, Option<i32>>,
    loop_vars: &HashSet<String>,
) {
    match node.node_type {
        IRNodeType::Store => {
            if node.children.len() == 2 {
                // Check if storing to a variable that's not modified in a loop
                if let IRNodeType::Variable = node.children[1].node_type {
                    if let Some(var_name) = &node.children[1].value {
                        if !loop_vars.contains(var_name) {
                            // Only track constants for non-loop variables
                            if let IRNodeType::Constant = node.children[0].node_type {
                                if let Some(const_val) = &node.children[0].value {
                                    if let Ok(num) = const_val.parse::<i32>() {
                                        const_map.insert(var_name.clone(), Some(num));
                                    }
                                }
                            }
                        } else {
                            // For loop variables, explicitly mark as non-constant
                            const_map.insert(var_name.clone(), None);
                        }
                    }
                }
            }
        }
        _ => {}
    }

    // Recursively process children
    for child in &node.children {
        collect_constants(child, const_map, loop_vars);
    }
}

// Check if expression is a constant or consists only of constants
fn is_constant_expr(node: &IRNode) -> bool {
    match node.node_type {
        IRNodeType::Constant => true,
        IRNodeType::BinaryOp | IRNodeType::UnaryOp => {
            node.children.iter().all(|child| is_constant_expr(child))
        }
        _ => false,
    }
}

// Evaluate constant expressions
fn evaluate_constant_expr(node: &IRNode) -> Option<i32> {
    match node.node_type {
        IRNodeType::Constant => {
            if let Some(val_str) = &node.value {
                val_str.parse::<i32>().ok()
            } else {
                None
            }
        }
        IRNodeType::BinaryOp => {
            if node.children.len() != 2 {
                return None;
            }

            let op = node.value.as_deref().unwrap_or("");
            let left = evaluate_constant_expr(&node.children[0])?;
            let right = evaluate_constant_expr(&node.children[1])?;

            match op {
                "+" => Some(left + right),
                "-" => Some(left - right),
                "*" => Some(left * right),
                "/" => {
                    if right != 0 {
                        Some(left / right)
                    } else {
                        None
                    }
                }
                "%" => {
                    if right != 0 {
                        Some(left % right)
                    } else {
                        None
                    }
                }
                "==" => Some(if left == right { 1 } else { 0 }),
                "!=" => Some(if left != right { 1 } else { 0 }),
                "<" => Some(if left < right { 1 } else { 0 }),
                "<=" => Some(if left <= right { 1 } else { 0 }),
                ">" => Some(if left > right { 1 } else { 0 }),
                ">=" => Some(if left >= right { 1 } else { 0 }),
                "&&" => Some(if left != 0 && right != 0 { 1 } else { 0 }),
                "||" => Some(if left != 0 || right != 0 { 1 } else { 0 }),
                "&" => Some(left & right),
                "|" => Some(left | right),
                "^" => Some(left ^ right),
                "<<" => Some(left << right),
                ">>" => Some(left >> right),
                _ => None,
            }
        }
        IRNodeType::UnaryOp => {
            if node.children.len() != 1 {
                return None;
            }

            let op = node.value.as_deref().unwrap_or("");
            let operand = evaluate_constant_expr(&node.children[0])?;

            match op {
                "-" => Some(-operand),
                "!" => Some(if operand == 0 { 1 } else { 0 }),
                "~" => Some(!operand),
                _ => None,
            }
        }
        _ => None,
    }
}

// Fold constants and propagate values throughout the IR
fn fold_and_propagate(
    node: IRNode,
    const_map: &HashMap<String, Option<i32>>,
    changed: &mut bool,
) -> Result<IRNode, String> {
    let mut result = node.clone();

    // Handle node types specific to constant folding
    match &result.node_type {
        IRNodeType::BinaryOp => {
            // First fold children recursively
            for i in 0..result.children.len() {
                result.children[i] =
                    fold_and_propagate(result.children[i].clone(), const_map, changed)?;
            }

            // Check if operation can be folded
            if is_constant_expr(&result) {
                if let Some(val) = evaluate_constant_expr(&result) {
                    println!("Folded binary op to constant: {}", val);
                    *changed = true;

                    // Create new constant node with appropriate type
                    let ty = result.ty.clone();
                    return Ok(IRNode::new(IRNodeType::Constant, Some(val.to_string()))
                        .with_type(ty.unwrap_or(IRType::Int32)));
                }
            }
        }
        IRNodeType::UnaryOp => {
            // First fold children recursively
            for i in 0..result.children.len() {
                result.children[i] =
                    fold_and_propagate(result.children[i].clone(), const_map, changed)?;
            }

            // Check if operation can be folded
            if is_constant_expr(&result) {
                if let Some(val) = evaluate_constant_expr(&result) {
                    println!("Folded unary op to constant: {}", val);
                    *changed = true;

                    // Create new constant node with appropriate type
                    let ty = result.ty.clone();
                    return Ok(IRNode::new(IRNodeType::Constant, Some(val.to_string()))
                        .with_type(ty.unwrap_or(IRType::Int32)));
                }
            }
        }
        IRNodeType::Load => {
            // First process child nodes
            for i in 0..result.children.len() {
                result.children[i] =
                    fold_and_propagate(result.children[i].clone(), const_map, changed)?;
            }

            // Check if loading a known constant variable
            if let Some(var_node) = result.children.first() {
                if let IRNodeType::Variable = var_node.node_type {
                    if let Some(var_name) = &var_node.value {
                        if let Some(const_val) = const_map.get(var_name) {
                            println!(
                                "Replaced load of {} with constant {:?}",
                                var_name, const_val
                            );
                            *changed = true;

                            // Create constant node with appropriate type
                            let ty = result.ty.clone();
                            if let Some(val) = const_val {
                                return Ok(IRNode::new(
                                    IRNodeType::Constant,
                                    Some(val.to_string()),
                                )
                                .with_type(ty.unwrap_or(IRType::Int32)));
                            } else {
                                // Handle None case
                                return Ok(result);
                            }
                        }
                    }
                }
            }
        }
        _ => {
            // Recursively process all children
            for i in 0..result.children.len() {
                result.children[i] =
                    fold_and_propagate(result.children[i].clone(), const_map, changed)?;
            }
        }
    }

    Ok(result)
}

// Enhanced Dead Code Elimination
fn eliminate_dead_code(node: IRNode) -> Result<(IRNode, bool), String> {
    // First pass: find all used variables
    let mut used_vars = HashSet::new();
    let mut effect_instrs = HashSet::new(); // Instructions with side effects

    find_used_variables(&node, &mut used_vars, &mut effect_instrs);

    // Second pass: remove unused code
    let (result, changed) = remove_dead_code(node, &used_vars, &effect_instrs)?;

    Ok((result, changed))
}

fn find_used_variables(
    node: &IRNode,
    used_vars: &mut HashSet<String>,
    effect_instrs: &mut HashSet<String>,
) {
    // Instructions with side effects must be kept regardless of used variables
    match node.node_type {
        IRNodeType::Call => {
            // Function calls have side effects
            let fn_id = format!("{:?}", node);
            effect_instrs.insert(fn_id);
        }
        IRNodeType::Return => {
            // Returns must be kept
            let ret_id = format!("{:?}", node);
            effect_instrs.insert(ret_id);

            // Variables used in return value are live
            for child in &node.children {
                find_live_variables(child, used_vars);
            }
        }
        IRNodeType::Store => {
            // Check if storing to a used variable
            if node.children.len() > 1 {
                if let IRNodeType::Variable = node.children[1].node_type {
                    if let Some(var_name) = &node.children[1].value {
                        if used_vars.contains(var_name) {
                            // If storing to a used variable, the value expression is live
                            find_live_variables(&node.children[0], used_vars);
                        }
                    }
                }
            }
        }
        IRNodeType::Branch => {
            // Branch conditions affect control flow
            let branch_id = format!("{:?}", node);
            effect_instrs.insert(branch_id);

            // Variables in branch condition are live
            if !node.children.is_empty() {
                find_live_variables(&node.children[0], used_vars);
            }
        }
        _ => {}
    }

    // Recursively process children
    for child in &node.children {
        find_used_variables(child, used_vars, effect_instrs);
    }
}

fn find_live_variables(node: &IRNode, used_vars: &mut HashSet<String>) {
    match node.node_type {
        IRNodeType::Variable => {
            if let Some(var_name) = &node.value {
                used_vars.insert(var_name.clone());
            }
        }
        IRNodeType::Load => {
            // Variable being loaded is used
            if let Some(var_node) = node.children.first() {
                if let IRNodeType::Variable = var_node.node_type {
                    if let Some(var_name) = &var_node.value {
                        used_vars.insert(var_name.clone());
                    }
                }
            }
        }
        _ => {
            // Recursively check all children
            for child in &node.children {
                find_live_variables(child, used_vars);
            }
        }
    }
}

fn remove_dead_code(
    node: IRNode,
    used_vars: &HashSet<String>,
    effect_instrs: &HashSet<String>,
) -> Result<(IRNode, bool), String> {
    let mut result = node.clone();
    let mut changed = false;

    // PRESERVE THE MAIN FUNCTION
    if let IRNodeType::Function = result.node_type {
        if let Some(func_name) = &result.value {
            if func_name == "main" {
                // For main function, preserve its basic structure
                for i in 0..result.children.len() {
                    let (new_child, child_changed) =
                        remove_dead_code(result.children[i].clone(), used_vars, effect_instrs)?;
                    result.children[i] = new_child;
                    changed |= child_changed;
                }
                // Keep the main function's structure intact
                return Ok((result, changed));
            }
        }
    }

    // NEVER ELIMINATE RETURN OR BRANCH NODES
    if matches!(
        result.node_type,
        IRNodeType::Return | IRNodeType::Branch | IRNodeType::Jump
    ) {
        // Process children but preserve the node itself
        for i in 0..result.children.len() {
            let (new_child, child_changed) =
                remove_dead_code(result.children[i].clone(), used_vars, effect_instrs)?;
            result.children[i] = new_child;
            changed |= child_changed;
        }
        return Ok((result, changed));
    }

    // MARK ANY BLOCK CONTAINING CONTROL FLOW AS IMPORTANT
    if let IRNodeType::BasicBlock = result.node_type {
        // Check if this block or any nested block contains control flow
        let has_control_flow = contains_control_flow(&result);

        if has_control_flow {
            // If it contains control flow, preserve the block's structure
            for i in 0..result.children.len() {
                let (new_child, child_changed) =
                    remove_dead_code(result.children[i].clone(), used_vars, effect_instrs)?;
                result.children[i] = new_child;
                changed |= child_changed;
            }
            return Ok((result, changed));
        } else {
            // For blocks without control flow, apply normal DCE rules
            let original_len = result.children.len();
            result.children = result
                .children
                .into_iter()
                .filter(|child| should_keep_instruction(child, used_vars, effect_instrs))
                .collect();
            changed = result.children.len() != original_len;
        }
    }

    // Process remaining children recursively
    for i in 0..result.children.len() {
        let (new_child, child_changed) =
            remove_dead_code(result.children[i].clone(), used_vars, effect_instrs)?;
        result.children[i] = new_child;
        changed |= child_changed;
    }

    Ok((result, changed))
}

// Helper function to check if a node or any of its children contain control flow
fn contains_control_flow(node: &IRNode) -> bool {
    matches!(
        node.node_type,
        IRNodeType::Return | IRNodeType::Branch | IRNodeType::Jump
    ) || node
        .children
        .iter()
        .any(|child| contains_control_flow(child))
}

// Helper function to decide if an instruction should be kept
fn should_keep_instruction(
    instr: &IRNode,
    used_vars: &HashSet<String>,
    effect_instrs: &HashSet<String>,
) -> bool {
    let instr_id = format!("{:?}", instr);

    match instr.node_type {
        // Always keep control flow instructions
        IRNodeType::Return | IRNodeType::Branch | IRNodeType::Jump | IRNodeType::Call => true,

        // Keep allocations for used variables
        IRNodeType::Alloca => {
            if let Some(var_name) = &instr.value {
                used_vars.contains(var_name)
            } else {
                true // If no name, conservatively keep it
            }
        }

        // Keep stores to used variables
        IRNodeType::Store => {
            if instr.children.len() > 1 {
                if let IRNodeType::Variable = instr.children[1].node_type {
                    if let Some(var_name) = &instr.children[1].value {
                        used_vars.contains(var_name)
                    } else {
                        true
                    }
                } else {
                    true
                }
            } else {
                true
            }
        }

        // Keep instructions with side effects or that are part of a basic block
        IRNodeType::BasicBlock => contains_control_flow(instr) || !instr.children.is_empty(),

        // For all other instructions, keep if they have side effects
        _ => effect_instrs.contains(&instr_id),
    }
}
// Common Subexpression Elimination with value numbering
fn common_subexpression_elimination(node: IRNode) -> Result<(IRNode, bool), String> {
    let mut expr_map = HashMap::new();
    let mut changed = false;

    // Apply CSE to the IR
    let result = apply_cse(node, &mut expr_map, &mut changed)?;

    Ok((result, changed))
}

// Create a unique key for an expression to identify duplicates
fn expression_key(node: &IRNode) -> String {
    match node.node_type {
        IRNodeType::BinaryOp => {
            if node.children.len() == 2 {
                let op = node.value.as_deref().unwrap_or("");
                let left_key = expression_key(&node.children[0]);
                let right_key = expression_key(&node.children[1]);
                format!("({} {} {})", left_key, op, right_key)
            } else {
                format!("{:?}", node)
            }
        }
        IRNodeType::UnaryOp => {
            if !node.children.is_empty() {
                let op = node.value.as_deref().unwrap_or("");
                let operand_key = expression_key(&node.children[0]);
                format!("({} {})", op, operand_key)
            } else {
                format!("{:?}", node)
            }
        }
        IRNodeType::Constant => {
            // Constants are uniquely identified by their value
            format!("CONST({})", node.value.as_deref().unwrap_or(""))
        }
        IRNodeType::Load => {
            if !node.children.is_empty() {
                let var_key = expression_key(&node.children[0]);
                format!("LOAD({})", var_key)
            } else {
                format!("{:?}", node)
            }
        }
        IRNodeType::Variable => {
            // Variables are uniquely identified by their names
            format!("VAR({})", node.value.as_deref().unwrap_or(""))
        }
        _ => {
            // Other node types can't be reliably CSE'd
            format!("{:?}", node)
        }
    }
}

fn apply_cse(
    node: IRNode,
    expr_map: &mut HashMap<String, IRNode>,
    changed: &mut bool,
) -> Result<IRNode, String> {
    let mut result = node.clone();

    // First optimize children recursively
    for i in 0..result.children.len() {
        result.children[i] = apply_cse(result.children[i].clone(), expr_map, changed)?;
    }

    // Only apply CSE to expressions
    match result.node_type {
        IRNodeType::BinaryOp | IRNodeType::UnaryOp | IRNodeType::Load => {
            // Create a key for this expression
            let key = expression_key(&result);

            // Check if we've seen this expression before
            if let Some(prev_expr) = expr_map.get(&key) {
                // Found a duplicate expression, replace with previous computation
                println!("CSE: Found duplicate expression: {}", key);
                *changed = true;
                return Ok(prev_expr.clone());
            } else {
                // First time seeing this expression, remember it
                expr_map.insert(key, result.clone());
            }
        }
        _ => {}
    }

    Ok(result)
}

// Comprehensive Loop Optimization
fn optimize_loops(node: IRNode) -> Result<(IRNode, bool), String> {
    let mut changed = false;

    // Detect loop structures
    let loops = detect_loops(&node);

    if !loops.is_empty() {
        println!("Found {} loop structures", loops.len());
    }

    // Apply various loop optimizations
    let mut result = node.clone();

    // For each detected loop, apply optimizations
    for loop_info in &loops {
        // 1. Loop Invariant Code Motion
        let (new_ir, licm_changed) = hoist_loop_invariants(result, loop_info)?;
        result = new_ir;
        changed |= licm_changed;

        // 2. Loop Strength Reduction
        let (new_ir, strength_changed) = strength_reduction(result, loop_info)?;
        result = new_ir;
        changed |= strength_changed;

        // 3. Loop Unrolling for small loops
        let (new_ir, unroll_changed) = unroll_loops(result, loop_info)?;
        result = new_ir;
        changed |= unroll_changed;
    }

    Ok((result, changed))
}

// Loop structure information
struct LoopInfo {
    header_block: String,           // Loop header block name
    body_blocks: Vec<String>,       // Loop body block names
    induction_vars: Vec<String>,    // Loop induction variables
    exit_condition: Option<IRNode>, // Loop exit condition
    trip_count: Option<i32>,        // Known trip count if available
}

// Detect loop structures in the IR
fn detect_loops(node: &IRNode) -> Vec<LoopInfo> {
    let mut loops = Vec::new();

    // Look for basic blocks with "for" or "while" in their names
    // This is a simplified approach - in real compilers, loop detection
    // would use control flow graph analysis

    match node.node_type {
        IRNodeType::BasicBlock => {
            if let Some(block_name) = &node.value {
                if block_name.contains("for") || block_name.contains("while") {
                    // Found potential loop block
                    let mut loop_info = LoopInfo {
                        header_block: block_name.clone(),
                        body_blocks: Vec::new(),
                        induction_vars: Vec::new(),
                        exit_condition: None,
                        trip_count: None,
                    };

                    // Analyze block to find induction variables and body blocks
                    for child in &node.children {
                        if let IRNodeType::BasicBlock = child.node_type {
                            if let Some(child_name) = &child.value {
                                if child_name.contains("body") {
                                    loop_info.body_blocks.push(child_name.clone());

                                    // Look for induction variables (simplified)
                                    find_induction_vars(child, &mut loop_info.induction_vars);
                                }
                            }
                        } else if let IRNodeType::Branch = child.node_type {
                            // Extract exit condition
                            if !child.children.is_empty() {
                                loop_info.exit_condition = Some(child.children[0].clone());

                                // Try to determine trip count if possible
                                loop_info.trip_count = analyze_trip_count(
                                    &child.children[0],
                                    &loop_info.induction_vars,
                                );
                            }
                        }
                    }

                    // Only add to loops if we found body blocks
                    if !loop_info.body_blocks.is_empty() {
                        loops.push(loop_info);
                    }
                }
            }
        }
        _ => {}
    }

    // Recursively search for loops in children
    for child in &node.children {
        let child_loops = detect_loops(child);
        loops.extend(child_loops);
    }

    loops
}

// Find variables used as loop counters
fn find_induction_vars(node: &IRNode, vars: &mut Vec<String>) {
    match node.node_type {
        IRNodeType::Store => {
            // Look for variables that are incremented/decremented
            if node.children.len() > 1 {
                if let IRNodeType::BinaryOp = node.children[0].node_type {
                    let op = node.children[0].value.as_deref().unwrap_or("");
                    if op == "+" || op == "-" {
                        // Check if operating on the same variable being stored to
                        if let IRNodeType::Variable = node.children[1].node_type {
                            if let Some(var_name) = &node.children[1].value {
                                // Check if this variable is used in the operation
                                if has_variable(&node.children[0], var_name) {
                                    vars.push(var_name.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        _ => {}
    }

    // Check children recursively
    for child in &node.children {
        find_induction_vars(child, vars);
    }
}

// Check if a node uses a specific variable
fn has_variable(node: &IRNode, var_name: &str) -> bool {
    match node.node_type {
        IRNodeType::Variable => {
            if let Some(name) = &node.value {
                return name == var_name;
            }
            false
        }
        IRNodeType::Load => {
            if !node.children.is_empty() {
                if let IRNodeType::Variable = node.children[0].node_type {
                    if let Some(name) = &node.children[0].value {
                        return name == var_name;
                    }
                }
            }
            false
        }
        _ => {
            // Check children recursively
            for child in &node.children {
                if has_variable(child, var_name) {
                    return true;
                }
            }
            false
        }
    }
}

// Try to determine loop trip count from exit condition
fn analyze_trip_count(condition: &IRNode, induction_vars: &[String]) -> Option<i32> {
    // This is a simplified analysis
    // In a real compiler, we would use more sophisticated analysis

    if let IRNodeType::BinaryOp = condition.node_type {
        let op = condition.value.as_deref().unwrap_or("");
        if op == "<" || op == "<=" || op == ">" || op == ">=" || op == "!=" {
            if condition.children.len() == 2 {
                // Check if comparing an induction variable with a constant
                let (var_node, const_node) =
                    if is_induction_var(&condition.children[0], induction_vars) {
                        (&condition.children[0], &condition.children[1])
                    } else if is_induction_var(&condition.children[1], induction_vars) {
                        (&condition.children[1], &condition.children[0])
                    } else {
                        return None;
                    };

                // Check if comparing against a constant
                if let IRNodeType::Constant = const_node.node_type {
                    if let Some(val_str) = &const_node.value {
                        if let Ok(limit) = val_str.parse::<i32>() {
                            // Very simplified analysis - assumes loop starts at 0 and increments by 1
                            match op {
                                "<" => return Some(limit),
                                "<=" => return Some(limit + 1),
                                ">" => return Some(limit), // For reverse loops
                                ">=" => return Some(limit + 1), // For reverse loops
                                "!=" => return Some(limit), // Assumes loop counter compared with limit
                                _ => return None,
                            }
                        }
                    }
                }
            }
        }
    }

    None
}

// Check if a node represents an induction variable
fn is_induction_var(node: &IRNode, induction_vars: &[String]) -> bool {
    match node.node_type {
        IRNodeType::Variable => {
            if let Some(name) = &node.value {
                induction_vars.contains(name)
            } else {
                false
            }
        }
        IRNodeType::Load => {
            if !node.children.is_empty() {
                if let IRNodeType::Variable = node.children[0].node_type {
                    if let Some(name) = &node.children[0].value {
                        induction_vars.contains(name)
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            }
        }
        _ => false,
    }
}

// Hoist loop invariant computations out of the loop
fn hoist_loop_invariants(node: IRNode, loop_info: &LoopInfo) -> Result<(IRNode, bool), String> {
    let mut result = node.clone();
    let mut changed = false;

    // Process the loop block
    if let IRNodeType::BasicBlock = result.node_type {
        if let Some(block_name) = &result.value {
            if block_name == &loop_info.header_block {
                // Find invariant expressions in body blocks
                let mut invariant_exprs = Vec::new();

                for i in 0..result.children.len() {
                    if let IRNodeType::BasicBlock = result.children[i].node_type {
                        if let Some(child_name) = &result.children[i].value {
                            if loop_info.body_blocks.contains(child_name) {
                                // This is a loop body block - find invariant expressions
                                find_invariant_expressions(
                                    &result.children[i],
                                    &loop_info.induction_vars,
                                    &mut invariant_exprs,
                                );
                            }
                        }
                    }
                }

                // If we found invariant expressions, create a preheader block
                if !invariant_exprs.is_empty() {
                    println!("Found {} loop-invariant expressions", invariant_exprs.len());

                    // Create preheader block
                    let preheader = IRNode {
                        node_type: IRNodeType::BasicBlock,
                        value: Some(format!("{}.preheader", loop_info.header_block)),
                        children: invariant_exprs.clone(),
                        ty: None,
                    };

                    // Insert preheader before the loop
                    result.children.insert(0, preheader);
                    changed = true;

                    // Remove the invariant expressions from loop body
                    remove_hoisted_expressions(&mut result, &invariant_exprs);
                }
            }
        }
    }

    // Recursively process all children
    for i in 0..result.children.len() {
        let (new_child, child_changed) =
            hoist_loop_invariants(result.children[i].clone(), loop_info)?;

        result.children[i] = new_child;
        changed |= child_changed;
    }

    Ok((result, changed))
}

// Find expressions that are invariant inside a loop
fn find_invariant_expressions(
    node: &IRNode,
    induction_vars: &[String],
    invariant_exprs: &mut Vec<IRNode>,
) {
    match node.node_type {
        IRNodeType::BinaryOp | IRNodeType::UnaryOp => {
            // An expression is invariant if it doesn't depend on induction variables
            if !uses_induction_vars(node, induction_vars) {
                // Only consider 'interesting' expressions (not just constants/variables)
                let is_interesting = match node.node_type {
                    IRNodeType::BinaryOp => true,
                    IRNodeType::UnaryOp => true,
                    _ => false,
                };

                if is_interesting && !invariant_exprs.contains(node) {
                    invariant_exprs.push(node.clone());
                }
            }
        }
        _ => {}
    }

    // Check children recursively
    for child in &node.children {
        find_invariant_expressions(child, induction_vars, invariant_exprs);
    }
}

// Check if an expression uses any induction variables
fn uses_induction_vars(node: &IRNode, induction_vars: &[String]) -> bool {
    match node.node_type {
        IRNodeType::Variable => {
            if let Some(name) = &node.value {
                induction_vars.contains(name)
            } else {
                false
            }
        }
        IRNodeType::Load => {
            if !node.children.is_empty() {
                if let IRNodeType::Variable = node.children[0].node_type {
                    if let Some(name) = &node.children[0].value {
                        induction_vars.contains(name)
                    } else {
                        false
                    }
                } else {
                    false
                }
            } else {
                false
            }
        }
        _ => {
            // Check children recursively
            for child in &node.children {
                if uses_induction_vars(child, induction_vars) {
                    return true;
                }
            }
            false
        }
    }
}

// Remove hoisted expressions from the loop body
fn remove_hoisted_expressions(node: &mut IRNode, invariant_exprs: &[IRNode]) {
    if let IRNodeType::BasicBlock = node.node_type {
        // Remove any expressions that match our invariant list
        node.children
            .retain(|child| !invariant_exprs.contains(child));
    }

    // Recursively process all children
    for child in &mut node.children {
        remove_hoisted_expressions(child, invariant_exprs);
    }
}

// Loop strength reduction - replace multiplications with additions
fn strength_reduction(node: IRNode, loop_info: &LoopInfo) -> Result<(IRNode, bool), String> {
    let mut result = node.clone();
    let mut changed = false;

    // Look for array index calculations in loops
    // This is a simplified implementation - real strength reduction would be more complex

    if let IRNodeType::BasicBlock = result.node_type {
        if let Some(block_name) = &result.value {
            if loop_info.body_blocks.contains(block_name) {
                // We're in a loop body - look for strength reduction opportunities
                for i in 0..result.children.len() {
                    let (new_child, child_changed) =
                        maybe_reduce_strength(&result.children[i], &loop_info.induction_vars)?;

                    if child_changed {
                        result.children[i] = new_child;
                        changed = true;
                    }
                }
            }
        }
    }

    // Recursively process all children
    for i in 0..result.children.len() {
        let (new_child, child_changed) = strength_reduction(result.children[i].clone(), loop_info)?;

        result.children[i] = new_child;
        changed |= child_changed;
    }

    Ok((result, changed))
}

// Convert multiplications involving induction variables to additions
fn maybe_reduce_strength(
    node: &IRNode,
    induction_vars: &[String],
) -> Result<(IRNode, bool), String> {
    match node.node_type {
        IRNodeType::BinaryOp => {
            let op = node.value.as_deref().unwrap_or("");

            if op == "*" && node.children.len() == 2 {
                // Check if multiplying by a loop induction variable
                let (is_ind_var, ind_idx, const_idx) =
                    if is_induction_var(&node.children[0], induction_vars) {
                        (true, 0, 1)
                    } else if is_induction_var(&node.children[1], induction_vars) {
                        (true, 1, 0)
                    } else {
                        (false, 0, 0)
                    };

                if is_ind_var {
                    // Check if other operand is constant
                    if let IRNodeType::Constant = node.children[const_idx].node_type {
                        // We can replace i*4 with a running sum that's incremented by 4 each iteration
                        println!("Found strength reduction opportunity: multiplication by induction variable");

                        // In a real implementation, we would introduce a new variable and
                        // transform the IR to use additions instead, but this is simplified
                        return Ok((node.clone(), true));
                    }
                }
            }

            // Recursively check children
            let mut result = node.clone();
            let mut changed = false;

            for i in 0..result.children.len() {
                let (new_child, child_changed) =
                    maybe_reduce_strength(&result.children[i], induction_vars)?;

                if child_changed {
                    result.children[i] = new_child;
                    changed = true;
                }
            }

            Ok((result, changed))
        }
        _ => {
            // Recursively check children
            let mut result = node.clone();
            let mut changed = false;

            for i in 0..result.children.len() {
                let (new_child, child_changed) =
                    maybe_reduce_strength(&result.children[i], induction_vars)?;

                if child_changed {
                    result.children[i] = new_child;
                    changed = true;
                }
            }

            Ok((result, changed))
        }
    }
}

// Unroll small loops for better performance
fn unroll_loops(node: IRNode, loop_info: &LoopInfo) -> Result<(IRNode, bool), String> {
    // Only unroll loops with known small trip counts
    if let Some(trip_count) = loop_info.trip_count {
        if trip_count <= 4 {
            println!("Candidate for loop unrolling: trip count = {}", trip_count);

            // In a real implementation, we would unroll the loop by duplicating the body
            // This is a simplified placeholder
            return Ok((node, false));
        }
    }

    Ok((node, false))
}
