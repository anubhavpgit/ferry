use crate::ir::types::{IRNode, IRNodeType, IRType};
use std::collections::{HashMap, HashSet, VecDeque};

// Structure to track optimization statistics
struct OptimizationStats {
    constants_folded: usize,
    dead_code_removed: usize,
    expressions_eliminated: usize,
    loops_optimized: usize,
    functions_optimized: usize,
}

impl OptimizationStats {
    fn new() -> Self {
        OptimizationStats {
            constants_folded: 0,
            dead_code_removed: 0,
            expressions_eliminated: 0,
            loops_optimized: 0,
            functions_optimized: 0,
        }
    }

    fn print_summary(&self) {
        println!("\n----- Optimization Summary -----");
        println!("Constants folded: {}", self.constants_folded);
        println!("Dead code blocks removed: {}", self.dead_code_removed);
        println!(
            "Common expressions eliminated: {}",
            self.expressions_eliminated
        );
        println!("Loops optimized: {}", self.loops_optimized);
        println!("Functions optimized: {}", self.functions_optimized);
        println!("-------------------------------\n");
    }
}

pub fn optimise_ir(ir: IRNode) -> Result<IRNode, String> {
    println!("\n---------Starting Advanced Optimization---------");

    // Record optimization statistics
    let mut stats = OptimizationStats::new();

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
        let (new_ir, fold_changed, fold_count) =
            constant_folding_and_propagation_with_stats(optimized)?;
        optimized = new_ir;
        changed |= fold_changed;
        stats.constants_folded += fold_count;

        // 2. Loop Analysis and Optimization
        let (new_ir, loop_changed, loop_count) = advanced_loop_optimization_with_stats(optimized)?;
        optimized = new_ir;
        changed |= loop_changed;
        stats.loops_optimized += loop_count;

        // 3. Dead Code Elimination - Run after loop optimization to remove dead branches
        let (new_ir, dce_changed, dce_count) =
            enhanced_dead_code_elimination_with_stats(optimized)?;
        optimized = new_ir;
        changed |= dce_changed;
        stats.dead_code_removed += dce_count;

        // 4. Common Subexpression Elimination
        let (new_ir, cse_changed, cse_count) =
            common_subexpression_elimination_with_stats(optimized)?;
        optimized = new_ir;
        changed |= cse_changed;
        stats.expressions_eliminated += cse_count;

        // 5. Function-Level Optimizations
        let (new_ir, func_changed, func_count) = optimize_functions_with_stats(optimized)?;
        optimized = new_ir;
        changed |= func_changed;
        stats.functions_optimized += func_count;
    }

    println!("Optimization complete after {} iterations", iteration);

    // Print optimization statistics
    stats.print_summary();

    Ok(optimized)
}

// Constant folding and propagation combined for better results
fn constant_folding_and_propagation(node: IRNode) -> Result<(IRNode, bool), String> {
    // Call the version that tracks statistics and discard the count
    let (result, changed, _) = constant_folding_and_propagation_with_stats(node)?;
    Ok((result, changed))
}

// Version that tracks statistics
fn constant_folding_and_propagation_with_stats(
    node: IRNode,
) -> Result<(IRNode, bool, usize), String> {
    // Track constants and changed status
    let mut const_map: HashMap<String, Option<i32>> = HashMap::new();
    let mut changed = false;
    let mut fold_count = 0;

    // Don't propagate constants in loops unless we can prove they're invariant
    let loop_vars = identify_loop_variables(&node);

    // First pass: Collect constant assignments outside loops
    collect_constants(&node, &mut const_map, &loop_vars);

    // Second pass: Fold constants and propagate values
    let (result, local_fold_count) = fold_and_propagate_with_stats(node, &const_map, &mut changed)?;
    fold_count += local_fold_count;

    Ok((result, changed, fold_count))
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

// Fold constants and propagate values throughout the IR
fn fold_and_propagate(
    node: IRNode,
    const_map: &HashMap<String, Option<i32>>,
    changed: &mut bool,
) -> Result<IRNode, String> {
    // Call the version that tracks statistics and discard the count
    let (result, count) = fold_and_propagate_with_stats(node, const_map, changed)?;
    Ok(result)
}

// Version that tracks statistics
fn fold_and_propagate_with_stats(
    node: IRNode,
    const_map: &HashMap<String, Option<i32>>,
    changed: &mut bool,
) -> Result<(IRNode, usize), String> {
    let mut result = node.clone();
    let mut fold_count = 0;

    // Handle node types specific to constant folding
    match &result.node_type {
        IRNodeType::BinaryOp => {
            // First fold children recursively
            for i in 0..result.children.len() {
                let (new_child, child_count) =
                    fold_and_propagate_with_stats(result.children[i].clone(), const_map, changed)?;
                result.children[i] = new_child;
                fold_count += child_count;
            }

            // Check if operation can be folded
            if is_constant_expr(&result) {
                if let Some(val) = evaluate_constant_expr(&result) {
                    println!("Folded binary op to constant: {}", val);
                    *changed = true;
                    fold_count += 1;

                    // Create new constant node with appropriate type
                    let ty = result.ty.clone();
                    return Ok((
                        IRNode::new(IRNodeType::Constant, Some(val.to_string()))
                            .with_type(ty.unwrap_or(IRType::Int32)),
                        fold_count,
                    ));
                }
            }
        }
        IRNodeType::UnaryOp => {
            // First fold children recursively
            for i in 0..result.children.len() {
                let (new_child, child_count) =
                    fold_and_propagate_with_stats(result.children[i].clone(), const_map, changed)?;
                result.children[i] = new_child;
                fold_count += child_count;
            }

            // Check if operation can be folded
            if is_constant_expr(&result) {
                if let Some(val) = evaluate_constant_expr(&result) {
                    println!("Folded unary op to constant: {}", val);
                    *changed = true;
                    fold_count += 1;

                    // Create new constant node with appropriate type
                    let ty = result.ty.clone();
                    return Ok((
                        IRNode::new(IRNodeType::Constant, Some(val.to_string()))
                            .with_type(ty.unwrap_or(IRType::Int32)),
                        fold_count,
                    ));
                }
            }
        }
        IRNodeType::Load => {
            // First process child nodes
            for i in 0..result.children.len() {
                let (new_child, child_count) =
                    fold_and_propagate_with_stats(result.children[i].clone(), const_map, changed)?;
                result.children[i] = new_child;
                fold_count += child_count;
            }

            // Check if loading a known constant variable
            if let Some(var_node) = result.children.first() {
                if let IRNodeType::Variable = var_node.node_type {
                    if let Some(var_name) = &var_node.value {
                        if let Some(Some(const_val)) = const_map.get(var_name) {
                            println!("Replaced load of {} with constant {}", var_name, const_val);
                            *changed = true;
                            fold_count += 1;

                            // Create constant node with appropriate type
                            let ty = result.ty.clone();
                            return Ok((
                                IRNode::new(IRNodeType::Constant, Some(const_val.to_string()))
                                    .with_type(ty.unwrap_or(IRType::Int32)),
                                fold_count,
                            ));
                        }
                    }
                }
            }
        }
        _ => {
            // Recursively process all children
            for i in 0..result.children.len() {
                let (new_child, child_count) =
                    fold_and_propagate_with_stats(result.children[i].clone(), const_map, changed)?;
                result.children[i] = new_child;
                fold_count += child_count;
            }
        }
    }

    Ok((result, fold_count))
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

// Enhanced Dead Code Elimination with improved unreachable code detection
fn enhanced_dead_code_elimination(node: IRNode) -> Result<(IRNode, bool), String> {
    // Call the version that tracks statistics and discard the count
    let (result, changed, _) = enhanced_dead_code_elimination_with_stats(node)?;
    Ok((result, changed))
}

// Version that tracks statistics
fn enhanced_dead_code_elimination_with_stats(
    node: IRNode,
) -> Result<(IRNode, bool, usize), String> {
    // First pass: find all used variables and reachable blocks
    let mut used_vars = HashSet::new();
    let mut effect_instrs = HashSet::new(); // Instructions with side effects
    let mut reachable_blocks = HashMap::new(); // Track block reachability

    // Analyze control flow and identify reachable blocks
    analyze_control_flow(&node, &mut reachable_blocks);

    // Find used variables in reachable code
    find_used_variables(&node, &mut used_vars, &mut effect_instrs, &reachable_blocks);

    // Second pass: remove unused code and unreachable blocks
    let (result, changed, removed_count) =
        remove_dead_code_with_stats(node, &used_vars, &effect_instrs, &reachable_blocks)?;

    Ok((result, changed, removed_count))
}

// Analyze control flow to identify reachable blocks
fn analyze_control_flow(node: &IRNode, reachable_blocks: &mut HashMap<String, bool>) {
    // Start with main function's entry block as reachable
    if let IRNodeType::Function = node.node_type {
        if let Some(func_name) = &node.value {
            if func_name == "main" {
                // Mark all blocks in main as initially reachable
                mark_reachable_blocks(node, true, reachable_blocks);
                return;
            }
        }
    }

    // Mark blocks based on branch conditions
    match node.node_type {
        IRNodeType::Branch => {
            if !node.children.is_empty() {
                // Check if branch condition is constant
                if let Some(cond) = node.children.first() {
                    if let IRNodeType::Constant = cond.node_type {
                        if let Some(val) = &cond.value {
                            if let Ok(val_int) = val.parse::<i32>() {
                                // Mark appropriate branch target as reachable
                                let target_idx = if val_int != 0 { 1 } else { 2 };
                                if node.children.len() > target_idx {
                                    mark_reachable_blocks(
                                        &node.children[target_idx],
                                        true,
                                        reachable_blocks,
                                    );

                                    // Mark other branch as unreachable if it exists
                                    let other_idx = if target_idx == 1 { 2 } else { 1 };
                                    if node.children.len() > other_idx {
                                        mark_reachable_blocks(
                                            &node.children[other_idx],
                                            false,
                                            reachable_blocks,
                                        );
                                    }

                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }
        _ => {}
    }

    // Recursively analyze children
    for child in &node.children {
        analyze_control_flow(child, reachable_blocks);
    }
}

// Mark blocks as reachable or unreachable
fn mark_reachable_blocks(
    node: &IRNode,
    reachable: bool,
    reachable_blocks: &mut HashMap<String, bool>,
) {
    if let IRNodeType::BasicBlock = node.node_type {
        if let Some(block_name) = &node.value {
            // Only update if setting to reachable or if not previously marked reachable
            if reachable || !reachable_blocks.contains_key(block_name) {
                reachable_blocks.insert(block_name.clone(), reachable);
            }
        }
    }

    // Recursively mark children
    for child in &node.children {
        mark_reachable_blocks(child, reachable, reachable_blocks);
    }
}

// Find used variables in reachable code
fn find_used_variables(
    node: &IRNode,
    used_vars: &mut HashSet<String>,
    effect_instrs: &mut HashSet<String>,
    reachable_blocks: &HashMap<String, bool>,
) {
    // Check if we're in an unreachable block
    if let IRNodeType::BasicBlock = node.node_type {
        if let Some(block_name) = &node.value {
            if let Some(&reachable) = reachable_blocks.get(block_name) {
                if !reachable {
                    // Skip unreachable blocks
                    return;
                }
            }
        }
    }

    // Instructions with side effects must be kept regardless of used variables
    match node.node_type {
        IRNodeType::Call => {
            // ALL function calls have side effects and must be preserved
            let fn_id = format!("{:?}", node);
            effect_instrs.insert(fn_id);

            // Mark the entire containing block as having side effects
            if let Some(parent_id) = get_parent_block_id(node) {
                effect_instrs.insert(parent_id);
            }

            // Arguments to function calls are used
            for child in &node.children {
                find_live_variables(child, used_vars);
            }
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
        IRNodeType::Jump => {
            // Jumps (including breaks) affect control flow
            let jump_id = format!("{:?}", node);
            effect_instrs.insert(jump_id);
        }
        _ => {}
    }

    // Recursively process children
    for child in &node.children {
        find_used_variables(child, used_vars, effect_instrs, reachable_blocks);
    }
}

// Helper function to get parent block ID (for context)
fn get_parent_block_id(node: &IRNode) -> Option<String> {
    // This is a simplified version - in a real compiler we'd track parent nodes
    // Here we just create a unique identifier for the containing block
    Some(format!("PARENT_OF_{:?}", node))
}
// Find live variables starting from usage points
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

// Remove dead code and unreachable blocks
fn remove_dead_code(
    node: IRNode,
    used_vars: &HashSet<String>,
    effect_instrs: &HashSet<String>,
    reachable_blocks: &HashMap<String, bool>,
) -> Result<(IRNode, bool), String> {
    // Call the version that tracks statistics and discard the count
    let (result, changed, _) =
        remove_dead_code_with_stats(node, used_vars, effect_instrs, reachable_blocks)?;
    Ok((result, changed))
}

// Version that tracks statistics
fn remove_dead_code_with_stats(
    node: IRNode,
    used_vars: &HashSet<String>,
    effect_instrs: &HashSet<String>,
    reachable_blocks: &HashMap<String, bool>,
) -> Result<(IRNode, bool, usize), String> {
    let mut result = node.clone();
    let mut changed = false;
    let mut removed_count = 0;

    // PRESERVE THE MAIN FUNCTION
    if let IRNodeType::Function = result.node_type {
        if let Some(func_name) = &result.value {
            if func_name == "main" {
                // Still process main function for dead code elimination
                // but preserve its core structure
                let mut preserved_children = Vec::new();

                for child in result.children {
                    let (new_child, child_changed, child_removed) = remove_dead_code_with_stats(
                        child,
                        used_vars,
                        effect_instrs,
                        reachable_blocks,
                    )?;

                    // Keep the child even if modified
                    preserved_children.push(new_child);
                    changed |= child_changed;
                    removed_count += child_removed;
                }

                result.children = preserved_children;
                return Ok((result, changed, removed_count));
            }
        }
    }

    // Check if this is an unreachable block
    if let IRNodeType::BasicBlock = result.node_type {
        if let Some(block_name) = &result.value {
            if let Some(&reachable) = reachable_blocks.get(block_name) {
                if !reachable {
                    // This block is unreachable - remove all its contents
                    println!("Removing unreachable block: {}", block_name);
                    let original_count = result.children.len();
                    result.children.clear();
                    changed = true;
                    removed_count += original_count;
                    return Ok((result, changed, removed_count));
                }
            }
        }
    }

    // For basic blocks, remove unused instructions
    if let IRNodeType::BasicBlock = result.node_type {
        // Keep track of the original number of children
        let original_len = result.children.len();

        // Filter out dead instructions
        let mut new_children = Vec::new();

        for child in result.children {
            if should_keep_instruction(&child, used_vars, effect_instrs) {
                // Process the children of instructions we're keeping
                let (new_child, child_changed, child_removed) =
                    remove_dead_code_with_stats(child, used_vars, effect_instrs, reachable_blocks)?;

                new_children.push(new_child);
                changed |= child_changed;
                removed_count += child_removed;
            } else {
                // Not keeping this instruction
                changed = true;
                removed_count += 1;
            }
        }

        result.children = new_children;
        return Ok((result, changed, removed_count));
    }

    // Special handling for branches with constant conditions
    if let IRNodeType::Branch = result.node_type {
        if !result.children.is_empty() {
            if let IRNodeType::Constant = result.children[0].node_type {
                if let Some(val) = &result.children[0].value {
                    if let Ok(val_int) = val.parse::<i32>() {
                        // Determine which branch to keep
                        let keep_idx = if val_int != 0 { 1 } else { 2 };

                        if result.children.len() > keep_idx {
                            // Replace branch with the target block
                            let (target_block, target_changed, target_removed) =
                                remove_dead_code_with_stats(
                                    result.children[keep_idx].clone(),
                                    used_vars,
                                    effect_instrs,
                                    reachable_blocks,
                                )?;

                            println!("Simplified constant branch to direct block");
                            changed = true;
                            removed_count += result.children.len() - 1; // Count the removed branch and condition
                            return Ok((target_block, true, removed_count + target_removed));
                        }
                    }
                }
            }
        }
    }

    // Process remaining children recursively
    let mut new_children = Vec::new();

    for child in result.children {
        let (new_child, child_changed, child_removed) =
            remove_dead_code_with_stats(child, used_vars, effect_instrs, reachable_blocks)?;

        new_children.push(new_child);
        changed |= child_changed;
        removed_count += child_removed;
    }

    result.children = new_children;
    Ok((result, changed, removed_count))
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
        IRNodeType::Return | IRNodeType::Branch | IRNodeType::Jump => true,

        // Always keep calls as they may have side effects
        IRNodeType::Call => true,

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

        // For all other instructions, keep if they have side effects or are part of a basic block
        _ => effect_instrs.contains(&instr_id),
    }
}

// Enhanced loop optimization with better recognition of patterns
fn advanced_loop_optimization(node: IRNode) -> Result<(IRNode, bool), String> {
    // Call the version that tracks statistics and discard the count
    let (result, changed, _) = advanced_loop_optimization_with_stats(node)?;
    Ok((result, changed))
}

// Version that tracks statistics
fn advanced_loop_optimization_with_stats(node: IRNode) -> Result<(IRNode, bool, usize), String> {
    // Analyze the full IR for loop patterns
    let mut changed = false;
    let mut result = node.clone();
    let mut optimized_count = 0;

    // Find and analyze all loops in the IR
    let loops = detect_loops_with_analysis(&result);

    if !loops.is_empty() {
        println!(
            "Found {} loop structures with advanced analysis",
            loops.len()
        );

        // For each loop, apply various optimizations based on the analysis
        for loop_info in &loops {
            println!("Analyzing loop: {}", loop_info.header_block);
            let mut loop_optimized = false;

            // Try the most aggressive optimizations first
            // Check if loop contains function calls with side effects
            let mut has_side_effects = false;
            let mut call_nodes = Vec::new();
            collect_function_calls_from_loop(&result, &loop_info.body_blocks, &mut call_nodes);

            if !call_nodes.is_empty() {
                has_side_effects = true;
                println!("Loop contains function calls with side effects - will preserve");
            }

            // 1. Loop Elimination (if result is deterministic)
            if !has_side_effects && loop_info.deterministic_result.is_some() {
                // This loop always produces the same result, we can eliminate it
                println!(
                    "Loop can be eliminated. Deterministic result: {:?}",
                    loop_info.deterministic_result
                );
                let (new_ir, elim_changed) = eliminate_deterministic_loop(result, loop_info)?;
                result = new_ir;
                changed |= elim_changed;

                if elim_changed {
                    // Count as optimized
                    loop_optimized = true;
                    // Skip other optimizations if the loop was eliminated
                    continue;
                }
            }

            // 2. Loop Bound Reduction (if early exit detected)
            if let Some(actual_trip_count) = loop_info.actual_trip_count {
                if let Some(original_trip_count) = loop_info.trip_count {
                    if actual_trip_count < original_trip_count {
                        println!(
                            "Loop bound can be reduced from {} to {}",
                            original_trip_count, actual_trip_count
                        );
                        let (new_ir, bound_changed) = reduce_loop_bounds(result, loop_info)?;
                        result = new_ir;
                        changed |= bound_changed;
                        loop_optimized |= bound_changed;
                    }
                }
            }

            // 3. Apply other loop optimizations (if the loop wasn't eliminated)

            // Loop Invariant Code Motion
            let (new_ir, licm_changed) = hoist_loop_invariants(result.clone(), loop_info)?;
            result = new_ir;
            changed |= licm_changed;
            loop_optimized |= licm_changed;

            // Loop Strength Reduction
            let (new_ir, strength_changed) = strength_reduction(result.clone(), loop_info)?;
            result = new_ir;
            changed |= strength_changed;
            loop_optimized |= strength_changed;

            // Loop Unrolling for small loops
            if let Some(trip_count) = loop_info.actual_trip_count.or(loop_info.trip_count) {
                if trip_count <= 4 {
                    let (new_ir, unroll_changed) =
                        unroll_loop(result.clone(), loop_info, trip_count)?;
                    result = new_ir;
                    changed |= unroll_changed;
                    loop_optimized |= unroll_changed;
                }
            }

            // Count this loop if any optimization was applied
            if loop_optimized {
                optimized_count += 1;
            }
        }
    }

    // Recursively process all children that weren't part of loops
    let mut new_children = Vec::new();
    let mut in_loop = false;

    for child in &node.children {
        // Skip processing loop children separately since they were handled above
        if let IRNodeType::BasicBlock = child.node_type {
            if let Some(block_name) = &child.value {
                if loops
                    .iter()
                    .any(|l| l.header_block == *block_name || l.body_blocks.contains(block_name))
                {
                    in_loop = true;
                    continue;
                }
            }
        }

        if !in_loop {
            let (new_child, child_changed, child_count) =
                advanced_loop_optimization_with_stats(child.clone())?;
            new_children.push(new_child);
            changed |= child_changed;
            optimized_count += child_count;
        }
    }

    // Only replace children if we're not in a loop context
    if !in_loop && !new_children.is_empty() {
        result.children = new_children;
    }

    Ok((result, changed, optimized_count))
}

// Enhanced LoopInfo structure with additional analysis data
struct LoopInfo {
    header_block: String,                         // Loop header block name
    body_blocks: Vec<String>,                     // Loop body block names
    induction_vars: Vec<String>,                  // Loop induction variables
    exit_condition: Option<IRNode>,               // Loop exit condition
    trip_count: Option<i32>,                      // Estimated trip count based on condition
    actual_trip_count: Option<i32>,               // Actual trip count based on early exits
    deterministic_result: Option<IRValue>, // If the loop always produces a deterministic result
    exit_variables: HashMap<String, Option<i32>>, // Variable values at loop exit
}

// Intermediate representation of a concrete value
#[derive(Debug, Clone)]
enum IRValue {
    Int(i32),
    Float(f32),
    Bool(bool),
    Unknown,
}

// Detect loops with more comprehensive analysis
fn detect_loops_with_analysis(node: &IRNode) -> Vec<LoopInfo> {
    let mut loops = Vec::new();

    // First collect basic loop structures
    let basic_loops = detect_loops(node);

    // For each basic loop, perform advanced analysis
    for basic_loop in basic_loops {
        let mut enhanced_loop = LoopInfo {
            header_block: basic_loop.header_block,
            body_blocks: basic_loop.body_blocks,
            induction_vars: basic_loop.induction_vars,
            exit_condition: basic_loop.exit_condition.clone(),
            trip_count: basic_loop.trip_count,
            actual_trip_count: None,
            deterministic_result: None,
            exit_variables: HashMap::new(),
        };

        // Analyze early exits
        analyze_early_exits(node, &mut enhanced_loop);

        // Analyze deterministic behavior
        analyze_deterministic_result(node, &mut enhanced_loop);

        // Add the enhanced loop info
        loops.push(enhanced_loop);
    }

    loops
}

// Analyze loop for early exit points
fn analyze_early_exits(node: &IRNode, loop_info: &mut LoopInfo) {
    // Check if this block is inside the loop
    let in_loop_body = if let IRNodeType::BasicBlock = node.node_type {
        if let Some(block_name) = &node.value {
            loop_info.body_blocks.contains(block_name)
        } else {
            false
        }
    } else {
        false
    };

    if in_loop_body {
        // Look for early exits or conditions that always become true
        for child in &node.children {
            if let IRNodeType::Branch = child.node_type {
                if !child.children.is_empty() {
                    // Check if this is an early exit condition
                    if let Some(exit_value) =
                        analyze_exit_condition(&child.children[0], &loop_info.induction_vars)
                    {
                        loop_info.actual_trip_count = Some(exit_value);
                        println!("Detected early exit after {} iterations", exit_value);
                    }
                }
            }

            // Check for return statements inside the loop
            if let IRNodeType::Return = child.node_type {
                // Return inside loop means we exit early
                if !child.children.is_empty() {
                    // Capture the return value
                    if let IRNodeType::Constant = child.children[0].node_type {
                        if let Some(val) = &child.children[0].value {
                            if let Ok(ret_val) = val.parse::<i32>() {
                                println!("Detected return with value {} inside loop", ret_val);
                                loop_info.deterministic_result = Some(IRValue::Int(ret_val));
                            }
                        }
                    }
                } else {
                    // Return with no value
                    println!("Detected void return inside loop");
                    loop_info.deterministic_result = Some(IRValue::Unknown);
                }
            }
        }
    }

    // Recursively check children
    for child in &node.children {
        analyze_early_exits(child, loop_info);
    }
}

// Analyze the exit condition to see if we can determine when it becomes true
fn analyze_exit_condition(condition: &IRNode, induction_vars: &[String]) -> Option<i32> {
    // We're looking for conditions like "i == 5" where i is an induction variable
    if let IRNodeType::BinaryOp = condition.node_type {
        let op = condition.value.as_deref().unwrap_or("");

        if op == "==" && condition.children.len() == 2 {
            // Look for pattern: induction_var == constant
            let (ind_var, const_node) = if is_induction_var(&condition.children[0], induction_vars)
            {
                (&condition.children[0], &condition.children[1])
            } else if is_induction_var(&condition.children[1], induction_vars) {
                (&condition.children[1], &condition.children[0])
            } else {
                return None;
            };

            // Extract the constant value
            if let IRNodeType::Constant = const_node.node_type {
                if let Some(val_str) = &const_node.value {
                    if let Ok(exit_val) = val_str.parse::<i32>() {
                        // Assuming the induction variable starts at 0 and increments by 1
                        // So an equality check for 5 means we exit after 5 iterations
                        return Some(exit_val);
                    }
                }
            }
        }
    }

    None
}

// Analyze if the loop always produces a deterministic result
fn analyze_deterministic_result(node: &IRNode, loop_info: &mut LoopInfo) {
    // Look for deterministic patterns in the loop
    let mut body_contains_returns = false;
    let mut all_exit_points_return_same = true;
    let mut return_value: Option<i32> = None;

    // Examine the loop body for return statements
    for block_name in &loop_info.body_blocks {
        let block = find_block_by_name(node, block_name);

        if let Some(block_node) = block {
            for child in &block_node.children {
                if let IRNodeType::Return = child.node_type {
                    body_contains_returns = true;

                    // Check return value
                    if !child.children.is_empty() {
                        if let IRNodeType::Constant = child.children[0].node_type {
                            if let Some(val) = &child.children[0].value {
                                if let Ok(ret_val) = val.parse::<i32>() {
                                    if let Some(prev_val) = return_value {
                                        if prev_val != ret_val {
                                            all_exit_points_return_same = false;
                                        }
                                    } else {
                                        return_value = Some(ret_val);
                                    }
                                }
                            }
                        } else {
                            // Non-constant return value
                            all_exit_points_return_same = false;
                        }
                    }
                }
            }
        }
    }

    // If we have early exits that all return the same value, that's deterministic
    if body_contains_returns && all_exit_points_return_same && return_value.is_some() {
        loop_info.deterministic_result = Some(IRValue::Int(return_value.unwrap()));
    }

    // Also check for loops that always exit after a fixed number of iterations
    // and don't modify any variables used outside the loop
    if loop_info.actual_trip_count.is_some() {
        // If all exit paths return the same value or have no effect outside the loop
        // we can consider the loop deterministic
        let exit_vars = analyze_variables_after_loop(node, loop_info);

        if !exit_vars.is_empty() {
            // Store exit_vars in loop_info
            loop_info.exit_variables = exit_vars.clone();

            // If we didn't already determine a result and all paths lead to one value
            if loop_info.deterministic_result.is_none() {
                if let Some(&Some(val)) = exit_vars.values().next() {
                    // Check if all exit variables have the same value
                    let all_same = exit_vars.values().all(|v| *v == Some(val));

                    if all_same {
                        loop_info.deterministic_result = Some(IRValue::Int(val));
                    }
                }
            }
        }
    }
}

// Find a block by name
fn find_block_by_name<'a>(node: &'a IRNode, name: &str) -> Option<&'a IRNode> {
    if let IRNodeType::BasicBlock = node.node_type {
        if let Some(block_name) = &node.value {
            if block_name == name {
                return Some(node);
            }
        }
    }

    // Recursively check children
    for child in &node.children {
        if let Some(found) = find_block_by_name(child, name) {
            return Some(found);
        }
    }

    None
}

// Analyze variable values after the loop exits
fn analyze_variables_after_loop(
    node: &IRNode,
    loop_info: &LoopInfo,
) -> HashMap<String, Option<i32>> {
    let mut exit_vars = HashMap::new();

    // Only meaningful for loops with known trip counts
    if let Some(trip_count) = loop_info.actual_trip_count.or(loop_info.trip_count) {
        // This is a simplified simulation of the loop execution
        // In a real compiler this would be more sophisticated

        // Try to simulate the loop to determine final variable values
        let induction_vars = &loop_info.induction_vars;

        for var in induction_vars {
            // Assume induction variables start at 0 and increment by 1
            // After loop exits, they'll have the exit value
            exit_vars.insert(var.clone(), Some(trip_count));
        }

        // Look at other variables modified in the loop
        // (This is simplified - real analysis would be more complex)
        for block_name in &loop_info.body_blocks {
            let block = find_block_by_name(node, block_name);

            if let Some(block_node) = block {
                for child in &block_node.children {
                    if let IRNodeType::Store = child.node_type {
                        if child.children.len() > 1 {
                            if let IRNodeType::Variable = child.children[1].node_type {
                                if let Some(var_name) = &child.children[1].value {
                                    // Skip induction variables, they're already handled
                                    if !induction_vars.contains(var_name) {
                                        // Check if storing a constant
                                        if let IRNodeType::Constant = child.children[0].node_type {
                                            if let Some(val) = &child.children[0].value {
                                                if let Ok(val_int) = val.parse::<i32>() {
                                                    exit_vars
                                                        .insert(var_name.clone(), Some(val_int));
                                                }
                                            }
                                        } else {
                                            // Non-constant store
                                            exit_vars.insert(var_name.clone(), None);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    exit_vars
}

// Eliminate a loop that always produces the same result
fn eliminate_deterministic_loop(
    node: IRNode,
    loop_info: &LoopInfo,
) -> Result<(IRNode, bool), String> {
    let mut result = node.clone();
    let mut changed = false;

    if loop_info.deterministic_result.is_none() {
        return Ok((result, changed));
    }

    match result.node_type {
        IRNodeType::BasicBlock => {
            if let Some(block_name) = &result.value {
                if block_name == &loop_info.header_block {
                    // Replace the loop with its result
                    println!("Eliminating deterministic loop: {}", block_name);

                    // Create replacement for the loop
                    let mut replacement = IRNode::new(
                        IRNodeType::BasicBlock,
                        Some(format!("{}_eliminated", block_name)),
                    );

                    // Add any allocation nodes from the original loop for variables used outside
                    for child in &result.children {
                        if let IRNodeType::Alloca = child.node_type {
                            replacement.add_child(child.clone());
                        }
                    }

                    // Collect all function calls from the loop body to preserve their side effects
                    let mut call_nodes = Vec::new();
                    collect_function_calls_from_loop(
                        &node,
                        &loop_info.body_blocks,
                        &mut call_nodes,
                    );

                    // Add all function calls to the replacement block in original order
                    for call in &call_nodes {
                        replacement.add_child(call.clone());
                    }

                    // Add stores for any variables modified in the loop
                    for (var_name, value) in &loop_info.exit_variables {
                        if let Some(val) = value {
                            // Create a store for this variable
                            let mut store = IRNode::new(IRNodeType::Store, None);

                            // Value to store
                            let constant = IRNode::new(IRNodeType::Constant, Some(val.to_string()))
                                .with_type(IRType::Int32);
                            store.add_child(constant);

                            // Variable to store to
                            let var = IRNode::new(IRNodeType::Variable, Some(var_name.clone()));
                            store.add_child(var);

                            replacement.add_child(store);
                        }
                    }

                    // If the loop had a deterministic return value, add it
                    if let Some(IRValue::Int(ret_val)) = &loop_info.deterministic_result {
                        let mut ret = IRNode::new(IRNodeType::Return, None);
                        let val = IRNode::new(IRNodeType::Constant, Some(ret_val.to_string()))
                            .with_type(IRType::Int32);
                        ret.add_child(val);
                        replacement.add_child(ret);
                    }

                    return Ok((replacement, true));
                }

                // Skip loop body blocks entirely
                if loop_info.body_blocks.contains(block_name) {
                    // This is part of the eliminated loop
                    return Ok((
                        IRNode::new(
                            IRNodeType::BasicBlock,
                            Some(format!("{}_eliminated", block_name)),
                        ),
                        true,
                    ));
                }
            }
        }
        _ => {}
    }

    // Process children except those in the eliminated loop
    let mut new_children = Vec::new();

    // Use vec_children to store a copy of the children
    let vec_children = result.children.clone();

    for child in vec_children {
        let skip_child = if let IRNodeType::BasicBlock = child.node_type {
            if let Some(child_name) = &child.value {
                child_name == &loop_info.header_block || loop_info.body_blocks.contains(child_name)
            } else {
                false
            }
        } else {
            false
        };

        if !skip_child {
            let (new_child, child_changed) = eliminate_deterministic_loop(child, loop_info)?;
            new_children.push(new_child);
            changed |= child_changed;
        } else {
            changed = true;
        }
    }

    if !new_children.is_empty() {
        result.children = new_children;
    }

    Ok((result, changed))
}

// Helper function to collect all function calls from a loop body
fn collect_function_calls_from_loop(
    node: &IRNode,
    body_blocks: &[String],
    call_nodes: &mut Vec<IRNode>,
) {
    // Check if this is a loop body block
    let in_loop_body = if let IRNodeType::BasicBlock = node.node_type {
        if let Some(block_name) = &node.value {
            body_blocks.contains(block_name)
        } else {
            false
        }
    } else {
        false
    };

    // If we're in a loop body, collect function calls
    if in_loop_body {
        for child in &node.children {
            if let IRNodeType::Call = child.node_type {
                call_nodes.push(child.clone());
            } else {
                // Recursively search for calls in nested blocks
                collect_function_calls_from_loop(child, body_blocks, call_nodes);
            }
        }
    }

    // If not in a loop body, check all children recursively
    if !in_loop_body {
        for child in &node.children {
            collect_function_calls_from_loop(child, body_blocks, call_nodes);
        }
    }
}
// Reduce loop bounds when we know it exits early
fn reduce_loop_bounds(node: IRNode, loop_info: &LoopInfo) -> Result<(IRNode, bool), String> {
    let mut result = node.clone();
    let mut changed = false;

    // We need both an actual and original trip count
    if loop_info.actual_trip_count.is_none() || loop_info.trip_count.is_none() {
        return Ok((result, changed));
    }

    let actual_count = loop_info.actual_trip_count.unwrap();
    let original_count = loop_info.trip_count.unwrap();

    if actual_count >= original_count {
        // No optimization possible
        return Ok((result, changed));
    }

    // Modify the loop condition
    match result.node_type {
        IRNodeType::BasicBlock => {
            if let Some(block_name) = &result.value {
                if block_name == &loop_info.header_block {
                    // Find and update the branch condition
                    for i in 0..result.children.len() {
                        if let IRNodeType::Branch = result.children[i].node_type {
                            if !result.children[i].children.is_empty() {
                                if let IRNodeType::BinaryOp =
                                    result.children[i].children[0].node_type
                                {
                                    // Found a comparison operation
                                    let op = result.children[i].children[0]
                                        .value
                                        .as_deref()
                                        .unwrap_or("");

                                    // Look for typical loop conditions
                                    if (op == "<" || op == "<=")
                                        && result.children[i].children[0].children.len() == 2
                                    {
                                        // Look for induction var < constant
                                        let (has_ind_var, ind_idx, const_idx) = if is_induction_var(
                                            &result.children[i].children[0].children[0],
                                            &loop_info.induction_vars,
                                        ) {
                                            (true, 0, 1)
                                        } else if is_induction_var(
                                            &result.children[i].children[0].children[1],
                                            &loop_info.induction_vars,
                                        ) {
                                            (true, 1, 0)
                                        } else {
                                            (false, 0, 0)
                                        };

                                        if has_ind_var {
                                            // Check if comparing against a constant
                                            if let IRNodeType::Constant =
                                                result.children[i].children[0].children[const_idx]
                                                    .node_type
                                            {
                                                // Update the constant to the actual trip count (adjusted based on operator)
                                                let new_limit = if op == "<" {
                                                    actual_count
                                                } else {
                                                    // "<="
                                                    actual_count - 1
                                                };

                                                let new_const = IRNode::new(
                                                    IRNodeType::Constant,
                                                    Some(new_limit.to_string()),
                                                )
                                                .with_type(IRType::Int32);

                                                // Replace the constant
                                                result.children[i].children[0].children
                                                    [const_idx] = new_const;
                                                println!(
                                                    "Reduced loop bound from {} to {}",
                                                    original_count, new_limit
                                                );
                                                changed = true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        _ => {}
    }

    // Recursively process children
    for i in 0..result.children.len() {
        let (new_child, child_changed) = reduce_loop_bounds(result.children[i].clone(), loop_info)?;
        result.children[i] = new_child;
        changed |= child_changed;
    }

    Ok((result, changed))
}

// Basic loop detection (from previous implementation)
fn detect_loops(node: &IRNode) -> Vec<LoopInfo> {
    let mut loops = Vec::new();

    // Look for basic blocks with "for" or "while" in their names
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
                        actual_trip_count: None,
                        deterministic_result: None,
                        exit_variables: HashMap::new(),
                    };

                    // Analyze block to find induction variables and body blocks
                    for child in &node.children {
                        if let IRNodeType::BasicBlock = child.node_type {
                            if let Some(child_name) = &child.value {
                                if child_name.contains("body") {
                                    loop_info.body_blocks.push(child_name.clone());

                                    // Look for induction variables
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

// Check if a node is an induction variable
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
                            // Estimate trip count based on the comparison operator
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
                    result = remove_hoisted_expressions(result, &invariant_exprs);
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
fn remove_hoisted_expressions(mut node: IRNode, invariant_exprs: &[IRNode]) -> IRNode {
    if let IRNodeType::BasicBlock = node.node_type {
        // Remove any expressions that match our invariant list
        node.children
            .retain(|child| !invariant_exprs.contains(child));
    }

    // Recursively process all children
    let mut new_children = Vec::new();
    for i in 0..node.children.len() {
        // Clone each child before processing to avoid ownership issues
        let child = node.children[i].clone();
        new_children.push(remove_hoisted_expressions(child, invariant_exprs));
    }
    node.children = new_children;

    node
}

// Loop strength reduction - replace multiplications with additions
fn strength_reduction(node: IRNode, loop_info: &LoopInfo) -> Result<(IRNode, bool), String> {
    let mut result = node.clone();
    let mut changed = false;

    // Look for array index calculations in loops
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
fn unroll_loop(
    node: IRNode,
    loop_info: &LoopInfo,
    trip_count: i32,
) -> Result<(IRNode, bool), String> {
    // Only unroll loops with known small trip counts
    if trip_count <= 4 {
        println!("Unrolling loop with trip count = {}", trip_count);

        // In a real implementation, we would duplicate the loop body trip_count times
        // and eliminate the loop control overhead

        // This is a simplified implementation
        let mut result = node.clone();
        let mut changed = false;

        // Check if we're processing the loop header
        if let IRNodeType::BasicBlock = result.node_type {
            if let Some(block_name) = &result.value {
                if block_name == &loop_info.header_block {
                    // Create a new block to replace the loop
                    let mut unrolled_block = IRNode::new(
                        IRNodeType::BasicBlock,
                        Some(format!("{}_unrolled", block_name)),
                    );

                    // Find the loop body blocks
                    let mut body_blocks = Vec::new();

                    for child in &result.children {
                        if let IRNodeType::BasicBlock = child.node_type {
                            if let Some(child_name) = &child.value {
                                if loop_info.body_blocks.contains(child_name) {
                                    body_blocks.push(child.clone());
                                }
                            }
                        }
                    }

                    // Duplicate the body blocks trip_count times
                    for i in 0..trip_count {
                        for body in &body_blocks {
                            let mut block_copy = body.clone();
                            // Update any references to induction variables
                            update_induction_vars(&mut block_copy, &loop_info.induction_vars, i);

                            // Add to unrolled block
                            for child in &block_copy.children {
                                // Skip branch back instructions in all but the last iteration
                                if i == trip_count - 1
                                    || !is_loop_back_branch(child, &loop_info.header_block)
                                {
                                    unrolled_block.add_child(child.clone());
                                }
                            }
                        }
                    }

                    return Ok((unrolled_block, true));
                }
            }
        }

        // Process children
        for i in 0..result.children.len() {
            let (new_child, child_changed) =
                unroll_loop(result.children[i].clone(), loop_info, trip_count)?;
            result.children[i] = new_child;
            changed |= child_changed;
        }

        return Ok((result, changed));
    }

    // If we can't unroll, return unchanged
    Ok((node, false))
}

// Update induction variable references to constants for loop unrolling
fn update_induction_vars(node: &mut IRNode, induction_vars: &[String], iteration: i32) {
    match node.node_type {
        IRNodeType::Load => {
            // Check if loading an induction variable
            if !node.children.is_empty() {
                if let IRNodeType::Variable = node.children[0].node_type {
                    if let Some(var_name) = &node.children[0].value {
                        if induction_vars.contains(var_name) {
                            // Create a new node without moving children
                            let new_node =
                                IRNode::new(IRNodeType::Constant, Some(iteration.to_string()))
                                    .with_type(IRType::Int32);
                            // Replace the entire node
                            *node = new_node;
                            return;
                        }
                    }
                }
            }
        }
        IRNodeType::Variable => {
            // Check if this is an induction variable
            if let Some(var_name) = &node.value {
                if induction_vars.contains(var_name) {
                    // Create a new node without moving children
                    let new_node = IRNode::new(IRNodeType::Constant, Some(iteration.to_string()))
                        .with_type(IRType::Int32);
                    // Replace the entire node
                    *node = new_node;
                    return;
                }
            }
        }
        _ => {}
    }

    // Recursively update children without moving the entire vector
    let mut new_children = Vec::with_capacity(node.children.len());

    for i in 0..node.children.len() {
        // Clone the child so we can modify it independently
        let mut child = node.children[i].clone();
        update_induction_vars(&mut child, induction_vars, iteration);
        new_children.push(child);
    }

    // Replace the children vector with our updated one
    node.children = new_children;
}

// Check if a node is a branch back to the loop header
fn is_loop_back_branch(node: &IRNode, header_block: &str) -> bool {
    if let IRNodeType::Jump = node.node_type {
        if let Some(target) = &node.value {
            return target == header_block;
        }
    }
    false
}

// Common Subexpression Elimination with value numbering
fn common_subexpression_elimination(node: IRNode) -> Result<(IRNode, bool), String> {
    // Call the version that tracks statistics and discard the count
    let (result, changed, _) = common_subexpression_elimination_with_stats(node)?;
    Ok((result, changed))
}

// Version that tracks statistics
fn common_subexpression_elimination_with_stats(
    node: IRNode,
) -> Result<(IRNode, bool, usize), String> {
    let mut expr_map = HashMap::new();
    let mut changed = false;
    let mut eliminated_count = 0;

    // Apply CSE to the IR
    let (result, elim_count) = apply_cse_with_stats(node, &mut expr_map, &mut changed)?;
    eliminated_count += elim_count;

    Ok((result, changed, eliminated_count))
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
    // Call the version that tracks statistics and discard the count
    let (result, _) = apply_cse_with_stats(node, expr_map, changed)?;
    Ok(result)
}

fn apply_cse_with_stats(
    node: IRNode,
    expr_map: &mut HashMap<String, IRNode>,
    changed: &mut bool,
) -> Result<(IRNode, usize), String> {
    let mut result = node.clone();
    let mut eliminated_count = 0;

    // First optimize children recursively
    for i in 0..result.children.len() {
        let (new_child, child_count) =
            apply_cse_with_stats(result.children[i].clone(), expr_map, changed)?;
        result.children[i] = new_child;
        eliminated_count += child_count;
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
                eliminated_count += 1;
                return Ok((prev_expr.clone(), eliminated_count));
            } else {
                // First time seeing this expression, remember it
                expr_map.insert(key, result.clone());
            }
        }
        _ => {}
    }

    Ok((result, eliminated_count))
}

// Function-level optimizations
fn optimize_functions(node: IRNode) -> Result<(IRNode, bool), String> {
    // Call the version that tracks statistics and discard the count
    let (result, changed, _) = optimize_functions_with_stats(node)?;
    Ok((result, changed))
}

// Version that tracks statistics
fn optimize_functions_with_stats(node: IRNode) -> Result<(IRNode, bool, usize), String> {
    let mut result = node.clone();
    let mut changed = false;
    let mut optimized_count = 0;

    // Analyze function return patterns
    if let IRNodeType::Function = result.node_type {
        // Check if this function is a candidate for return value optimization
        let (can_optimize, return_value) = analyze_function_returns(&result);

        if can_optimize && return_value.is_some() {
            // This function always returns the same constant value
            println!("Function always returns constant: {:?}", return_value);

            // In a real compiler, we might replace calls to this function
            // or simplify its body

            // For this example, just mark that we detected the pattern
            changed = true;
            optimized_count += 1;
        }
    }

    // Recursively process children
    for i in 0..result.children.len() {
        let (new_child, child_changed, child_count) =
            optimize_functions_with_stats(result.children[i].clone())?;
        result.children[i] = new_child;
        changed |= child_changed;
        optimized_count += child_count;
    }

    Ok((result, changed, optimized_count))
}

// Analyze function return patterns
fn analyze_function_returns(func_node: &IRNode) -> (bool, Option<i32>) {
    let mut return_values = Vec::new();
    let mut has_multiple_returns = false;

    // Find all return statements
    collect_return_values(func_node, &mut return_values);

    // Check if all returns have the same constant value
    if return_values.is_empty() {
        return (false, None);
    }

    if return_values.len() > 1 {
        has_multiple_returns = true;

        // Check if all values are the same
        let first = return_values[0];
        if !return_values.iter().all(|&v| v == first) {
            return (false, None);
        }
    }

    // All returns have the same constant value
    (true, Some(return_values[0]))
}

// Collect all return statement values
fn collect_return_values(node: &IRNode, values: &mut Vec<i32>) {
    match node.node_type {
        IRNodeType::Return => {
            // Check if this return has a constant value
            if !node.children.is_empty() {
                if let IRNodeType::Constant = node.children[0].node_type {
                    if let Some(val_str) = &node.children[0].value {
                        if let Ok(val) = val_str.parse::<i32>() {
                            values.push(val);
                        }
                    }
                }
            }
        }
        _ => {
            // Recursively check children
            for child in &node.children {
                collect_return_values(child, values);
            }
        }
    }
}
