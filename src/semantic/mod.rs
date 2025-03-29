pub mod preprocessor;
pub mod symbol_table;
pub mod type_checker;

use crate::parser::ast::ASTNode;
use crate::parser::types::ASTNodeType;
use crate::semantic::preprocessor::{process_preprocessor_directives, remove_preprocessor_nodes};
use crate::semantic::type_checker::is_type_compatible;

pub fn analyze_semantics(mut ast: ASTNode) -> Result<ASTNode, String> {
    // Main entry point for semantic analysis
    let mut errors = Vec::new();
    let mut symbol_table = symbol_table::SymbolTable::new();
    let mut function_context = None;

    process_preprocessor_directives(&ast, &mut symbol_table);
    // Remove preprocessor directives from the AST
    remove_preprocessor_nodes(&mut ast);

    // Traverse AST and analyze
    analyze_node(&ast, &mut symbol_table, &mut errors, &mut function_context);

    if errors.is_empty() {
        Ok(ast)
    } else {
        // Format all errors into a single string
        Err(format!("{}", errors.join("\n")))
    }
}

struct FunctionContext {
    function_name: String,
    return_type: symbol_table::Type,
    has_return: bool,
}

fn analyze_node(
    node: &ASTNode,
    symbol_table: &mut symbol_table::SymbolTable,
    errors: &mut Vec<String>,
    function_context: &mut Option<FunctionContext>,
) {
    match &node.node_type {
        ASTNodeType::Program => {
            // Process all top-level declarations
            for child in &node.children {
                analyze_node(child, symbol_table, errors, function_context);
            }
        }

        ASTNodeType::FunctionDeclaration | ASTNodeType::Function => {
            // Extract function name
            let name = match &node.value {
                Some(name) => name.clone(),
                None => {
                    errors.push("Function declaration missing name".to_string());
                    return;
                }
            };

            // Extract return type
            let return_type = if !node.children.is_empty()
                && matches!(node.children[0].node_type, ASTNodeType::Type)
            {
                match &node.children[0].value {
                    Some(type_str) => match parse_type_string(type_str) {
                        Ok(t) => t,
                        Err(e) => {
                            errors.push(e);
                            symbol_table::Type::Void // Default for error recovery
                        }
                    },
                    None => {
                        errors.push("Function return type missing".to_string());
                        symbol_table::Type::Void // Default for error recovery
                    }
                }
            } else {
                errors.push("Function declaration missing return type".to_string());
                symbol_table::Type::Void
            };

            // Process parameters as before...
            let mut params = Vec::new();
            let mut param_index = 1; // Start after return type

            // Collect parameter information
            while param_index < node.children.len() {
                let child = &node.children[param_index];

                if matches!(child.node_type, ASTNodeType::VariableDeclaration) {
                    // Extract parameter type
                    if !child.children.is_empty()
                        && matches!(child.children[0].node_type, ASTNodeType::Type)
                    {
                        if let Some(type_str) = &child.children[0].value {
                            match parse_type_string(type_str) {
                                Ok(t) => params.push(t),
                                Err(e) => errors.push(e),
                            }
                        }
                    }
                    param_index += 1;
                } else {
                    // Found function body or something else
                    break;
                }
            }

            // Register function in symbol table
            let func_type = symbol_table::Type::Function {
                return_type: Box::new(return_type.clone()),
                params: params.clone(),
            };

            if let Err(e) = symbol_table.define(name.clone(), func_type, true) {
                errors.push(e);
            }

            // Create new scope for function body
            symbol_table.enter_scope();

            // Create new function context
            let new_context = FunctionContext {
                function_name: name.clone(),
                return_type: return_type.clone(),
                has_return: false,
            };

            // Save the old context to restore later
            let old_context = function_context.take();
            *function_context = Some(new_context);

            // Add parameters to function scope
            param_index = 1;
            while param_index < node.children.len() {
                let child = &node.children[param_index];

                if matches!(child.node_type, ASTNodeType::VariableDeclaration) {
                    analyze_node(child, symbol_table, errors, function_context);
                    param_index += 1;
                } else {
                    break;
                }
            }

            // Process function body if present
            if param_index < node.children.len() {
                analyze_node(
                    &node.children[param_index],
                    symbol_table,
                    errors,
                    function_context,
                );
            }

            // Check for return statement in non-void functions
            if let Some(ctx) = function_context.take() {
                if !matches!(ctx.return_type, symbol_table::Type::Void)
                    && !ctx.has_return
                    && ctx.function_name != "main"
                {
                    // main is special in C, can implicitly return 0
                    errors.push(format!(
                        "Function '{}' has return type {:?} but no return statement",
                        ctx.function_name, ctx.return_type
                    ));
                }
            }

            // Restore the old context
            *function_context = old_context;

            // Exit function scope
            symbol_table.exit_scope();
        }

        ASTNodeType::ReturnStatement => {
            // Check if we're in a function context
            if let Some(ctx) = function_context.as_mut() {
                // Mark that we've found a return statement
                ctx.has_return = true;

                // Check return value type if present
                if !node.children.is_empty() {
                    let return_expr = &node.children[0];
                    match type_checker::check_types(return_expr, symbol_table) {
                        Ok(expr_type) => {
                            // Check compatibility with function return type
                            if !is_type_compatible(&ctx.return_type, &expr_type) {
                                errors.push(format!(
                                    "Return type mismatch in function '{}': expected {:?}, found {:?}",
                                    ctx.function_name, ctx.return_type, expr_type
                                ));
                            }
                        }
                        Err(e) => errors.push(format!("Error in return expression: {}", e)),
                    }
                } else if !matches!(ctx.return_type, symbol_table::Type::Void) {
                    // Empty return in non-void function
                    errors.push(format!(
                        "Function '{}' has return type {:?} but empty return statement",
                        ctx.function_name, ctx.return_type
                    ));
                }
            } else {
                errors.push("Return statement outside of function".to_string());
            }
        }

        // The rest of the match cases remain the same, but we need to pass the function context
        ASTNodeType::VariableDeclaration => {
            // Code remains the same but we need to pass function_context to recursive calls
            // Extract variable name and type
            let name = match &node.value {
                Some(name) => name.clone(),
                None => {
                    errors.push("Variable declaration missing name".to_string());
                    return;
                }
            };

            let var_type = if !node.children.is_empty()
                && matches!(node.children[0].node_type, ASTNodeType::Type)
            {
                match &node.children[0].value {
                    Some(type_str) => match parse_type_string(type_str) {
                        Ok(t) => t,
                        Err(e) => {
                            errors.push(e);
                            symbol_table::Type::Void // Default for error recovery
                        }
                    },
                    None => {
                        errors.push("Variable type missing".to_string());
                        symbol_table::Type::Void
                    }
                }
            } else {
                errors.push("Variable declaration missing type".to_string());
                symbol_table::Type::Void
            };

            // Register variable in symbol table
            let has_initializer = node.children.len() > 1;
            if let Err(e) = symbol_table.define(name, var_type.clone(), has_initializer) {
                errors.push(e);
            }

            // Type check initializer if present
            if has_initializer {
                let initializer = &node.children[1];
                match type_checker::check_types(initializer, &symbol_table) {
                    Ok(init_type) => {
                        if !is_type_compatible(&var_type, &init_type) {
                            errors.push(format!(
                                "Type mismatch in variable initialization: cannot assign {:?} to {:?}",
                                init_type, var_type
                            ));
                        }
                    }
                    Err(e) => errors.push(format!("Error in initializer: {}", e)),
                }
            }
        }

        ASTNodeType::BlockStatement => {
            // First, check if this is a function body block with a nested declaration block
            let is_function_body_with_declarations = 
                !node.children.is_empty() && 
                matches!(node.children[0].node_type, ASTNodeType::BlockStatement) &&
                node.children[0].children.iter().all(|child| 
                    matches!(child.node_type, ASTNodeType::VariableDeclaration)
                );
        
            // Create new scope for block
            symbol_table.enter_scope();
        
            // If this is a function with declaration block, handle it specially
            if is_function_body_with_declarations {
                let decl_block = &node.children[0];
                
                // Process all variable declarations in the declaration block
                for var_decl in &decl_block.children {
                    analyze_node(var_decl, symbol_table, errors, function_context);
                }
                
                // Process all other statements in the function body
                for child in &node.children[1..] {
                    analyze_node(child, symbol_table, errors, function_context);
                }
            } else {
                // Regular block processing (unchanged)
                for child in &node.children {
                    if matches!(child.node_type, ASTNodeType::VariableDeclaration) {
                        // Your existing variable declaration handling
                        let name = match &child.value {
                            Some(name) => name.clone(),
                            None => {
                                errors.push("Variable declaration missing name".to_string());
                                continue;
                            }
                        };
        
                        let var_type = if !child.children.is_empty()
                            && matches!(child.children[0].node_type, ASTNodeType::Type)
                        {
                            match &child.children[0].value {
                                Some(type_str) => match parse_type_string(type_str) {
                                    Ok(t) => t,
                                    Err(e) => {
                                        errors.push(e);
                                        symbol_table::Type::Void
                                    }
                                },
                                None => {
                                    errors.push("Variable type missing".to_string());
                                    symbol_table::Type::Void
                                }
                            }
                        } else {
                            errors.push("Variable declaration missing type".to_string());
                            symbol_table::Type::Void
                        };
        
                        if let Err(e) = symbol_table.define(name, var_type, false) {
                            errors.push(e);
                        }
                    } else {
                        analyze_node(child, symbol_table, errors, function_context);
                    }
                }
            }
        
            // Exit block scope
            symbol_table.exit_scope();
        }
        ASTNodeType::ExpressionStatement => {
            // Process the contained expression
            if !node.children.is_empty() {
                analyze_node(&node.children[0], symbol_table, errors, function_context);
            }
        }

        ASTNodeType::Assignment => {
            // Same code, just pass function_context to recursive calls
            if node.children.len() != 2 {
                errors.push("Assignment must have exactly two operands".to_string());
                return;
            }

            // Process target (left side)
            let target = &node.children[0];

            if !matches!(target.node_type, ASTNodeType::Variable) {
                errors.push("Left side of assignment must be a variable".to_string());
                return;
            }

            let var_name = match &target.value {
                Some(name) => name,
                None => {
                    errors.push("Variable name missing in assignment".to_string());
                    return;
                }
            };

            // Check if variable exists and get its type
            let target_type = match symbol_table.lookup(var_name) {
                Some(symbol) => symbol.symbol_type.clone(),
                None => {
                    print!("Here");
                    errors.push(format!("Undefined variable '{}'", var_name));
                    return;
                }
            };

            // Process and type check the value expression
            let value = &node.children[1];
            match type_checker::check_types(value, symbol_table) {
                Ok(value_type) => {
                    if !is_type_compatible(&target_type, &value_type) {
                        errors.push(format!(
                            "Type mismatch in assignment: cannot assign {:?} to {:?}",
                            value_type, target_type
                        ));
                    }
                }
                Err(e) => errors.push(format!("Error in assignment value: {}", e)),
            }
        }

        // Continue updating all other node cases to pass function_context
        ASTNodeType::IfStatement => {
            if node.children.is_empty() {
                errors.push("If statement missing condition".to_string());
                return;
            }

            // Type check condition
            let condition = &node.children[0];
            match type_checker::check_types(condition, symbol_table) {
                Ok(cond_type) => {
                    if !matches!(cond_type, symbol_table::Type::Bool) {
                        errors.push(format!(
                            "If condition must be a boolean expression, got {:?}",
                            cond_type
                        ));
                    }
                }
                Err(e) => errors.push(format!("Error in if condition: {}", e)),
            }

            // Process then and else branches
            if node.children.len() > 1 {
                analyze_node(&node.children[1], symbol_table, errors, function_context);
            }

            if node.children.len() > 2 {
                analyze_node(&node.children[2], symbol_table, errors, function_context);
            }
        }

        ASTNodeType::WhileStatement | ASTNodeType::DoWhileStatement => {
            // Same but pass function_context
            if node.children.len() < 2 {
                errors.push(format!(
                    "{:?} statement must have condition and body",
                    node.node_type
                ));
                return;
            }

            // Get indices for condition and body based on statement type
            let (cond_idx, body_idx) = match node.node_type {
                ASTNodeType::WhileStatement => (0, 1),
                ASTNodeType::DoWhileStatement => (1, 0),
                _ => unreachable!(),
            };

            // Type check condition
            let condition = &node.children[cond_idx];
            match type_checker::check_types(condition, symbol_table) {
                Ok(cond_type) => {
                    if !matches!(cond_type, symbol_table::Type::Bool) {
                        errors.push(format!(
                            "Loop condition must be a boolean expression, got {:?}",
                            cond_type
                        ));
                    }
                }
                Err(e) => errors.push(format!("Error in loop condition: {}", e)),
            }

            // Process loop body
            analyze_node(
                &node.children[body_idx],
                symbol_table,
                errors,
                function_context,
            );
        }

        ASTNodeType::ForStatement => {
            // Same but pass function_context
            if node.children.len() < 4 {
                errors.push(
                    "For statement must have initialization, condition, increment, and body"
                        .to_string(),
                );
                return;
            }

            // Process initialization
            analyze_node(&node.children[0], symbol_table, errors, function_context);

            // Type check condition
            let condition = &node.children[1];
            match type_checker::check_types(condition, symbol_table) {
                Ok(cond_type) => {
                    if !matches!(cond_type, symbol_table::Type::Bool) {
                        errors.push(format!(
                            "For condition must be a boolean expression, got {:?}",
                            cond_type
                        ));
                    }
                }
                Err(e) => errors.push(format!("Error in for condition: {}", e)),
            }

            // Process increment and body
            analyze_node(&node.children[2], symbol_table, errors, function_context);
            analyze_node(&node.children[3], symbol_table, errors, function_context);
        }

        ASTNodeType::CallExpression => {
            // Extract function name
            let func_name = match &node.value {
                Some(name) => name,
                None => {
                    errors.push("Function call missing name".to_string());
                    return;
                }
            };
        
            // Check if function exists and has correct type
            match symbol_table.lookup(func_name) {
                Some(symbol) => match &symbol.symbol_type {
                    symbol_table::Type::Function {
                        return_type,
                        params,
                    } => {
                        // Check argument count (first child is callee)
                        let arg_count = node.children.len() - 1;
                        if arg_count != params.len() {
                            errors.push(format!(
                                "Function '{}' called with {} arguments, but expected {}",
                                func_name, arg_count, params.len()
                            ));
                            return;
                        }
        
                        // Type check each argument
                        for (i, param_type) in params.iter().enumerate() {
                            if i + 1 < node.children.len() {
                                let arg = &node.children[i + 1];
                                match type_checker::check_types(arg, symbol_table) {
                                    Ok(arg_type) => {
                                        if !is_type_compatible(param_type, &arg_type) {
                                            errors.push(format!(
                                                "Type mismatch in argument {} of call to '{}': expected {:?}, got {:?}",
                                                i + 1, func_name, param_type, arg_type
                                            ));
                                        }
                                    }
                                    Err(e) => errors.push(format!("Error in function argument: {}", e)),
                                }
                            }
                        }
                    },
                    symbol_table::Type::Variadic(return_type) => {
                        // For variadic functions like printf/scanf, we're more flexible
                        // First argument (format string) should be handled specially
                        if node.children.len() <= 1 {
                            errors.push(format!(
                                "Variadic function '{}' requires at least one argument",
                                func_name
                            ));
                            return;
                        }
                        
                        // Basic check for the format string argument (first argument)
                        let format_arg = &node.children[1];
                        match type_checker::check_types(format_arg, symbol_table) {
                            Ok(arg_type) => {
                                // Format string should typically be a char* (string)
                                if !matches!(arg_type, symbol_table::Type::Pointer(t) 
                                          if matches!(*t, symbol_table::Type::Char)) {
                                    errors.push(format!(
                                        "First argument to '{}' should be a format string",
                                        func_name
                                    ));
                                }
                            },
                            Err(e) => errors.push(format!("Error in format string argument: {}", e)),
                        }
                        
                        // For scanf-family functions, remaining args should be pointers
                        if func_name.contains("scanf") {
                            for i in 2..node.children.len() {
                                let arg = &node.children[i];
                                // Check for address-of operator (&) for scanf arguments
                                if !matches!(arg.node_type, ASTNodeType::UnaryExpression) 
                                    || arg.value.as_ref().map_or(true, |op| op != "&") {
                                    errors.push(format!(
                                        "Argument {} to {} should use the address-of operator (&)",
                                        i - 1, func_name
                                    ));
                                }
                            }
                        }
                    },
                    _ => {
                        errors.push(format!("'{}' is not a function", func_name));
                        return;
                    }
                },
                None => {
                    errors.push(format!("Undefined function '{}'", func_name));
                    return;
                }
            }
        },

        // These node types are handled by type_checker directly
        ASTNodeType::BinaryExpression
        | ASTNodeType::UnaryExpression
        | ASTNodeType::Literal
        | ASTNodeType::Variable => {
            if let Err(e) = type_checker::check_types(node, symbol_table) {
                errors.push(e);
            }
        }

        ASTNodeType::BreakStatement | ASTNodeType::ContinueStatement => {
            // Ideally we would check if these statements are inside a loop
            // This would require tracking loop context
        }

        ASTNodeType::PreprocessorDirective => {
            // No semantic checks needed for preprocessor directives
        }

        // For any unhandled node types, recursively process children
        _ => {
            for child in &node.children {
                analyze_node(child, symbol_table, errors, function_context);
            }
        }
    }
}

// Helper functions for type handling
fn parse_type_string(type_str: &str) -> Result<symbol_table::Type, String> {
    match type_str {
        "Int" => Ok(symbol_table::Type::Int),
        "Void" => Ok(symbol_table::Type::Void),
        "Char" => Ok(symbol_table::Type::Char),
        "Float" => Ok(symbol_table::Type::Float),
        "Double" => Ok(symbol_table::Type::Double),
        "Bool" => Ok(symbol_table::Type::Bool),
        _ => Err(format!("Unsupported type: {}", type_str)),
    }
}
