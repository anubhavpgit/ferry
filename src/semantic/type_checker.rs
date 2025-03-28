// src/semantic/type_checker.rs
use super::symbol_table::{SymbolTable, Type};
use crate::parser::ast::ASTNode;
use crate::parser::types::ASTNodeType;

fn check_binary_expression(node: &ASTNode, symbol_table: &SymbolTable) -> Result<Type, String> {
    if node.children.len() != 2 {
        return Err("Binary expression must have exactly two operands".to_string());
    }

    let left_type = check_types(&node.children[0], symbol_table)?;
    let right_type = check_types(&node.children[1], symbol_table)?;

    let operator = node
        .value
        .as_ref()
        .ok_or_else(|| "Binary expression must have an operator".to_string())?;

    // Implement type compatibility rules for binary operations
    // This is simplified and would need to be expanded
    match operator.as_str() {
        "+" | "-" | "*" | "/" => {
            if matches!(left_type, Type::Int) && matches!(right_type, Type::Int) {
                Ok(Type::Int)
            } else if matches!(left_type, Type::Float) || matches!(right_type, Type::Float) {
                Ok(Type::Float)
            } else {
                Err(format!("Invalid operand types for operator '{}'", operator))
            }
        }
        "==" | "!=" | "<" | ">" | "<=" | ">=" => {
            // Comparison operators result in boolean type
            Ok(Type::Bool)
        }
        _ => Err(format!("Unsupported binary operator '{}'", operator)),
    }
}

// Add to type_checker.rs
pub fn check_unary_expression(node: &ASTNode, symbol_table: &SymbolTable) -> Result<Type, String> {
    if node.children.len() != 1 {
        return Err("Unary expression must have exactly one operand".to_string());
    }

    let operand = &node.children[0];
    let operand_type = check_types(operand, symbol_table)?;

    let operator = node
        .value
        .as_ref()
        .ok_or_else(|| "Unary expression must have an operator".to_string())?;

    match operator.as_str() {
        "!" => {
            if matches!(operand_type, Type::Bool) {
                Ok(Type::Bool)
            } else {
                Err(format!(
                    "Logical NOT operator requires boolean operand, got {:?}",
                    operand_type
                ))
            }
        }
        "&" => {
            // Address-of operator returns a pointer to the operand type
            Ok(Type::Pointer(Box::new(operand_type)))
        }
        "-" => match operand_type {
            Type::Int => Ok(Type::Int),
            Type::Float => Ok(Type::Float),
            Type::Double => Ok(Type::Double),
            _ => Err(format!(
                "Numeric negation requires numeric operand, got {:?}",
                operand_type
            )),
        },
        // Add support for postfix operators
        "post++" | "post--" => match operand_type {
            Type::Int | Type::Float | Type::Double => Ok(operand_type),
            _ => Err(format!(
                "Increment/decrement operator requires numeric operand, got {:?}",
                operand_type
            )),
        },
        _ => Err(format!("Unsupported unary operator '{}'", operator)),
    }
}
pub fn check_function_call(node: &ASTNode, symbol_table: &SymbolTable) -> Result<Type, String> {
    let func_name = node
        .value
        .as_ref()
        .ok_or_else(|| "Function call missing name".to_string())?;

    let symbol = symbol_table
        .lookup(func_name)
        .ok_or_else(|| format!("Undefined function '{}'", func_name))?;

    match &symbol.symbol_type {
        Type::Variadic(return_type) => {
            // Ensure at least one argument (format string)
            if node.children.len() <= 1 {
                return Err(format!(
                    "Function '{}' requires at least a format string argument",
                    func_name
                ));
            }

            // Check that the first argument is a string
            let format_arg = &node.children[1];
            let format_type = check_types(format_arg, symbol_table)?;

            if !matches!(format_type, Type::Pointer(t) if matches!(*t, Type::Char)) {
                return Err(format!(
                    "First argument to '{}' must be a format string",
                    func_name
                ));
            }

            // Add this section to validate format strings if possible
            // Only perform validation if the format string is a literal that we can inspect
            if let ASTNodeType::Literal = format_arg.node_type {
                if let Some(format_str) = &format_arg.value {
                    if format_str.starts_with('"') && format_str.ends_with('"') {
                        // Extract the actual string content (remove quotes)
                        let content = &format_str[1..format_str.len() - 1];

                        // Get remaining arguments (excluding function name and format string)
                        let args = &node.children[2..];

                        // Determine if this is a scanf-family function
                        let is_scanf = func_name.contains("scanf");

                        // Validate format string against arguments
                        if let Err(err) =
                            validate_format_string(content, args, symbol_table, is_scanf)
                        {
                            return Err(err);
                        }
                    }
                }
            }

            // For scanf-family functions, check that remaining arguments are addresses
            if func_name.contains("scanf") {
                for i in 2..node.children.len() {
                    if let ASTNodeType::UnaryExpression = node.children[i].node_type {
                        if let Some(op) = &node.children[i].value {
                            if op != "&" {
                                return Err(format!(
                                    "Argument {} to {} should use the address-of operator",
                                    i - 1,
                                    func_name
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Argument {} to {} should use the address-of operator",
                                i - 1,
                                func_name
                            ));
                        }
                    } else {
                        return Err(format!(
                            "Argument {} to {} should use the address-of operator",
                            i - 1,
                            func_name
                        ));
                    }
                }
            }

            // Return the function's return type
            Ok(*return_type.clone())
        }
        Type::Function {
            return_type,
            params,
        } => {
            // Check argument count (first child is function name)
            let arg_count = node.children.len() - 1;
            if arg_count != params.len() {
                return Err(format!(
                    "Function '{}' called with {} arguments, but expected {}",
                    func_name,
                    arg_count,
                    params.len()
                ));
            }

            // Check each argument's type
            for (i, param_type) in params.iter().enumerate() {
                if i + 1 < node.children.len() {
                    let arg = &node.children[i + 1];
                    let mut_symbol_table = symbol_table;
                    let arg_type = check_types(arg, mut_symbol_table)?;

                    // Check type compatibility
                    if !is_type_compatible(param_type, &arg_type) {
                        return Err(format!(
                            "Type mismatch in argument {} of call to '{}': expected {:?}, got {:?}",
                            i + 1,
                            func_name,
                            param_type,
                            arg_type
                        ));
                    }
                }
            }

            // Return the function's return type
            Ok(*return_type.clone())
        }
        _ => Err(format!("'{}' is not a function", func_name)),
    }
}
// Update the check_types function to include these cases
pub fn check_types(node: &ASTNode, symbol_table: &SymbolTable) -> Result<Type, String> {
    match node.node_type {
        ASTNodeType::Literal => {
            // Determine type from the literal value
            let value = node
                .value
                .as_ref()
                .ok_or_else(|| "Literal node missing value".to_string())?;

            if value.starts_with('"') {
                Ok(Type::Pointer(Box::new(Type::Char))) // String literal
            } else if value.starts_with('\'') && value.ends_with('\'') {
                // Character literal
                let char_content = &value[1..value.len() - 1];
                if char_content.len() > 1 && !char_content.starts_with('\\') {
                    return Err(format!(
                        "Invalid character literal: '{}' - too many characters",
                        char_content
                    ));
                }
                Ok(Type::Char)
            } else if value == "true" || value == "false" {
                Ok(Type::Bool)
            } else {
                // Try to parse as number
                if value.contains('.') {
                    Ok(Type::Float)
                } else {
                    Ok(Type::Int)
                }
            }
        }
        ASTNodeType::Variable => {
            let name = node
                .value
                .as_ref()
                .ok_or_else(|| "Variable node missing name".to_string())?;

            let symbol = symbol_table
                .lookup(name)
                .ok_or_else(|| format!("Undefined variable '{}'", name))?;

            Ok(symbol.symbol_type.clone())
        }
        ASTNodeType::BinaryExpression => check_binary_expression(node, symbol_table),
        ASTNodeType::UnaryExpression => check_unary_expression(node, symbol_table),
        ASTNodeType::CallExpression => check_function_call(node, symbol_table),

        ASTNodeType::VariableDeclaration => {
            // Extract variable type from first child
            if node.children.is_empty() || !matches!(node.children[0].node_type, ASTNodeType::Type)
            {
                return Err("Variable declaration missing type".to_string());
            }

            let type_str = node.children[0]
                .value
                .as_ref()
                .ok_or_else(|| "Type node missing value".to_string())?;

            let var_type = parse_type_string(type_str)?;

            // If there's an initializer, check its type compatibility
            if node.children.len() > 1 {
                let init_type = check_types(&node.children[1], symbol_table)?;

                if !is_type_compatible(&var_type, &init_type) {
                    return Err(format!(
                        "Type mismatch in variable initialization: cannot assign {:?} to {:?}",
                        init_type, var_type
                    ));
                }
            }

            Ok(var_type)
        }

        ASTNodeType::FunctionDeclaration => {
            // Extract return type from first child
            if node.children.is_empty() || !matches!(node.children[0].node_type, ASTNodeType::Type)
            {
                return Err("Function declaration missing return type".to_string());
            }

            let return_type_str = node.children[0]
                .value
                .as_ref()
                .ok_or_else(|| "Type node missing value".to_string())?;

            let return_type = parse_type_string(return_type_str)?;

            // Extract parameter types
            let mut param_types = Vec::new();
            let mut param_index = 1;

            while param_index < node.children.len() {
                let child = &node.children[param_index];

                if matches!(child.node_type, ASTNodeType::VariableDeclaration) {
                    // Check parameter type
                    let param_type = check_types(child, symbol_table)?;
                    param_types.push(param_type);
                    param_index += 1;
                } else {
                    break;
                }
            }

            // Create function type
            let func_type = Type::Function {
                return_type: Box::new(return_type),
                params: param_types,
            };

            Ok(func_type)
        }

        ASTNodeType::Type => {
            // Just return the type specified by the node
            let type_str = node
                .value
                .as_ref()
                .ok_or_else(|| "Type node missing value".to_string())?;

            parse_type_string(type_str)
        }

        ASTNodeType::Assignment => {
            if node.children.len() != 2 {
                return Err("Assignment must have exactly two operands".to_string());
            }

            let target_type = check_types(&node.children[0], symbol_table)?;
            let value_type = check_types(&node.children[1], symbol_table)?;

            if !is_type_compatible(&target_type, &value_type) {
                return Err(format!(
                    "Type mismatch in assignment: cannot assign {:?} to {:?}",
                    value_type, target_type
                ));
            }

            Ok(target_type)
        }

        ASTNodeType::ReturnStatement => {
            // If there's a return value, check its type
            // In a complete implementation, you would verify this against the function's return type
            if node.children.is_empty() {
                Ok(Type::Void)
            } else {
                check_types(&node.children[0], symbol_table)
            }
        }

        ASTNodeType::ExpressionStatement => {
            // Pass through to the contained expression
            if node.children.is_empty() {
                return Err("Expression statement has no expression".to_string());
            }

            check_types(&node.children[0], symbol_table)
        }

        ASTNodeType::IfStatement | ASTNodeType::WhileStatement | ASTNodeType::DoWhileStatement => {
            // For if/while/do-while, check that the condition is a boolean expression
            let condition_index = if matches!(node.node_type, ASTNodeType::DoWhileStatement) {
                1
            } else {
                0
            };

            if node.children.len() <= condition_index {
                return Err(format!("{:?} statement missing condition", node.node_type));
            }

            let cond_type = check_types(&node.children[condition_index], symbol_table)?;

            if !matches!(cond_type, Type::Bool) && !matches!(cond_type, Type::Int) {
                return Err(format!(
                    "Condition must be a boolean or integer expression, got {:?}",
                    cond_type
                ));
            }

            // For if statements with else clause, both branches must be checked
            // For loops, the body must be checked
            // But this doesn't affect the resulting type

            Ok(Type::Void) // Control flow statements don't have a meaningful type
        }

        ASTNodeType::ForStatement => {
            // Check initialization
            if node.children.len() < 4 {
                return Err(
                    "For statement must have initialization, condition, increment, and body"
                        .to_string(),
                );
            }

            let _ = check_types(&node.children[0], symbol_table)?;

            // Check condition (must be boolean)
            let cond_type = check_types(&node.children[1], symbol_table)?;

            if !matches!(cond_type, Type::Bool) && !matches!(cond_type, Type::Int) {
                return Err(format!(
                    "For condition must be a boolean or integer expression, got {:?}",
                    cond_type
                ));
            }

            // Check increment and body (types not important)
            let _ = check_types(&node.children[2], symbol_table)?;
            let _ = check_types(&node.children[3], symbol_table)?;

            Ok(Type::Void) // Control flow statements don't have a meaningful type
        }

        ASTNodeType::BlockStatement => {
            // Process all statements in block, return type of last expression (if any)
            let mut result_type = Type::Void;

            for child in &node.children {
                result_type = check_types(child, symbol_table)?;
            }

            Ok(result_type)
        }

        ASTNodeType::GroupingExpression => {
            // Pass through to the contained expression
            if node.children.is_empty() {
                return Err("Grouping expression has no expression".to_string());
            }

            check_types(&node.children[0], symbol_table)
        }

        ASTNodeType::SwitchStatement => {
            // Check that switch expression is an integer
            if node.children.is_empty() {
                return Err("Switch statement missing expression".to_string());
            }

            let expr_type = check_types(&node.children[0], symbol_table)?;

            if !matches!(expr_type, Type::Int) && !matches!(expr_type, Type::Char) {
                return Err(format!(
                    "Switch expression must be an integer or character, got {:?}",
                    expr_type
                ));
            }

            // Check case statements (not critical for type checking)
            for i in 1..node.children.len() {
                let _ = check_types(&node.children[i], symbol_table)?;
            }

            Ok(Type::Void)
        }

        ASTNodeType::CaseStatement | ASTNodeType::DefaultStatement => {
            // Check case expression (must be constant integer)
            if matches!(node.node_type, ASTNodeType::CaseStatement) && !node.children.is_empty() {
                let case_type = check_types(&node.children[0], symbol_table)?;

                if !matches!(case_type, Type::Int) && !matches!(case_type, Type::Char) {
                    return Err(format!(
                        "Case expression must be an integer or character constant, got {:?}",
                        case_type
                    ));
                }
            }

            // Process case body statements
            for i in 1..node.children.len() {
                let _ = check_types(&node.children[i], symbol_table)?;
            }

            Ok(Type::Void)
        }

        ASTNodeType::BreakStatement | ASTNodeType::ContinueStatement => {
            // These statements don't have a type
            Ok(Type::Void)
        }

        ASTNodeType::PreprocessorDirective => {
            // Preprocessor directives don't have a type
            Ok(Type::Void)
        }

        ASTNodeType::Program => {
            // Process all top-level declarations
            let mut result_type = Type::Void;

            for child in &node.children {
                result_type = check_types(child, symbol_table)?;
            }

            Ok(result_type)
        }

        // Any other node types not explicitly handled
        _ => Err(format!(
            "Type checking not implemented for {:?}",
            node.node_type
        )),
    }
}

// Helper function to parse type strings
fn parse_type_string(type_str: &str) -> Result<Type, String> {
    match type_str {
        "Int" => Ok(Type::Int),
        "Void" => Ok(Type::Void),
        "Char" => Ok(Type::Char),
        "Float" => Ok(Type::Float),
        "Double" => Ok(Type::Double),
        "Bool" => Ok(Type::Bool),
        _ => Err(format!("Unsupported type: {}", type_str)),
    }
}
pub fn is_type_compatible(target_type: &Type, value_type: &Type) -> bool {
    match (target_type, value_type) {
        // Basic types
        (Type::Int, Type::Int) => true,
        (Type::Float, Type::Int) => true, // Implicit conversion
        (Type::Float, Type::Float) => true,
        (Type::Double, Type::Int) => true,   // Implicit conversion
        (Type::Double, Type::Float) => true, // Implicit conversion
        (Type::Double, Type::Double) => true,
        (Type::Bool, Type::Bool) => true,
        (Type::Char, Type::Char) => true,
        (Type::Void, Type::Void) => true,

        (Type::Pointer(target_inner), _) if matches!(**target_inner, Type::Char) => {
            // Allow string literals to be assigned to char* (for printf format strings)
            if let Type::Pointer(value_inner) = value_type {
                if matches!(**value_inner, Type::Char) {
                    return true;
                }
            }
            false
        }

        // Pointer types
        (Type::Pointer(target_inner), Type::Pointer(value_inner)) => {
            is_type_compatible(target_inner, value_inner)
        }

        // Void pointer compatibility (void* can be assigned from any pointer)
        (Type::Pointer(target_inner), _) if matches!(**target_inner, Type::Void) => {
            matches!(value_type, Type::Pointer(_))
        }

        // Any pointer can be assigned to void*
        (_, Type::Pointer(value_inner)) if matches!(**value_inner, Type::Void) => {
            matches!(target_type, Type::Pointer(_))
        }

        // Function types
        (
            Type::Function {
                return_type: target_return,
                params: target_params,
            },
            Type::Function {
                return_type: value_return,
                params: value_params,
            },
        ) => {
            // Functions are compatible if their return types and parameter lists match
            if !is_type_compatible(target_return, value_return) {
                return false;
            }

            if target_params.len() != value_params.len() {
                return false;
            }

            // Check each parameter type
            for (target_param, value_param) in target_params.iter().zip(value_params.iter()) {
                if !is_type_compatible(target_param, value_param) {
                    return false;
                }
            }

            true
        }

        // Array types
        (
            Type::Array {
                element_type: target_elem,
                size: _,
            },
            Type::Array {
                element_type: value_elem,
                size: _,
            },
        ) => {
            // In C, array size isn't strictly checked in all contexts
            is_type_compatible(target_elem, value_elem)
        }

        // Struct types (typically only compatible with identical types)
        (Type::Struct(target_name), Type::Struct(value_name)) => target_name == value_name,

        // For any other combinations, types are not compatible
        _ => false,
    }
}

fn validate_format_string(
    format_str: &str,
    args: &[ASTNode],
    symbol_table: &SymbolTable,
    is_scanf: bool,
) -> Result<(), String> {
    // Extract format specifiers (%d, %s, etc.)
    let format_specifiers = extract_format_specifiers(format_str);

    // Check the number of format specifiers matches the number of arguments
    if format_specifiers.len() != args.len() {
        return Err(format!(
            "Number of format specifiers ({}) doesn't match number of arguments ({})",
            format_specifiers.len(),
            args.len()
        ));
    }

    // Check each format specifier against the corresponding argument
    for (i, specifier) in format_specifiers.iter().enumerate() {
        let arg = &args[i];
        let arg_type = check_types(arg, symbol_table)?;

        if !is_type_compatible_with_format_specifier(specifier, &arg_type, is_scanf) {
            return Err(format!(
                "Argument {} type mismatch for format specifier '{}'",
                i + 1,
                specifier
            ));
        }
    }

    Ok(())
}

fn extract_format_specifiers(format_str: &str) -> Vec<String> {
    // Extract format specifiers like %d, %s, etc.
    let mut specifiers = Vec::new();
    let mut chars = format_str.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            if let Some(&next) = chars.peek() {
                if next != '%' {
                    // Found a format specifier
                    let mut specifier = String::from('%');
                    specifier.push(next);
                    specifiers.push(specifier);
                }
                // Skip the character after %
                chars.next();
            }
        }
    }

    specifiers
}

fn is_type_compatible_with_format_specifier(
    specifier: &str,
    arg_type: &Type,
    is_scanf: bool,
) -> bool {
    match specifier {
        "%d" | "%i" => {
            if is_scanf {
                // For scanf, need a pointer to int
                matches!(arg_type, Type::Pointer(t) if matches!(**t, Type::Int))
            } else {
                // For printf, need an int
                matches!(arg_type, Type::Int)
            }
        }
        "%f" => {
            if is_scanf {
                // For scanf, need a pointer to float
                matches!(arg_type, Type::Pointer(t) if matches!(**t, Type::Float))
            } else {
                // For printf, need a float
                matches!(arg_type, Type::Float)
            }
        }
        "%c" => {
            if is_scanf {
                // For scanf, need a pointer to char
                matches!(arg_type, Type::Pointer(t) if matches!(**t, Type::Char))
            } else {
                // For printf, need a char
                matches!(arg_type, Type::Char)
            }
        }
        "%s" => {
            // For both printf and scanf, need a char* (string)
            matches!(arg_type, Type::Pointer(t) if matches!(**t, Type::Char))
        }
        // Add other format specifiers as needed
        _ => false,
    }
}
