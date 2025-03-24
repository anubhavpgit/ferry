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
        "-" => match operand_type {
            Type::Int => Ok(Type::Int),
            Type::Float => Ok(Type::Float),
            Type::Double => Ok(Type::Double),
            _ => Err(format!(
                "Numeric negation requires numeric operand, got {:?}",
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
            // This is simplified - we'd need more sophisticated parsing in a real compiler
            let value = node
                .value
                .as_ref()
                .ok_or_else(|| "Literal node missing value".to_string())?;

            if value.starts_with('"') {
                Ok(Type::Pointer(Box::new(Type::Char))) // String literal
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
        _ => Err(format!(
            "Type checking not implemented for {:?}",
            node.node_type
        )),
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
