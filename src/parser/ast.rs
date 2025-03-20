/*
This module defines the structure of the Abstract Syntax Tree (AST) for the compiler.
It includes the definition of AST nodes, their types, fields, and methods for creating and manipulating the AST. and the overall structure of the AST.
*/
use crate::parser::tokenizer::Token;
use crate::parser::types::ASTNodeType;

pub struct ASTNode {
    node_type: ASTNodeType,
    children: Vec<ASTNode>,
    value: Option<String>,
}

pub fn build_ast(tokens: &[Token]) -> Result<ASTNode, String> {
    // Placeholder for AST building logic
    // This function should take the tokens and build the AST
    // For now, we will just return a dummy AST node
    let ast = ASTNode {
        node_type: ASTNodeType::Program,
        children: vec![],
        value: None,
    };
    Ok(ast)
}
