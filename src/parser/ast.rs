/*
// This module defines the structure of the Abstract Syntax Tree (AST) for the compiler.
// It includes the definition of AST nodes, their types, fields, and methods for creating and manipulating the AST. and the overall structure of the AST.
// */

use crate::parser::tokenizer::Token;
use crate::parser::types::ASTNodeType;
#[derive(Debug)]

pub struct ASTNode {
    pub node_type: ASTNodeType,
    pub children: Vec<ASTNode>,
    pub value: Option<String>,
}

impl ASTNode {
    pub fn new(node_type: ASTNodeType, value: Option<String>) -> Self {
        ASTNode {
            node_type,
            children: vec![],
            value,
        }
    }

    pub fn add_child(&mut self, child: ASTNode) {
        self.children.push(child);
    }
}

// Re-export Parser struct so build_ast continues to work
pub use crate::parser::parser_impl::Parser;

pub fn build_ast(tokens: &[Token]) -> Result<ASTNode, String> {
    let mut parser = Parser::new(tokens);
    parser.parse_program()
}
