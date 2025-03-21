/*
This module is responsible for parsing the source code into an Abstract Syntax Tree (AST).
It includes functions for tokenizing the source code and building the AST, error handling, semantic analysis and syntax checking.
 */
mod ast;
mod declarations;
mod expressions;
mod parser_impl;
mod preprocessor;
mod statements;
mod tokenizer;
mod types;

use crate::parser::ast::build_ast;
use crate::parser::tokenizer::tokenize;

// use crate::parser::ast::{ASTNode, ASTNodeType};
// use crate::parser::ast::ASTNode;
// use crate::parser::semantic_analyzer::analyze_semantics;
// use crate::parser::error_handling::{Error, ErrorType};
// use crate::parser::semantic_analyzer::SemanticAnalyzer;

pub fn parse_source(source: &str) -> Result<(), String> {
    // Tokenize the source code
    let tokens = tokenize(source)?;

    // DEBUG

    println!("Tokens:");
    for token in &tokens {
        println!("{:?} ", token.token_type);
    }
    println!("Parsed and tokenized successfully\n");

    let ast = build_ast(&tokens)?;

    // DEBUG

    println!("AST Structure:");
    println!("└── Root");
    for (i, node) in ast.children.iter().enumerate() {
        let is_last = i == ast.children.len() - 1;
        print_ast_node(node, "", is_last);
    }

    println!("\nAST built successfully\n");

    // Perform semantic analysis and build the AST

    // Placeholder for return
    Ok(())
}

// Helper function to print the AST
fn print_ast_node(node: &ast::ASTNode, prefix: &str, is_last: bool) {
    let node_connector = if is_last { "└── " } else { "├── " };
    let node_info = format!("{:?}: {:?}", node.node_type, node.value);

    println!("{}{}{}", prefix, node_connector, node_info);

    if !node.children.is_empty() {
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };

        for (i, child) in node.children.iter().enumerate() {
            let is_last_child = i == node.children.len() - 1;
            print_ast_node(child, &child_prefix, is_last_child);
        }
    }
}
