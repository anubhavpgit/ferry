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

    for token in &tokens {
        println!("{:?} ", token.token_type);
    }
    println!(" Parsed and tokenized successfully\n");

    let _ast = build_ast(&tokens)?;

    for node in _ast.children {
        println!("{:?} ", node.node_type);
        println!("{:?} ", node.value);
        println!("{:?} ", node.children);
        println!("-----------------");
    }

    println!(" AST built successfully\n");

    // Perform semantic analysis and build the AST

    // Placeholder for return
    Ok(())
}
