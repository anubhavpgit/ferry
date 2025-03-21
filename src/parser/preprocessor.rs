use crate::parser::ast::ASTNode;
use crate::parser::parser_impl::Parser;
use crate::parser::types::{ASTNodeType, TokenType};

impl<'a> Parser<'a> {
    pub fn parse_preprocessor(&mut self) -> Result<ASTNode, String> {
        // Extract the directive text
        let directive = match &self.peek().token_type {
            TokenType::PreprocessorDirective(text) => text.clone(),
            _ => return Err("Expected preprocessor directive".to_string()),
        };

        // Consume the directive token
        self.advance();

        // Create an AST node for the directive
        Ok(ASTNode::new(
            ASTNodeType::PreprocessorDirective,
            Some(directive),
        ))
    }
}
