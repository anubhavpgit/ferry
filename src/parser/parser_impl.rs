// parser_impl.rs - Move the base Parser struct here
use crate::parser::ast::ASTNode;
use crate::parser::tokenizer::Token;
use crate::parser::types::{ASTNodeType, TokenType};

pub struct Parser<'a> {
    pub tokens: &'a [Token],
    pub current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn match_token(&mut self, types: &[TokenType]) -> bool {
        for type_to_match in types {
            if self.check_type(type_to_match) {
                self.advance();
                return true;
            }
        }
        false
    }

    pub fn check_type(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        match (token_type, &self.peek().token_type) {
            (TokenType::Number(_), TokenType::Number(_)) => true,
            (TokenType::String(_), TokenType::String(_)) => true,
            (TokenType::Char(_), TokenType::Char(_)) => true,
            (TokenType::Identifier(_), TokenType::Identifier(_)) => true,
            _ => {
                std::mem::discriminant(&self.peek().token_type)
                    == std::mem::discriminant(token_type)
            }
        }
    }

    pub fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(&token_type)
        }
    }

    pub fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(message.to_string())
        }
    }

    pub fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    pub fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::EOF)
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    // Entry point (delegates to self)
    pub fn parse_program(&mut self) -> Result<ASTNode, String> {
        let mut program = ASTNode::new(ASTNodeType::Program, None);

        while !self.is_at_end() {
            if let TokenType::Comment(_) = self.peek().token_type {
                self.advance(); // Simply consume and ignore comment tokens
                continue;
            }
            match &self.peek().token_type {
                TokenType::Type(_) => {
                    let declaration = self.parse_declaration()?;
                    program.add_child(declaration);
                }
                TokenType::PreprocessorDirective(_) => {
                    let directive = self.parse_preprocessor()?;
                    program.add_child(directive);
                }
                _ => {
                    return Err(format!(
                        "Expected declaration, found {:?}.",
                        self.peek().token_type
                    ));
                }
            }
        }

        Ok(program)
    }
}
