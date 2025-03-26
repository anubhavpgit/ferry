use crate::parser::ast::ASTNode;
use crate::parser::parser_impl::Parser;
use crate::parser::types::{ASTNodeType, Keyword, TokenType};

impl<'a> Parser<'a> {
    pub fn parse_function_call(&mut self, name: String) -> Result<ASTNode, String> {
        // Create the call expression node
        let mut call_node = ASTNode::new(ASTNodeType::CallExpression, Some(name.clone()));

        // Add the callee (function name) as first child
        let callee = ASTNode::new(ASTNodeType::Variable, Some(name));
        call_node.add_child(callee);

        // Parse arguments
        self.consume(TokenType::LeftParen, "Expected '(' after function name.")?;

        // Parse comma-separated arguments
        if !self.check(TokenType::RightParen) {
            loop {
                let arg = self.parse_expression()?;
                call_node.add_child(arg);

                if !self.check(TokenType::Comma) {
                    break;
                }

                self.advance(); // Consume the comma
            }
        }

        self.consume(
            TokenType::RightParen,
            "Expected ')' after function arguments.",
        )?;

        Ok(call_node)
    }

    pub fn parse_expression(&mut self) -> Result<ASTNode, String> {
        self.parse_assignment()
    }

    pub fn parse_assignment(&mut self) -> Result<ASTNode, String> {
        let expr = self.parse_equality()?;

        if self.match_token(&[TokenType::Equal]) {
            let value = self.parse_assignment()?;

            match expr.node_type {
                ASTNodeType::Variable => {
                    return Ok(ASTNode {
                        node_type: ASTNodeType::Assignment,
                        children: vec![expr, value],
                        value: None,
                    });
                }
                _ => return Err("Invalid assignment target.".to_string()),
            }
        }

        Ok(expr)
    }

    pub fn parse_equality(&mut self) -> Result<ASTNode, String> {
        let mut expr = self.parse_comparison()?;

        while self.match_token(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = match self.previous().token_type {
                TokenType::EqualEqual => "==",
                TokenType::BangEqual => "!=",
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;

            let mut binary_expr =
                ASTNode::new(ASTNodeType::BinaryExpression, Some(operator.to_string()));
            binary_expr.add_child(expr);
            binary_expr.add_child(right);
            expr = binary_expr;
        }

        Ok(expr)
    }

    pub fn parse_comparison(&mut self) -> Result<ASTNode, String> {
        let mut expr = self.parse_term()?;

        while self.match_token(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
            let operator = match self.previous().token_type {
                TokenType::Less => "<",
                TokenType::LessEqual => "<=",
                TokenType::Greater => ">",
                TokenType::GreaterEqual => ">=",
                _ => unreachable!(),
            };
            let right = self.parse_term()?;

            let mut binary_expr =
                ASTNode::new(ASTNodeType::BinaryExpression, Some(operator.to_string()));
            binary_expr.add_child(expr);
            binary_expr.add_child(right);
            expr = binary_expr;
        }

        Ok(expr)
    }

    pub fn parse_term(&mut self) -> Result<ASTNode, String> {
        let mut expr = self.parse_factor()?;

        while self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            let operator = match self.previous().token_type {
                TokenType::Plus => "+",
                TokenType::Minus => "-",
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;

            let mut binary_expr =
                ASTNode::new(ASTNodeType::BinaryExpression, Some(operator.to_string()));
            binary_expr.add_child(expr);
            binary_expr.add_child(right);
            expr = binary_expr;
        }

        Ok(expr)
    }

    pub fn parse_factor(&mut self) -> Result<ASTNode, String> {
        let mut expr = self.parse_unary()?;

        while self.match_token(&[TokenType::Star, TokenType::Slash, TokenType::Percent]) {
            let operator = match self.previous().token_type {
                TokenType::Star => "*",
                TokenType::Slash => "/",
                TokenType::Percent => "%",
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;

            let mut binary_expr =
                ASTNode::new(ASTNodeType::BinaryExpression, Some(operator.to_string()));
            binary_expr.add_child(expr);
            binary_expr.add_child(right);
            expr = binary_expr;
        }

        Ok(expr)
    }
    pub fn parse_postfix(&mut self) -> Result<ASTNode, String> {
        // First parse the primary expression (variable, literal, etc.)
        let mut expr = self.parse_primary()?;

        // Check if it's followed by a postfix operator (++ or --)
        if self.match_token(&[TokenType::Increment, TokenType::Decrement]) {
            let operator = match self.previous().token_type {
                TokenType::Increment => "++",
                TokenType::Decrement => "--",
                _ => unreachable!(),
            };

            // Create a unary expression for the postfix operator
            let mut postfix_expr = ASTNode::new(
                ASTNodeType::UnaryExpression,
                Some(format!("post{}", operator)),
            );
            postfix_expr.add_child(expr);
            expr = postfix_expr;
        }

        Ok(expr)
    }

    pub fn parse_unary(&mut self) -> Result<ASTNode, String> {
        if self.match_token(&[TokenType::Bang, TokenType::Minus]) {
            let operator = match self.previous().token_type {
                TokenType::Bang => "!",
                TokenType::Minus => "-",
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;

            let mut unary_expr =
                ASTNode::new(ASTNodeType::UnaryExpression, Some(operator.to_string()));
            unary_expr.add_child(right);
            return Ok(unary_expr);
        }

        self.parse_postfix()
    }
    // We also need to enhance parse_primary to handle function calls
    pub fn parse_primary(&mut self) -> Result<ASTNode, String> {
        if self.check(TokenType::Number(0.0)) {
            // Clone the token_type before advancing
            let token_type = self.peek().token_type.clone();
            self.advance();

            if let TokenType::Number(value) = token_type {
                return Ok(ASTNode::new(ASTNodeType::Literal, Some(value.to_string())));
            }
        }
        if self.check(TokenType::String(String::new())) {
            // Clone the token_type before advancing
            let token_type = self.peek().token_type.clone();
            self.advance();

            if let TokenType::String(value) = token_type {
                return Ok(ASTNode::new(
                    ASTNodeType::Literal,
                    Some(format!("\"{}\"", value)),
                ));
            }
        }
        // Add this block for character literals
        if self.check(TokenType::Char(' ')) {
            // The actual char value doesn't matter for the check
            // Clone the token_type before advancing
            let token_type = self.peek().token_type.clone();
            self.advance();

            if let TokenType::Char(value) = token_type {
                return Ok(ASTNode::new(
                    ASTNodeType::Literal,
                    Some(format!("'{}'", value)),
                ));
            }
        }
        if self.check(TokenType::Identifier(String::new())) {
            // Clone the token_type before advancing
            let token_type = self.peek().token_type.clone();
            self.advance();

            if let TokenType::Identifier(name) = token_type {
                // Check for function call
                if self.check(TokenType::LeftParen) {
                    return self.parse_function_call(name);
                }

                // Otherwise, it's a variable reference
                return Ok(ASTNode::new(ASTNodeType::Variable, Some(name)));
            }
        }
        if self.check(TokenType::LeftParen) {
            self.advance(); // Consume left paren
            let expr = self.parse_expression()?;
            self.consume(TokenType::RightParen, "Expected ')' after expression.")?;

            let mut group = ASTNode::new(ASTNodeType::GroupingExpression, None);
            group.add_child(expr);
            return Ok(group);
        }

        Err(format!(
            "Expected expression, found {:?}.",
            self.peek().token_type
        ))
    }
}
