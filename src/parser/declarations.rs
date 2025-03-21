use crate::parser::ast::ASTNode;
use crate::parser::parser_impl::Parser;
use crate::parser::types::{ASTNodeType, DataType, Keyword, TokenType};

impl<'a> Parser<'a> {
    pub fn parse_declaration(&mut self) -> Result<ASTNode, String> {
        let type_token = self.peek();

        // Check if it's a type token before advancing
        let data_type = match &type_token.token_type {
            TokenType::Type(dt) => {
                // Clone the data type since we can't move out of a reference
                *dt
            }
            _ => return Err(format!("Expected type specifier.")),
        };

        // Now we can advance since we have all data from the current token
        self.advance();

        let id_token = self.peek();

        // Extract the name from the token
        let name = match &id_token.token_type {
            TokenType::Identifier(n) => n.clone(), // Clone the string
            _ => return Err(format!("Expected identifier after type specifier.")),
        };

        // Now we can advance again
        self.advance();

        // Check for function or variable declaration
        if self.check(TokenType::LeftParen) {
            self.parse_function_declaration(data_type, name)
        } else {
            self.parse_variable_declaration_with_type(data_type, name)
        }
    }

    pub fn parse_function_declaration(
        &mut self,
        return_type: DataType,
        name: String,
    ) -> Result<ASTNode, String> {
        // Create the function declaration node
        let mut func_node = ASTNode::new(ASTNodeType::FunctionDeclaration, Some(name));

        // Add the return type as a child
        let type_node = ASTNode::new(ASTNodeType::Type, Some(format!("{:?}", return_type)));
        func_node.add_child(type_node);

        // Parse parameter list
        self.consume(TokenType::LeftParen, "Expected '(' after function name.")?;

        // For now, just handle empty parameter lists
        // TODO: Add parameter parsing here
        if !self.check(TokenType::RightParen) {
            // This is a simplified parameter parsing
            while !self.check(TokenType::RightParen) {
                if self.check(TokenType::Type(DataType::Int)) {
                    // Parse parameter with type
                    self.advance(); // Consume type

                    // Get parameter name
                    if let TokenType::Identifier(param_name) = &self.peek().token_type {
                        let param_name_clone = param_name.clone();
                        self.advance(); // Consume identifier

                        // Add parameter to function declaration
                        let mut param_node =
                            ASTNode::new(ASTNodeType::VariableDeclaration, Some(param_name_clone));
                        let param_type = ASTNode::new(ASTNodeType::Type, Some("Int".to_string()));
                        param_node.add_child(param_type);
                        func_node.add_child(param_node);

                        // Check for comma
                        if self.check(TokenType::Comma) {
                            self.advance(); // Consume comma
                        } else {
                            break;
                        }
                    } else {
                        return Err("Expected parameter name after type.".to_string());
                    }
                } else {
                    self.advance(); // Skip unknown tokens
                }
            }
        }

        self.consume(
            TokenType::RightParen,
            "Expected ')' after function parameters.",
        )?;

        // Parse function body
        if self.check(TokenType::LeftBrace) {
            let body = self.parse_block()?;
            func_node.add_child(body);
        } else {
            // Just a function prototype
            self.consume(
                TokenType::Semicolon,
                "Expected ';' after function prototype.",
            )?;
        }

        Ok(func_node)
    }

    pub fn parse_variable_declaration_with_type(
        &mut self,
        data_type: DataType,
        identifier: String,
    ) -> Result<ASTNode, String> {
        // Create variable declaration node
        let mut var_node = ASTNode::new(ASTNodeType::VariableDeclaration, Some(identifier));

        // Add type information as the first child
        let type_node = ASTNode::new(ASTNodeType::Type, Some(format!("{:?}", data_type)));
        var_node.add_child(type_node);

        // Check for initializer
        if self.check(TokenType::Equal) {
            self.advance(); // Consume '='

            // Parse the initializer expression
            let initializer = self.parse_expression()?;
            var_node.add_child(initializer);
        }

        // Verify declaration ends with a semicolon
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;

        Ok(var_node)
    }

    pub fn parse_variable_declaration(&mut self) -> Result<ASTNode, String> {
        // Get the data type
        let data_type = match &self.peek().token_type {
            TokenType::Type(data_type) => {
                let dt = *data_type;
                self.advance();
                dt
            }
            _ => return Err("Expected type specifier.".to_string()),
        };

        // Get the variable identifier
        let identifier = match &self.peek().token_type {
            TokenType::Identifier(name) => {
                let name_clone = name.clone();
                self.advance();
                name_clone
            }
            _ => return Err("Expected identifier after type specifier.".to_string()),
        };

        // Check for initializer
        let initializer = if self.match_token(&[TokenType::Equal]) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Expect semicolon
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;

        // Create the AST node for the variable declaration
        let mut var_node = ASTNode::new(ASTNodeType::VariableDeclaration, Some(identifier));

        // Create a type node
        let type_node = ASTNode::new(ASTNodeType::Type, Some(format!("{:?}", data_type)));
        var_node.add_child(type_node);

        // Add initializer if present
        if let Some(init) = initializer {
            var_node.add_child(init);
        }

        Ok(var_node)
    }
}
