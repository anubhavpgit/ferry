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
            // Process variable declaration right here
            let mut var_node = ASTNode::new(ASTNodeType::VariableDeclaration, Some(name));

            // Create a type node
            let type_node = ASTNode::new(ASTNodeType::Type, Some(format!("{:?}", data_type)));
            var_node.add_child(type_node);

            // Check for initializer
            if self.match_token(&[TokenType::Equal]) {
                let initializer = self.parse_expression()?;
                var_node.add_child(initializer);
            }

            // Expect semicolon
            self.consume(
                TokenType::Semicolon,
                "Expected ';' after variable declaration.",
            )?;

            Ok(var_node)
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

    pub fn parse_variable_declaration_with_type(&mut self) -> Result<ASTNode, String> {
        // Get the data type
        let data_type = match &self.peek().token_type {
            TokenType::Type(data_type) => {
                let dt = *data_type;
                self.advance();
                dt
            }
            _ => return Err("Expected type specifier.".to_string()),
        };

        // Get the first variable identifier
        let first_identifier = match &self.peek().token_type {
            TokenType::Identifier(name) => {
                let name_clone = name.clone();
                self.advance();
                name_clone
            }
            _ => return Err("Expected identifier after type specifier.".to_string()),
        };

        // Create a block node to contain multiple declarations
        let mut block_node = ASTNode::new(ASTNodeType::BlockStatement, None);

        // Process the first variable
        let mut first_var = ASTNode::new(ASTNodeType::VariableDeclaration, Some(first_identifier));
        let type_node = ASTNode::new(ASTNodeType::Type, Some(format!("{:?}", data_type)));
        first_var.add_child(type_node);

        // Add support for initializers - this is the key addition
        if self.match_token(&[TokenType::Equal]) {
            let initializer = self.parse_expression()?;
            first_var.add_child(initializer);
        }

        // Add the first variable to our block
        block_node.add_child(first_var);

        // Process any additional variables separated by commas
        while self.check(TokenType::Comma) {
            self.advance(); // Consume comma

            // Get the next variable name
            if let TokenType::Identifier(next_identifier) = &self.peek().token_type.clone() {
                let next_name = next_identifier.clone();
                self.advance(); // Consume identifier

                // Create a new variable declaration with the same type
                let mut next_var = ASTNode::new(ASTNodeType::VariableDeclaration, Some(next_name));
                let next_type = ASTNode::new(ASTNodeType::Type, Some(format!("{:?}", data_type)));
                next_var.add_child(next_type);

                // Also check for initializer here
                if self.match_token(&[TokenType::Equal]) {
                    let initializer = self.parse_expression()?;
                    next_var.add_child(initializer);
                }

                // Add it to our block
                block_node.add_child(next_var);
            } else {
                return Err("Expected identifier after comma".to_string());
            }
        }
        // Consume the semicolon
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;

        // If we only have one variable, return it directly
        if block_node.children.len() == 1 {
            return Ok(block_node.children.remove(0));
        }
        // Otherwise return the block with all declarations
        Ok(block_node)
    }
    pub fn parse_variable_declaration(&mut self) -> Result<ASTNode, String> {
        print!("Parsing variable declaration... ");
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
