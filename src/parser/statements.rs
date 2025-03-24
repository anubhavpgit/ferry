use crate::parser::ast::ASTNode;
use crate::parser::parser_impl::Parser;
use crate::parser::types::{ASTNodeType, Keyword, TokenType};

impl<'a> Parser<'a> {
    pub fn parse_statement(&mut self) -> Result<ASTNode, String> {
        while self.check(TokenType::Comment(String::new())) {
            self.advance(); // Consume the comment token
        }
        match &self.peek().token_type {
            // Control flow statements
            TokenType::Keyword(Keyword::If) => self.parse_if_statement(),
            TokenType::Keyword(Keyword::While) => self.parse_while_statement(),
            TokenType::Keyword(Keyword::For) => self.parse_for_statement(),
            TokenType::Keyword(Keyword::Do) => self.parse_do_while_statement(),
            TokenType::Keyword(Keyword::Switch) => self.parse_switch_statement(),

            // Jump statements
            TokenType::Keyword(Keyword::Break) => self.parse_break_statement(),
            TokenType::Keyword(Keyword::Continue) => self.parse_continue_statement(),
            TokenType::Keyword(Keyword::Return) => self.parse_return_statement(),
            TokenType::Keyword(Keyword::Goto) => self.parse_goto_statement(),

            // Block statement
            TokenType::LeftBrace => self.parse_block(),

            // Declarations
            TokenType::Type(_) => self.parse_variable_declaration(),

            // Preprocessor directives
            TokenType::PreprocessorDirective(_) => self.parse_preprocessor(),

            // Expression statements (assignments, function calls, etc.)
            _ => self.parse_expression_statement(),
        }
    }
    pub fn parse_if_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "if" keyword
        self.advance();

        // Expect and consume the opening parenthesis
        self.consume(TokenType::LeftParen, "Expected '(' after 'if'.")?;

        // Parse the condition expression
        let condition = self.parse_expression()?;

        // Expect and consume the closing parenthesis
        self.consume(TokenType::RightParen, "Expected ')' after if condition.")?;

        // Parse the if branch
        let if_branch = self.parse_statement()?;

        // Check for else branch
        let else_branch = if self.match_token(&[TokenType::Keyword(Keyword::Else)]) {
            Some(self.parse_statement()?)
        } else {
            None
        };

        // Create the AST node for the if statement
        let mut if_node = ASTNode::new(ASTNodeType::IfStatement, None);
        if_node.add_child(condition);
        if_node.add_child(if_branch);

        if let Some(else_branch) = else_branch {
            if_node.add_child(else_branch);
        }

        Ok(if_node)
    }

    pub fn parse_while_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "while" keyword
        self.advance();

        // Expect and consume the opening parenthesis
        self.consume(TokenType::LeftParen, "Expected '(' after 'while'.")?;

        // Parse the condition expression
        let condition = self.parse_expression()?;

        // Expect and consume the closing parenthesis
        self.consume(TokenType::RightParen, "Expected ')' after while condition.")?;

        // Parse the while body
        let body = self.parse_statement()?;

        // Create the AST node for the while statement
        let mut while_node = ASTNode::new(ASTNodeType::WhileStatement, None);
        while_node.add_child(condition);
        while_node.add_child(body);

        Ok(while_node)
    }
    pub fn parse_for_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "for" keyword
        self.advance();

        // Create for statement node
        let mut for_node = ASTNode::new(ASTNodeType::ForStatement, None);

        // Expect and consume the opening parenthesis
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'.")?;

        // Parse initialization
        if !self.check(TokenType::Semicolon) {
            if let TokenType::Type(_) = self.peek().token_type {
                // If it starts with a type, it's a variable declaration
                let init = self.parse_variable_declaration()?;
                for_node.add_child(init);
            } else {
                // Otherwise it's an expression
                let expr = self.parse_expression()?;
                let mut expr_stmt = ASTNode::new(ASTNodeType::ExpressionStatement, None);
                expr_stmt.add_child(expr);
                for_node.add_child(expr_stmt);
                self.consume(
                    TokenType::Semicolon,
                    "Expected ';' after for loop initialization.",
                )?;
            }
        } else {
            // Empty initialization
            self.advance(); // Consume semicolon
        }

        // Parse condition
        if !self.check(TokenType::Semicolon) {
            let condition = self.parse_expression()?;
            for_node.add_child(condition);
        } else {
            // Empty condition (equivalent to true)
            let true_node = ASTNode::new(ASTNodeType::Literal, Some("true".to_string()));
            for_node.add_child(true_node);
        }
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after for loop condition.",
        )?;

        // Parse increment
        if !self.check(TokenType::RightParen) {
            let increment = self.parse_expression()?;
            let mut expr_stmt = ASTNode::new(ASTNodeType::ExpressionStatement, None);
            expr_stmt.add_child(increment);
            for_node.add_child(expr_stmt);
        }

        // Consume the closing parenthesis
        self.consume(TokenType::RightParen, "Expected ')' after for clauses.")?;

        // Parse the body
        let body = self.parse_statement()?;
        for_node.add_child(body);

        Ok(for_node)
    }

    pub fn parse_do_while_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "do" keyword
        self.advance();

        // Create do-while statement node
        let mut do_while_node = ASTNode::new(ASTNodeType::DoWhileStatement, None);

        // Parse the body
        let body = self.parse_statement()?;
        do_while_node.add_child(body);

        // Expect and consume the "while" keyword
        self.consume(
            TokenType::Keyword(Keyword::While),
            "Expected 'while' after 'do' body.",
        )?;

        // Expect and consume the opening parenthesis
        self.consume(TokenType::LeftParen, "Expected '(' after 'while'.")?;

        // Parse the condition
        let condition = self.parse_expression()?;
        do_while_node.add_child(condition);

        // Expect and consume the closing parenthesis
        self.consume(
            TokenType::RightParen,
            "Expected ')' after do-while condition.",
        )?;

        // Expect and consume the semicolon
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after do-while statement.",
        )?;

        Ok(do_while_node)
    }

    pub fn parse_switch_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "switch" keyword
        self.advance();

        // Create switch statement node
        let mut switch_node = ASTNode::new(ASTNodeType::SwitchStatement, None);

        // Expect and consume the opening parenthesis
        self.consume(TokenType::LeftParen, "Expected '(' after 'switch'.")?;

        // Parse the switch expression
        let expression = self.parse_expression()?;
        switch_node.add_child(expression);

        // Expect and consume the closing parenthesis
        self.consume(
            TokenType::RightParen,
            "Expected ')' after switch expression.",
        )?;

        // Expect and consume the opening brace
        self.consume(TokenType::LeftBrace, "Expected '{' to begin switch body.")?;

        // Parse case statements
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            if self.check(TokenType::Keyword(Keyword::Case)) {
                // Parse case clause
                self.advance(); // Consume "case"

                // Parse case value
                let case_value = self.parse_expression()?;
                let mut case_node = ASTNode::new(ASTNodeType::CaseStatement, None);
                case_node.add_child(case_value);

                // Expect and consume colon
                self.consume(TokenType::Colon, "Expected ':' after case value.")?;

                // Parse case body (multiple statements until break, next case, or end of switch)
                while !self.check(TokenType::Keyword(Keyword::Case))
                    && !self.check(TokenType::Keyword(Keyword::Default))
                    && !self.check(TokenType::RightBrace)
                    && !self.is_at_end()
                {
                    // Check for break statement
                    if self.check(TokenType::Keyword(Keyword::Break)) {
                        self.advance(); // Consume "break"

                        // Expect and consume semicolon
                        self.consume(TokenType::Semicolon, "Expected ';' after 'break'.")?;

                        // Add break node to case
                        let break_node = ASTNode::new(ASTNodeType::BreakStatement, None);
                        case_node.add_child(break_node);

                        break; // Exit the inner loop
                    }

                    // Parse a statement
                    let stmt = self.parse_statement()?;
                    case_node.add_child(stmt);
                }

                // Add completed case to switch
                switch_node.add_child(case_node);
            } else if self.check(TokenType::Keyword(Keyword::Default)) {
                // Parse default clause
                self.advance(); // Consume "default"

                // Create default node
                let mut default_node = ASTNode::new(ASTNodeType::DefaultStatement, None);

                // Expect and consume colon
                self.consume(TokenType::Colon, "Expected ':' after 'default'.")?;

                // Parse default body (multiple statements until end of switch)
                while !self.check(TokenType::RightBrace) && !self.is_at_end() {
                    // Check for break statement
                    if self.check(TokenType::Keyword(Keyword::Break)) {
                        self.advance(); // Consume "break"

                        // Expect and consume semicolon
                        self.consume(TokenType::Semicolon, "Expected ';' after 'break'.")?;

                        // Add break node to default
                        let break_node = ASTNode::new(ASTNodeType::BreakStatement, None);
                        default_node.add_child(break_node);

                        break; // Exit the inner loop
                    }

                    // Parse a statement
                    let stmt = self.parse_statement()?;
                    default_node.add_child(stmt);
                }

                // Add completed default to switch
                switch_node.add_child(default_node);
            } else {
                return Err("Expected 'case' or 'default' in switch statement.".to_string());
            }
        }

        // Expect and consume the closing brace
        self.consume(TokenType::RightBrace, "Expected '}' to end switch body.")?;

        Ok(switch_node)
    }

    pub fn parse_break_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "break" keyword
        self.advance();

        // Expect and consume the semicolon
        self.consume(TokenType::Semicolon, "Expected ';' after 'break'.")?;

        Ok(ASTNode::new(ASTNodeType::BreakStatement, None))
    }

    pub fn parse_continue_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "continue" keyword
        self.advance();

        // Expect and consume the semicolon
        self.consume(TokenType::Semicolon, "Expected ';' after 'continue'.")?;

        Ok(ASTNode::new(ASTNodeType::ContinueStatement, None))
    }

    pub fn parse_goto_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "goto" keyword
        self.advance();

        // Get the label identifier
        let label = match &self.peek().token_type {
            TokenType::Identifier(name) => {
                let name_clone = name.clone();
                self.advance();
                name_clone
            }
            _ => return Err("Expected identifier after 'goto'.".to_string()),
        };

        // Expect and consume the semicolon
        self.consume(TokenType::Semicolon, "Expected ';' after goto label.")?;

        Ok(ASTNode::new(ASTNodeType::GotoStatement, Some(label)))
    }

    pub fn parse_return_statement(&mut self) -> Result<ASTNode, String> {
        // Consume the "return" keyword
        self.advance();

        // Check if there is a value to return
        let value = if !self.check(TokenType::Semicolon) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        // Expect semicolon
        self.consume(TokenType::Semicolon, "Expected ';' after return statement.")?;

        // Create the AST node for the return statement
        let mut return_node = ASTNode::new(ASTNodeType::ReturnStatement, None);

        // Add return value if present
        if let Some(val) = value {
            return_node.add_child(val);
        }

        Ok(return_node)
    }
    pub fn parse_block(&mut self) -> Result<ASTNode, String> {
        // Consume the opening brace
        self.consume(
            TokenType::LeftBrace,
            "Expected '{' at the start of a block.",
        )?;

        // Create block statement node
        let mut block_node = ASTNode::new(ASTNodeType::BlockStatement, None);

        // Parse statements until we reach the closing brace
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            // Skip any comments
            if let TokenType::Comment(_) = self.peek().token_type {
                self.advance(); // Simply consume and ignore comment tokens
                continue;
            }

            // Try to parse a statement
            // If we can't, we've likely hit a closing brace, which is fine
            if let Ok(statement) = self.parse_statement() {
                block_node.add_child(statement);
            } else {
                // If we couldn't parse a statement and haven't hit RightBrace,
                // something's wrong - but we should still try to recover
                if !self.check(TokenType::RightBrace) {
                    self.advance(); // Skip the problematic token and continue
                }
            }
        }

        // Consume the closing brace
        self.consume(TokenType::RightBrace, "Expected '}' at the end of a block.")?;

        Ok(block_node)
    }
    pub fn parse_expression_statement(&mut self) -> Result<ASTNode, String> {
        let expr = self.parse_expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;

        Ok(ASTNode {
            node_type: ASTNodeType::ExpressionStatement,
            children: vec![expr],
            value: None,
        })
    }
}
