use super::types::{IRNode, IRNodeType, IRType};
use crate::parser::ast::ASTNode;
use crate::parser::types::ASTNodeType;

pub struct IRGenerator {
    // Counter for generating unique variable names in the IR
    var_counter: usize,
    // Symbol table for tracking variables and their types
    symbol_table: std::collections::HashMap<String, IRType>,
}

impl IRGenerator {
    pub fn new() -> Self {
        IRGenerator {
            var_counter: 0,
            symbol_table: std::collections::HashMap::new(),
        }
    }

    // Generate a new temporary variable name
    fn fresh_var(&mut self) -> String {
        let var_name = format!("%t{}", self.var_counter);
        self.var_counter += 1;
        var_name
    }

    // Main entry point for IR generation
    pub fn generate(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        match ast.node_type {
            ASTNodeType::Program => self.generate_program(ast),
            ASTNodeType::FunctionDeclaration => self.generate_function(ast),
            ASTNodeType::VariableDeclaration => self.generate_variable_declaration(ast),
            ASTNodeType::BlockStatement => self.generate_block(ast),
            ASTNodeType::ExpressionStatement => self.generate_expression_statement(ast),
            ASTNodeType::BreakStatement => self.generate_break_statement(ast),
            ASTNodeType::IfStatement => self.generate_if_statement(ast),
            ASTNodeType::WhileStatement => self.generate_while_statement(ast),
            ASTNodeType::ForStatement => self.generate_for_statement(ast),
            ASTNodeType::ReturnStatement => self.generate_return_statement(ast),
            ASTNodeType::Expression => self.generate_expression(ast),
            ASTNodeType::BinaryExpression => self.generate_binary_expression(ast),
            ASTNodeType::UnaryExpression => self.generate_unary_expression(ast),
            ASTNodeType::CallExpression => self.generate_call_expression(ast),
            ASTNodeType::Variable => self.generate_variable(ast),
            ASTNodeType::Literal => self.generate_literal(ast),
            ASTNodeType::Assignment => self.generate_assignment(ast),
            _ => Err(format!("Unsupported AST node type: {:?}", ast.node_type)),
        }
    }

    // Generate IR for the top-level program
    fn generate_program(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let mut module = IRNode::new(IRNodeType::Module, None);

        for child in &ast.children {
            let ir_node = self.generate(child)?;
            module.add_child(ir_node);
        }

        Ok(module)
    }

    // Generate IR for a function declaration
    fn generate_function(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let func_name = ast.value.clone().unwrap_or_else(|| "unknown".to_string());
        let mut func_node = IRNode::new(IRNodeType::Function, Some(func_name));

        // First child is return type
        if !ast.children.is_empty() {
            let return_type = match ast.children[0].value.as_deref() {
                Some("Int") => IRType::Int32,
                Some("Float") => IRType::Float,
                Some("Void") => IRType::Void,
                _ => IRType::Int32, // Default to int
            };
            func_node = func_node.with_type(return_type);
        }

        // Process parameters (may be between type and body)
        for i in 1..ast.children.len() {
            if ast.children[i].node_type == ASTNodeType::VariableDeclaration {
                let param = self.generate_parameter(&ast.children[i])?;
                func_node.add_child(param);
            }
        }

        // Last child should be the function body
        if let Some(last) = ast.children.last() {
            if last.node_type == ASTNodeType::BlockStatement {
                let body = self.generate_block(last)?;
                func_node.add_child(body);
            }
        }

        Ok(func_node)
    }

    // Generate IR for function parameters
    fn generate_parameter(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let param_name = ast.value.clone().unwrap_or_else(|| "param".to_string());
        let mut param_node = IRNode::new(IRNodeType::Parameter, Some(param_name.clone()));

        // Get the parameter type
        if !ast.children.is_empty() && ast.children[0].node_type == ASTNodeType::Type {
            let param_type = match ast.children[0].value.as_deref() {
                Some("Int") => IRType::Int32,
                Some("Float") => IRType::Float,
                _ => IRType::Int32, // Default to int
            };
            param_node = param_node.with_type(param_type.clone());

            // Add to symbol table
            self.symbol_table.insert(param_name, param_type);
        }

        Ok(param_node)
    }

    // Generate IR for variable declarations
    fn generate_variable_declaration(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let var_name = ast.value.clone().unwrap_or_else(|| "unknown".to_string());

        // Determine variable type
        let var_type = if !ast.children.is_empty() && ast.children[0].node_type == ASTNodeType::Type
        {
            match ast.children[0].value.as_deref() {
                Some("Int") => IRType::Int32,
                Some("Float") => IRType::Float,
                _ => IRType::Int32, // Default to int
            }
        } else {
            IRType::Int32 // Default to int if type not specified
        };

        // Create alloca instruction
        let alloca_node =
            IRNode::new(IRNodeType::Alloca, Some(var_name.clone())).with_type(var_type.clone());

        // Register variable in symbol table
        self.symbol_table.insert(var_name.clone(), var_type);

        // Handle initialization if present
        if ast.children.len() > 1 {
            let init_expr = self.generate(&ast.children[1])?;

            // Create store instruction to initialize the variable
            let mut store_node = IRNode::new(IRNodeType::Store, None);

            // Add source (the initializer expression)
            store_node.add_child(init_expr);

            // Add destination (the variable reference)
            let var_ref = IRNode::new(IRNodeType::Variable, Some(var_name));
            store_node.add_child(var_ref);

            // Add both nodes to a block
            let mut block = IRNode::new(IRNodeType::BasicBlock, None);
            block.add_child(alloca_node);
            block.add_child(store_node);

            return Ok(block);
        }

        Ok(alloca_node)
    }

    // Generate IR for code blocks
    fn generate_block(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let mut block = IRNode::new(IRNodeType::BasicBlock, None);

        for child in &ast.children {
            let ir_node = self.generate(child)?;
            block.add_child(ir_node);
        }

        Ok(block)
    }

    // Generate IR for expression statements
    fn generate_expression_statement(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if let Some(expr) = ast.children.first() {
            self.generate(expr)
        } else {
            Err("Empty expression statement".to_string())
        }
    }

    // Generate IR for if statements
    fn generate_if_statement(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if ast.children.len() < 2 {
            return Err("If statement requires condition and body".to_string());
        }

        // Generate the condition
        let condition = self.generate(&ast.children[0])?;

        // Generate the then block
        let then_block = self.generate(&ast.children[1])?;

        // Generate the else block if it exists
        let else_block = if ast.children.len() > 2 {
            Some(self.generate(&ast.children[2])?)
        } else {
            None
        };

        // Create the branch instruction
        let mut branch = IRNode::new(IRNodeType::Branch, None);
        branch.add_child(condition.clone());
        branch.add_child(then_block);

        if let Some(else_block) = else_block {
            branch.add_child(else_block);
        }

        Ok(branch)
    }

    fn generate_break_statement(&mut self, _ast: &ASTNode) -> Result<IRNode, String> {
        // Create a jump instruction with a label that indicates this is a break statement
        // In a real compiler, this would jump to the exit point of the nearest enclosing loop
        let break_jump = IRNode::new(IRNodeType::Jump, Some("loop.exit".to_string()));

        Ok(break_jump)
    }

    // Generate IR for while loops
    fn generate_while_statement(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if ast.children.len() < 2 {
            return Err("While statement requires condition and body".to_string());
        }

        // Create a basic block for the loop header (condition evaluation)
        let mut header_block =
            IRNode::new(IRNodeType::BasicBlock, Some("while.header".to_string()));

        // Generate the condition
        let condition = self.generate(&ast.children[0])?;
        header_block.add_child(condition.clone());

        // Generate the body
        let body_block = self.generate(&ast.children[1])?;

        // Create the branch instruction
        let mut branch = IRNode::new(IRNodeType::Branch, None);
        branch.add_child(condition);
        branch.add_child(body_block);

        // Create a jump back to the header
        let jump_back = IRNode::new(IRNodeType::Jump, Some("while.header".to_string()));

        // Assemble the complete while loop structure
        let mut while_node = IRNode::new(IRNodeType::BasicBlock, Some("while".to_string()));
        while_node.add_child(header_block);
        while_node.add_child(branch);
        while_node.add_child(jump_back);

        Ok(while_node)
    }

    // Generate IR for for loops
    fn generate_for_statement(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if ast.children.len() < 4 {
            return Err(
                "For statement requires initialization, condition, increment, and body".to_string(),
            );
        }

        // Generate initialization
        let init = self.generate(&ast.children[0])?;

        // Generate condition
        let condition = self.generate(&ast.children[1])?;

        // Generate increment
        let increment = self.generate(&ast.children[2])?;

        // Generate body
        let body = self.generate(&ast.children[3])?;

        // Create the for loop structure
        let mut for_node = IRNode::new(IRNodeType::BasicBlock, Some("for".to_string()));

        // Add initialization
        for_node.add_child(init);

        // Create header block with condition
        let mut header_block = IRNode::new(IRNodeType::BasicBlock, Some("for.header".to_string()));
        header_block.add_child(condition.clone());
        for_node.add_child(header_block);

        // Create branch
        let mut branch = IRNode::new(IRNodeType::Branch, None);
        branch.add_child(condition.clone());

        // Create body block that includes the original body and increment
        let mut body_block = IRNode::new(IRNodeType::BasicBlock, Some("for.body".to_string()));
        body_block.add_child(body);
        body_block.add_child(increment);

        // Add jump back to header
        let jump_back = IRNode::new(IRNodeType::Jump, Some("for.header".to_string()));
        body_block.add_child(jump_back);

        // Connect branch to body
        branch.add_child(body_block);
        for_node.add_child(branch);

        Ok(for_node)
    }

    // Generate IR for return statements
    fn generate_return_statement(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let mut return_node = IRNode::new(IRNodeType::Return, None);

        // If there's a return value, generate it
        if let Some(expr) = ast.children.first() {
            let value = self.generate(expr)?;
            return_node.add_child(value);
        }

        Ok(return_node)
    }

    // Generate IR for expressions
    fn generate_expression(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if let Some(child) = ast.children.first() {
            self.generate(child)
        } else {
            Err("Empty expression".to_string())
        }
    }
    fn generate_unary_expression(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if ast.children.len() != 1 {
            return Err("Unary expression requires exactly one operand".to_string());
        }
        let operator = ast.value.clone().unwrap_or_else(|| "!".to_string());

        // Handle postfix operators
        if operator == "post++" || operator == "post--" {
            // Get the operand (should be a variable)
            let operand = &ast.children[0];
            if operand.node_type != ASTNodeType::Variable {
                return Err("Operand of postfix operator must be a variable".to_string());
            }

            let var_name = operand
                .value
                .clone()
                .unwrap_or_else(|| "unknown".to_string());

            // 1. Create a block to hold multiple instructions
            let mut block = IRNode::new(IRNodeType::BasicBlock, None);

            // 2. Load the variable's current value
            let var_ref = IRNode::new(IRNodeType::Variable, Some(var_name.clone()));
            let mut load = IRNode::new(IRNodeType::Load, None);
            load.add_child(var_ref.clone());
            block.add_child(load.clone());

            // 3. Create the increment/decrement operation
            let mut binary_op = IRNode::new(
                IRNodeType::BinaryOp,
                Some(if operator == "post++" { "+" } else { "-" }.to_string()),
            );
            binary_op.add_child(load);
            let one =
                IRNode::new(IRNodeType::Constant, Some("1".to_string())).with_type(IRType::Int32);
            binary_op.add_child(one);

            // 4. Store the new value back to the variable
            let mut store = IRNode::new(IRNodeType::Store, None);
            store.add_child(binary_op);
            store.add_child(var_ref);
            block.add_child(store);

            return Ok(block);
        }

        // Handle standard unary operators
        let mut unary_op = IRNode::new(IRNodeType::UnaryOp, Some(operator));

        // Generate the operand
        let operand = self.generate(&ast.children[0])?;
        unary_op.add_child(operand);

        Ok(unary_op)
    }

    // Generate IR for binary expressions (e.g., a + b)
    fn generate_binary_expression(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if ast.children.len() != 2 {
            return Err("Binary expression requires exactly two operands".to_string());
        }

        let operator = ast.value.clone().unwrap_or_else(|| "+".to_string());
        let mut binary_op = IRNode::new(IRNodeType::BinaryOp, Some(operator));

        // Generate left and right operands
        let left = self.generate(&ast.children[0])?;
        let right = self.generate(&ast.children[1])?;

        binary_op.add_child(left);
        binary_op.add_child(right);

        Ok(binary_op)
    }

    // Generate IR for function calls
    fn generate_call_expression(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let func_name = ast.value.clone().unwrap_or_else(|| "unknown".to_string());
        let mut call_node = IRNode::new(IRNodeType::Call, Some(func_name));

        // Skip the first child which is the function name variable
        for i in 1..ast.children.len() {
            let arg = self.generate(&ast.children[i])?;
            call_node.add_child(arg);
        }

        Ok(call_node)
    }

    // Generate IR for variables
    fn generate_variable(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let var_name = ast.value.clone().unwrap_or_else(|| "unknown".to_string());

        // Create a load instruction to get the variable's value
        let mut load_node = IRNode::new(IRNodeType::Load, None);

        // Add the variable reference as the source
        let var_ref = IRNode::new(IRNodeType::Variable, Some(var_name.clone()));
        load_node.add_child(var_ref);

        // Set the result type if the variable is in the symbol table
        if let Some(var_type) = self.symbol_table.get(&var_name) {
            load_node = load_node.with_type(var_type.clone());
        }

        Ok(load_node)
    }

    // Generate IR for literals (constants)
    fn generate_literal(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        let value = ast.value.clone().unwrap_or_else(|| "0".to_string());

        // Determine the type of literal
        let ty = if value.starts_with('"') && value.ends_with('"') {
            // It's a string
            IRType::Pointer(Box::new(IRType::Int32)) // char* is represented as pointer to int32
        } else if value.contains('.') {
            // It's a floating point number
            IRType::Float
        } else {
            // It's an integer
            IRType::Int32
        };

        Ok(IRNode::new(IRNodeType::Constant, Some(value)).with_type(ty))
    }

    // Generate IR for assignment expressions (e.g., a = b)
    fn generate_assignment(&mut self, ast: &ASTNode) -> Result<IRNode, String> {
        if ast.children.len() != 2 {
            return Err("Assignment requires left-hand side and right-hand side".to_string());
        }

        // The left-hand side should be a variable
        let lhs = &ast.children[0];
        if lhs.node_type != ASTNodeType::Variable {
            return Err("Left-hand side of assignment must be a variable".to_string());
        }

        let var_name = lhs.value.clone().unwrap_or_else(|| "unknown".to_string());

        // Generate the right-hand side expression
        let rhs = self.generate(&ast.children[1])?;

        // Create a store instruction
        let mut store_node = IRNode::new(IRNodeType::Store, None);

        // Add the right-hand side as the source
        store_node.add_child(rhs);

        // Add the variable reference as the destination
        let var_ref = IRNode::new(IRNodeType::Variable, Some(var_name));
        store_node.add_child(var_ref);

        Ok(store_node)
    }
}
