// src/semantic/error.rs

#[derive(Debug)]
pub enum SemanticErrorType {
    UndefinedVariable(String),
    UndefinedFunction(String),
    TypeMismatch {
        expected: String,
        found: String,
    },
    RedefinedVariable(String),
    RedefinedFunction(String),
    InvalidOperator {
        operator: String,
        types: Vec<String>,
    },
    BreakOutsideLoop,
    ContinueOutsideLoop,
    ReturnTypeMismatch {
        expected: String,
        found: String,
    },
    Other(String),
}

#[derive(Debug)]
pub struct SemanticError {
    pub error_type: SemanticErrorType,
    pub line: Option<usize>,
    pub column: Option<usize>,
    pub message: String,
}

impl SemanticError {
    pub fn new(error_type: SemanticErrorType, line: Option<usize>, column: Option<usize>) -> Self {
        let message = match &error_type {
            SemanticErrorType::UndefinedVariable(name) => format!("Undefined variable: {}", name),
            SemanticErrorType::UndefinedFunction(name) => format!("Undefined function: {}", name),
            SemanticErrorType::TypeMismatch { expected, found } => {
                format!("Type mismatch: expected {}, found {}", expected, found)
            }
            SemanticErrorType::RedefinedVariable(name) => {
                format!("Variable redefinition: {} already defined", name)
            }
            SemanticErrorType::RedefinedFunction(name) => {
                format!("Function redefinition: {} already defined", name)
            }
            SemanticErrorType::InvalidOperator { operator, types } => format!(
                "Invalid operator {} for types: {}",
                operator,
                types.join(", ")
            ),
            SemanticErrorType::BreakOutsideLoop => "Break statement outside of loop".to_string(),
            SemanticErrorType::ContinueOutsideLoop => {
                "Continue statement outside of loop".to_string()
            }
            SemanticErrorType::ReturnTypeMismatch { expected, found } => format!(
                "Return type mismatch: expected {}, found {}",
                expected, found
            ),
            SemanticErrorType::Other(msg) => msg.clone(),
        };

        SemanticError {
            error_type,
            line,
            column,
            message,
        }
    }

    pub fn to_string(&self) -> String {
        match (self.line, self.column) {
            (Some(line), Some(column)) => format!("[{}:{}] {}", line, column, self.message),
            (Some(line), None) => format!("[line {}] {}", line, self.message),
            _ => self.message.clone(),
        }
    }
}
