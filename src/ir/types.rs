#[derive(Debug, Clone, PartialEq)]
pub enum IRNodeType {
    // Program structure
    Module,
    Function,
    BasicBlock,

    // Memory operations
    Alloca, // Stack allocation
    Load,   // Load from memory
    Store,  // Store to memory

    // Control flow
    Branch, // Conditional branch
    Jump,   // Unconditional jump
    Return, // Function return

    // Operations
    BinaryOp, // Binary operations like +, -, *, /
    UnaryOp,  // Unary operations like !, -
    Call,     // Function call

    // Values
    Constant,  // Literal values
    Variable,  // Variables
    Parameter, // Function parameters

    // Types
    Type, // Type information
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRNode {
    pub node_type: IRNodeType,
    pub value: Option<String>,
    pub children: Vec<IRNode>,
    pub ty: Option<IRType>, // Type information
}

// Type system for IR
#[derive(Debug, Clone, PartialEq)]
pub enum IRType {
    Void,
    Int32,
    Int64,
    Float,
    Double,
    Pointer(Box<IRType>),
    Array(Box<IRType>, usize), // Type and size
}

impl IRNode {
    pub fn new(node_type: IRNodeType, value: Option<String>) -> Self {
        IRNode {
            node_type,
            value,
            children: vec![],
            ty: None,
        }
    }

    pub fn with_type(mut self, ty: IRType) -> Self {
        self.ty = Some(ty);
        self
    }

    pub fn add_child(&mut self, child: IRNode) {
        self.children.push(child);
    }
}
