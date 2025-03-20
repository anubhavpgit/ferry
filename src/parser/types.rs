#[derive(Debug)]
pub enum TokenType {
    // Literals
    Number(f64),
    String(String),
    Char(char),

    // Operators
    // Arithmetic
    Plus,
    Minus,
    Star,      // Multiplication
    Slash,     // Division
    Percent,   // Modulo
    Increment, // ++
    Decrement, // --

    // Comparison
    Equal,        // =
    EqualEqual,   // ==
    BangEqual,    // !=
    Less,         //
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=

    // Logical
    Bang, // !
    And,  // &&
    Or,   // ||

    // Bitwise
    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^
    BitwiseNot, // ~
    LeftShift,  //
    RightShift, // >>

    // Assignment
    PlusEqual,       // +=
    MinusEqual,      // -=
    StarEqual,       // *=
    SlashEqual,      // /=
    PercentEqual,    // %=
    AndEqual,        // &=
    OrEqual,         // |=
    XorEqual,        // ^=
    LeftShiftEqual,  // <<=
    RightShiftEqual, // >>=

    // Delimiters
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
    Semicolon,    // ;
    Comma,        // ,
    Colon,        // :
    Dot,          // .
    Arrow,        // ->
    QuestionMark, // ?

    // Preprocessor
    PreprocessorDirective(String),

    // Other
    Identifier(String),
    Comment(String),

    // Keywords
    Keyword(Keyword),

    // Types
    Type(DataType),

    // End of file marker
    EOF,
}

// Enum for operators - useful for expression parsing
#[derive(Debug, Clone, Copy)]
enum Operator {
    // Arithmetic
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Increment,
    Decrement,

    // Comparison
    Equal,
    EqualEqual,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Logical
    Bang,
    And,
    Or,

    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    LeftShift,
    RightShift,
}

// Enum for keywords
#[derive(Clone, Copy, Debug)]
pub enum Keyword {
    // Control flow
    If,
    Else,
    While,
    For,
    Return,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Do,
    Goto,

    // Function related
    Sizeof,

    // Structure-related
    Struct,
    Union,
    Enum,

    // Not needed since these are in DataType
    // But kept for backward compatibility
    Print, // Not a C keyword but useful for your language
    TypeDef,
    Volatile,
    Const,
    Register,
    Auto,
    Static,
    Extern,
}

// Enum for data types - separate from keywords for clarity
#[derive(Clone, Copy, Debug)]
pub enum DataType {
    // Basic types
    Void,
    Int,
    Char,
    Short,
    Long,
    Float,
    Double,
    LongDouble,
    UnsignedChar,
    UnsignedInt,
    UnsignedShort,
    UnsignedLong,
    Bool, // C99

    // Type modifiers - can also be used as standalone
    Typedef,
    Const,
    Volatile,
    Register,
    Auto,
    Static,
    Extern,
    Unsigned,
    Signed,

    // Complex types - often combined with identifiers
    Struct,
    Union,
    Enum,
}

pub enum ASTNodeType {
    Program,
    Function,
    VariableDeclaration,
    Expression,
    Statement,
    // Add more node types as needed
}
