use crate::parser::types::{DataType, Keyword, TokenType};
use std::collections::HashMap;

// Struct Token
pub struct Token {
    pub token_type: TokenType, // Type of token
    pub line: usize,           // Line number for better error reporting
    pub column: usize,         // Column number for better error reporting
}

// Tokenize the source code
pub fn tokenize(source: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut current = 0;
    let mut line = 1;
    let mut column = 1;

    // Initialize keyword HashMap
    let mut keywords = HashMap::new();
    keywords.insert("print", Keyword::Print);
    keywords.insert("if", Keyword::If);
    keywords.insert("else", Keyword::Else);
    keywords.insert("while", Keyword::While);
    keywords.insert("for", Keyword::For);
    keywords.insert("return", Keyword::Return);
    keywords.insert("break", Keyword::Break);
    keywords.insert("continue", Keyword::Continue);
    keywords.insert("switch", Keyword::Switch);
    keywords.insert("case", Keyword::Case);
    keywords.insert("default", Keyword::Default);
    keywords.insert("do", Keyword::Do);
    keywords.insert("goto", Keyword::Goto);
    keywords.insert("sizeof", Keyword::Sizeof);
    keywords.insert("struct", Keyword::Struct);
    keywords.insert("union", Keyword::Union);
    keywords.insert("enum", Keyword::Enum);
    keywords.insert("typedef", Keyword::TypeDef);
    keywords.insert("volatile", Keyword::Volatile);
    keywords.insert("const", Keyword::Const);
    keywords.insert("register", Keyword::Register);
    keywords.insert("auto", Keyword::Auto);
    keywords.insert("static", Keyword::Static);
    keywords.insert("extern", Keyword::Extern);

    // Initialize data type HashMap
    let mut data_types = HashMap::new();
    data_types.insert("void", DataType::Void);
    data_types.insert("int", DataType::Int);
    data_types.insert("char", DataType::Char);
    data_types.insert("short", DataType::Short);
    data_types.insert("long", DataType::Long);
    data_types.insert("float", DataType::Float);
    data_types.insert("double", DataType::Double);
    data_types.insert("bool", DataType::Bool);
    data_types.insert("unsigned", DataType::Unsigned);
    data_types.insert("signed", DataType::Signed);
    data_types.insert("typedef", DataType::Typedef);
    data_types.insert("const", DataType::Const);
    data_types.insert("volatile", DataType::Volatile);
    data_types.insert("register", DataType::Register);
    data_types.insert("auto", DataType::Auto);
    data_types.insert("static", DataType::Static);
    data_types.insert("extern", DataType::Extern);
    data_types.insert("struct", DataType::Struct);
    data_types.insert("union", DataType::Union);
    data_types.insert("enum", DataType::Enum);

    let chars: Vec<char> = source.chars().collect();
    while current < chars.len() {
        let c = chars[current];
        let start_column = column;

        match c {
            '#' => {
                // Handle preprocessor directives
                let mut directive = String::new();
                while current < chars.len() && chars[current] != '\n' {
                    directive.push(chars[current]);
                    current += 1;
                    column += 1;
                }
                tokens.push(Token {
                    token_type: TokenType::PreprocessorDirective(directive),
                    line,
                    column: start_column,
                });
            }
            '(' => {
                tokens.push(Token {
                    token_type: TokenType::LeftParen,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            ')' => {
                tokens.push(Token {
                    token_type: TokenType::RightParen,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            '{' => {
                tokens.push(Token {
                    token_type: TokenType::LeftBrace,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            '}' => {
                tokens.push(Token {
                    token_type: TokenType::RightBrace,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            '[' => {
                tokens.push(Token {
                    token_type: TokenType::LeftBracket,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            ']' => {
                tokens.push(Token {
                    token_type: TokenType::RightBracket,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            '+' => {
                if current + 1 < chars.len() {
                    match chars[current + 1] {
                        '+' => {
                            tokens.push(Token {
                                token_type: TokenType::Increment,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        '=' => {
                            tokens.push(Token {
                                token_type: TokenType::PlusEqual,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::Plus,
                                line,
                                column: start_column,
                            });
                            current += 1;
                            column += 1;
                        }
                    }
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Plus,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '-' => {
                if current + 1 < chars.len() {
                    match chars[current + 1] {
                        '-' => {
                            tokens.push(Token {
                                token_type: TokenType::Decrement,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        '=' => {
                            tokens.push(Token {
                                token_type: TokenType::MinusEqual,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        '>' => {
                            tokens.push(Token {
                                token_type: TokenType::Arrow,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::Minus,
                                line,
                                column: start_column,
                            });
                            current += 1;
                            column += 1;
                        }
                    }
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Minus,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '*' => {
                if current + 1 < chars.len() && chars[current + 1] == '=' {
                    tokens.push(Token {
                        token_type: TokenType::StarEqual,
                        line,
                        column: start_column,
                    });
                    current += 2;
                    column += 2;
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Star,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '/' => {
                if current + 1 < chars.len() {
                    match chars[current + 1] {
                        '/' => {
                            // Comment handling logic - ignore until newline
                            let mut comment = String::new();
                            while current < chars.len() && chars[current] != '\n' {
                                comment.push(chars[current]);
                                current += 1;
                                column += 1;
                            }
                            tokens.push(Token {
                                token_type: TokenType::Comment(comment),
                                line,
                                column: start_column,
                            });
                        }
                        '=' => {
                            tokens.push(Token {
                                token_type: TokenType::SlashEqual,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::Slash,
                                line,
                                column: start_column,
                            });
                            current += 1;
                            column += 1;
                        }
                    }
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Slash,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '%' => {
                if current + 1 < chars.len() && chars[current + 1] == '=' {
                    tokens.push(Token {
                        token_type: TokenType::PercentEqual,
                        line,
                        column: start_column,
                    });
                    current += 2;
                    column += 2;
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Percent,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '=' => {
                if current + 1 < chars.len() && chars[current + 1] == '=' {
                    tokens.push(Token {
                        token_type: TokenType::EqualEqual,
                        line,
                        column: start_column,
                    });
                    current += 2;
                    column += 2;
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Equal,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '!' => {
                if current + 1 < chars.len() && chars[current + 1] == '=' {
                    tokens.push(Token {
                        token_type: TokenType::BangEqual,
                        line,
                        column: start_column,
                    });
                    current += 2;
                    column += 2;
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Bang,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '<' => {
                if current + 1 < chars.len() {
                    match chars[current + 1] {
                        '=' => {
                            tokens.push(Token {
                                token_type: TokenType::LessEqual,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        '<' => {
                            if current + 2 < chars.len() && chars[current + 2] == '=' {
                                tokens.push(Token {
                                    token_type: TokenType::LeftShiftEqual,
                                    line,
                                    column: start_column,
                                });
                                current += 3;
                                column += 3;
                            } else {
                                tokens.push(Token {
                                    token_type: TokenType::LeftShift,
                                    line,
                                    column: start_column,
                                });
                                current += 2;
                                column += 2;
                            }
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::Less,
                                line,
                                column: start_column,
                            });
                            current += 1;
                            column += 1;
                        }
                    }
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Less,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '>' => {
                if current + 1 < chars.len() {
                    match chars[current + 1] {
                        '=' => {
                            tokens.push(Token {
                                token_type: TokenType::GreaterEqual,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        '>' => {
                            if current + 2 < chars.len() && chars[current + 2] == '=' {
                                tokens.push(Token {
                                    token_type: TokenType::RightShiftEqual,
                                    line,
                                    column: start_column,
                                });
                                current += 3;
                                column += 3;
                            } else {
                                tokens.push(Token {
                                    token_type: TokenType::RightShift,
                                    line,
                                    column: start_column,
                                });
                                current += 2;
                                column += 2;
                            }
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::Greater,
                                line,
                                column: start_column,
                            });
                            current += 1;
                            column += 1;
                        }
                    }
                } else {
                    tokens.push(Token {
                        token_type: TokenType::Greater,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '&' => {
                if current + 1 < chars.len() {
                    match chars[current + 1] {
                        '&' => {
                            tokens.push(Token {
                                token_type: TokenType::And,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        '=' => {
                            tokens.push(Token {
                                token_type: TokenType::AndEqual,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::BitwiseAnd,
                                line,
                                column: start_column,
                            });
                            current += 1;
                            column += 1;
                        }
                    }
                } else {
                    tokens.push(Token {
                        token_type: TokenType::BitwiseAnd,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '|' => {
                if current + 1 < chars.len() {
                    match chars[current + 1] {
                        '|' => {
                            tokens.push(Token {
                                token_type: TokenType::Or,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        '=' => {
                            tokens.push(Token {
                                token_type: TokenType::OrEqual,
                                line,
                                column: start_column,
                            });
                            current += 2;
                            column += 2;
                        }
                        _ => {
                            tokens.push(Token {
                                token_type: TokenType::BitwiseOr,
                                line,
                                column: start_column,
                            });
                            current += 1;
                            column += 1;
                        }
                    }
                } else {
                    tokens.push(Token {
                        token_type: TokenType::BitwiseOr,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '^' => {
                if current + 1 < chars.len() && chars[current + 1] == '=' {
                    tokens.push(Token {
                        token_type: TokenType::XorEqual,
                        line,
                        column: start_column,
                    });
                    current += 2;
                    column += 2;
                } else {
                    tokens.push(Token {
                        token_type: TokenType::BitwiseXor,
                        line,
                        column: start_column,
                    });
                    current += 1;
                    column += 1;
                }
            }
            '~' => {
                tokens.push(Token {
                    token_type: TokenType::BitwiseNot,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            ';' => {
                tokens.push(Token {
                    token_type: TokenType::Semicolon,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            ',' => {
                tokens.push(Token {
                    token_type: TokenType::Comma,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            ':' => {
                tokens.push(Token {
                    token_type: TokenType::Colon,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            '.' => {
                tokens.push(Token {
                    token_type: TokenType::Dot,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            '?' => {
                tokens.push(Token {
                    token_type: TokenType::QuestionMark,
                    line,
                    column: start_column,
                });
                current += 1;
                column += 1;
            }
            '"' => {
                let mut string = String::new();
                current += 1; // Skip the opening quote
                column += 1;

                while current < chars.len() && chars[current] != '"' {
                    if chars[current] == '\\' && current + 1 < chars.len() {
                        // Handle escape sequences
                        current += 1;
                        column += 1;

                        match chars[current] {
                            'n' => string.push('\n'),
                            't' => string.push('\t'),
                            'r' => string.push('\r'),
                            '\\' => string.push('\\'),
                            '"' => string.push('"'),
                            '\'' => string.push('\''),
                            '0' => string.push('\0'),
                            _ => string.push(chars[current]), // Unrecognized escape
                        }
                    } else {
                        string.push(chars[current]);
                    }
                    current += 1;
                    column += 1;
                }

                if current < chars.len() {
                    current += 1; // Skip the closing quote
                    column += 1;
                } else {
                    return Err(format!(
                        "Unterminated string at line {}, column {}",
                        line, start_column
                    ));
                }

                tokens.push(Token {
                    token_type: TokenType::String(string),
                    line,
                    column: start_column,
                });
            }
            '\'' => {
                current += 1; // Skip the opening quote
                column += 1;

                let char_value;

                if current < chars.len() && chars[current] == '\\' {
                    // Handle escape sequence
                    current += 1;
                    column += 1;

                    if current < chars.len() {
                        char_value = match chars[current] {
                            'n' => '\n',
                            't' => '\t',
                            'r' => '\r',
                            '\\' => '\\',
                            '\'' => '\'',
                            '"' => '"',
                            '0' => '\0',
                            _ => chars[current], // Unrecognized escape
                        };
                        current += 1;
                        column += 1;
                    } else {
                        return Err(format!(
                            "Unterminated character literal at line {}, column {}",
                            line, start_column
                        ));
                    }
                } else if current < chars.len() {
                    char_value = chars[current];
                    current += 1;
                    column += 1;
                } else {
                    return Err(format!(
                        "Unterminated character literal at line {}, column {}",
                        line, start_column
                    ));
                }

                // Check for closing quote
                if current < chars.len() && chars[current] == '\'' {
                    current += 1;
                    column += 1;
                } else {
                    return Err(format!(
                        "Unterminated character literal at line {}, column {}",
                        line, start_column
                    ));
                }

                tokens.push(Token {
                    token_type: TokenType::Char(char_value),
                    line,
                    column: start_column,
                });
            }
            '0'..='9' => {
                let mut number = String::new();
                while current < chars.len() && chars[current].is_digit(10) {
                    number.push(chars[current]);
                    current += 1;
                    column += 1;
                }
                if current < chars.len() && chars[current] == '.' {
                    number.push('.');
                    current += 1;
                    column += 1;
                    while current < chars.len() && chars[current].is_digit(10) {
                        number.push(chars[current]);
                        current += 1;
                        column += 1;
                    }
                }
                let num_value: f64 = number.parse().map_err(|_| {
                    format!("Invalid number at line {}, column {}", line, start_column)
                })?;
                tokens.push(Token {
                    token_type: TokenType::Number(num_value),
                    line,
                    column: start_column,
                });
            }
            ' ' | '\r' | '\t' => {
                // Skip whitespace
                current += 1;
                column += 1;
            }
            '\n' => {
                line += 1;
                current += 1;
                column = 1; // Reset column count on new line
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut identifier = String::new();
                while current < chars.len()
                    && (chars[current].is_alphanumeric() || chars[current] == '_')
                {
                    identifier.push(chars[current]);
                    current += 1;
                    column += 1;
                }

                // Check if it's a keyword
                let token_type = if let Some(keyword) = keywords.get(identifier.as_str()) {
                    TokenType::Keyword(*keyword)
                }
                // Check if it's a data type
                else if let Some(data_type) = data_types.get(identifier.as_str()) {
                    TokenType::Type(*data_type)
                }
                // Otherwise, it's an identifier
                else {
                    TokenType::Identifier(identifier)
                };

                tokens.push(Token {
                    token_type,
                    line,
                    column: start_column,
                });
            }
            _ => {
                return Err(format!(
                    "Unexpected character '{}' at line {}, column {}",
                    c, line, column
                ));
            }
        }
    }

    // Add EOF token
    tokens.push(Token {
        token_type: TokenType::EOF,
        line,
        column,
    });

    Ok(tokens)
}
