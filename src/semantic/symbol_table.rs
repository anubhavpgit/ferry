// src/semantic/symbol_table.rs
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Int,
    Char,
    Float,
    Double,
    Bool,
    Pointer(Box<Type>),
    Function {
        return_type: Box<Type>,
        params: Vec<Type>,
    },
    Struct(String),
    Array {
        element_type: Box<Type>,
        size: Option<usize>,
    },
    Variadic(Box<Type>), // For variadic function return types
}

#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: Type,
    pub is_initialized: bool,
}

pub struct Scope {
    symbols: HashMap<String, Symbol>,
}

pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let global_scope = Scope {
            symbols: HashMap::new(),
        };
        SymbolTable {
            scopes: vec![global_scope],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope {
            symbols: HashMap::new(),
        });
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub fn define(
        &mut self,
        name: String,
        symbol_type: Type,
        initialized: bool,
    ) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().unwrap();

        if current_scope.symbols.contains_key(&name) {
            return Err(format!("Symbol '{}' already defined in this scope", name));
        }

        current_scope.symbols.insert(
            name.clone(),
            Symbol {
                name,
                symbol_type,
                is_initialized: initialized,
            },
        );

        Ok(())
    }
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // Search through all scopes from innermost to outermost
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn print_symbols(&self) {
        for (i, scope) in self.scopes.iter().enumerate() {
            println!("Scope {}:", i);
            for (name, symbol) in &scope.symbols {
                println!("  Symbol: {}, Type: {:?}", name, symbol.symbol_type);
            }
        }
    }
}
