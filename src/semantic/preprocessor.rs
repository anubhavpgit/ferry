// src/semantic/preprocessor.rs
use crate::parser::ast::ASTNode;
use crate::parser::types::ASTNodeType;
use crate::semantic::symbol_table::{SymbolTable, Type};
use std::collections::HashMap;

/// Process all preprocessor directives in the AST and register
/// appropriate standard library functions in the symbol table.
pub fn process_preprocessor_directives(ast: &ASTNode, symbol_table: &mut SymbolTable) {
    // Create a mapping of header files to registration functions
    let mut header_map: HashMap<&str, fn(&mut SymbolTable)> = HashMap::new();

    // Register standard header files
    header_map.insert("stdio.h", register_stdio_functions);
    header_map.insert("stdlib.h", register_stdlib_functions);
    header_map.insert("string.h", register_string_functions);
    header_map.insert("math.h", register_math_functions);

    // Process all preprocessor directives in the AST
    process_directives_recursive(ast, symbol_table, &header_map);
}

/// Recursively process preprocessor directives in the AST
fn process_directives_recursive(
    node: &ASTNode,
    symbol_table: &mut SymbolTable,
    header_map: &HashMap<&str, fn(&mut SymbolTable)>,
) {
    // Check if this node is a preprocessor directive
    if let ASTNodeType::PreprocessorDirective = node.node_type {
        if let Some(directive) = &node.value {
            // Parse the #include directive
            if directive.starts_with("#include") {
                // Extract the header file name from <header.h>
                if let Some(start) = directive.find('<') {
                    if let Some(end) = directive.find('>') {
                        let header = &directive[start + 1..end];

                        // Check if we support this header
                        if let Some(register_fn) = header_map.get(header) {
                            // Register the functions from this header
                            register_fn(symbol_table);
                        }
                    }
                }
                // Extract the header file name from "header.h"
                else if let Some(start) = directive.find('"') {
                    if let Some(end) = directive.rfind('"') {
                        let header = &directive[start + 1..end];

                        // Check if we support this header
                        if let Some(register_fn) = header_map.get(header) {
                            // Register the functions from this header
                            register_fn(symbol_table);
                        }
                    }
                }
            }
            // Other preprocessor directives like #define, #ifdef, etc.
            // could be handled here in the future
        }
    }

    // Recursively process all children
    for child in &node.children {
        process_directives_recursive(child, symbol_table, header_map);
    }
}

/// Register stdio.h functions in the symbol table
fn register_stdio_functions(symbol_table: &mut SymbolTable) {
    // Define common types
    let int_type = Type::Int;
    let char_type = Type::Char;
    let void_type = Type::Void;
    let char_ptr_type = Type::Pointer(Box::new(char_type));
    let void_ptr_type = Type::Pointer(Box::new(void_type.clone()));
    let file_ptr_type = Type::Pointer(Box::new(void_type.clone())); // FILE* simplification

    // printf (simplified - real printf has variable arguments)
    let printf_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![char_ptr_type.clone()], // Format string
    };
    let _ = symbol_table.define("printf".to_string(), printf_type, true);

    // fprintf
    let fprintf_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![file_ptr_type.clone(), char_ptr_type.clone()], // FILE*, format string
    };
    let _ = symbol_table.define("fprintf".to_string(), fprintf_type, true);

    // scanf (simplified)
    let scanf_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![char_ptr_type.clone()], // Format string
    };
    let _ = symbol_table.define("scanf".to_string(), scanf_type, true);

    // fscanf
    let fscanf_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![file_ptr_type.clone(), char_ptr_type.clone()], // FILE*, format string
    };
    let _ = symbol_table.define("fscanf".to_string(), fscanf_type, true);

    // fopen
    let fopen_type = Type::Function {
        return_type: Box::new(file_ptr_type.clone()),
        params: vec![char_ptr_type.clone(), char_ptr_type.clone()], // filename, mode
    };
    let _ = symbol_table.define("fopen".to_string(), fopen_type, true);

    // fclose
    let fclose_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![file_ptr_type.clone()], // FILE*
    };
    let _ = symbol_table.define("fclose".to_string(), fclose_type, true);

    // fgetc
    let fgetc_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![file_ptr_type.clone()], // FILE*
    };
    let _ = symbol_table.define("fgetc".to_string(), fgetc_type, true);

    // fputc
    let fputc_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![int_type.clone(), file_ptr_type.clone()], // int, FILE*
    };
    let _ = symbol_table.define("fputc".to_string(), fputc_type, true);

    // fgets
    let fgets_type = Type::Function {
        return_type: Box::new(char_ptr_type.clone()),
        params: vec![
            char_ptr_type.clone(),
            int_type.clone(),
            file_ptr_type.clone(),
        ], // char*, int, FILE*
    };
    let _ = symbol_table.define("fgets".to_string(), fgets_type, true);

    // fputs
    let fputs_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![char_ptr_type.clone(), file_ptr_type.clone()], // const char*, FILE*
    };
    let _ = symbol_table.define("fputs".to_string(), fputs_type, true);
}

/// Register stdlib.h functions in the symbol table
fn register_stdlib_functions(symbol_table: &mut SymbolTable) {
    // Define common types
    let int_type = Type::Int;
    let char_type = Type::Char;
    let void_type = Type::Void;
    let char_ptr_type = Type::Pointer(Box::new(char_type));
    let void_ptr_type = Type::Pointer(Box::new(void_type.clone()));

    // malloc
    let malloc_type = Type::Function {
        return_type: Box::new(void_ptr_type.clone()),
        params: vec![int_type.clone()], // size
    };
    let _ = symbol_table.define("malloc".to_string(), malloc_type, true);

    // calloc
    let calloc_type = Type::Function {
        return_type: Box::new(void_ptr_type.clone()),
        params: vec![int_type.clone(), int_type.clone()], // count, size
    };
    let _ = symbol_table.define("calloc".to_string(), calloc_type, true);

    // realloc
    let realloc_type = Type::Function {
        return_type: Box::new(void_ptr_type.clone()),
        params: vec![void_ptr_type.clone(), int_type.clone()], // ptr, size
    };
    let _ = symbol_table.define("realloc".to_string(), realloc_type, true);

    // free
    let free_type = Type::Function {
        return_type: Box::new(void_type.clone()),
        params: vec![void_ptr_type.clone()], // ptr
    };
    let _ = symbol_table.define("free".to_string(), free_type, true);

    // exit
    let exit_type = Type::Function {
        return_type: Box::new(void_type.clone()),
        params: vec![int_type.clone()], // status
    };
    let _ = symbol_table.define("exit".to_string(), exit_type, true);

    // abort
    let abort_type = Type::Function {
        return_type: Box::new(void_type.clone()),
        params: vec![],
    };
    let _ = symbol_table.define("abort".to_string(), abort_type, true);

    // atoi
    let atoi_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![char_ptr_type.clone()], // str
    };
    let _ = symbol_table.define("atoi".to_string(), atoi_type, true);

    // rand
    let rand_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![],
    };
    let _ = symbol_table.define("rand".to_string(), rand_type, true);

    // srand
    let srand_type = Type::Function {
        return_type: Box::new(void_type.clone()),
        params: vec![int_type.clone()], // seed
    };
    let _ = symbol_table.define("srand".to_string(), srand_type, true);
}

/// Register string.h functions in the symbol table
fn register_string_functions(symbol_table: &mut SymbolTable) {
    // Define common types
    let int_type = Type::Int;
    let char_type = Type::Char;
    let void_type = Type::Void;
    let char_ptr_type = Type::Pointer(Box::new(char_type));
    let void_ptr_type = Type::Pointer(Box::new(void_type.clone()));

    // strlen
    let strlen_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![char_ptr_type.clone()], // str
    };
    let _ = symbol_table.define("strlen".to_string(), strlen_type, true);

    // strcpy
    let strcpy_type = Type::Function {
        return_type: Box::new(char_ptr_type.clone()),
        params: vec![char_ptr_type.clone(), char_ptr_type.clone()], // dest, src
    };
    let _ = symbol_table.define("strcpy".to_string(), strcpy_type, true);

    // strncpy
    let strncpy_type = Type::Function {
        return_type: Box::new(char_ptr_type.clone()),
        params: vec![
            char_ptr_type.clone(),
            char_ptr_type.clone(),
            int_type.clone(),
        ], // dest, src, n
    };
    let _ = symbol_table.define("strncpy".to_string(), strncpy_type, true);

    // strcat
    let strcat_type = Type::Function {
        return_type: Box::new(char_ptr_type.clone()),
        params: vec![char_ptr_type.clone(), char_ptr_type.clone()], // dest, src
    };
    let _ = symbol_table.define("strcat".to_string(), strcat_type, true);

    // strncat
    let strncat_type = Type::Function {
        return_type: Box::new(char_ptr_type.clone()),
        params: vec![
            char_ptr_type.clone(),
            char_ptr_type.clone(),
            int_type.clone(),
        ], // dest, src, n
    };
    let _ = symbol_table.define("strncat".to_string(), strncat_type, true);

    // strcmp
    let strcmp_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![char_ptr_type.clone(), char_ptr_type.clone()], // s1, s2
    };
    let _ = symbol_table.define("strcmp".to_string(), strcmp_type, true);

    // strncmp
    let strncmp_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![
            char_ptr_type.clone(),
            char_ptr_type.clone(),
            int_type.clone(),
        ], // s1, s2, n
    };
    let _ = symbol_table.define("strncmp".to_string(), strncmp_type, true);

    // strchr
    let strchr_type = Type::Function {
        return_type: Box::new(char_ptr_type.clone()),
        params: vec![char_ptr_type.clone(), int_type.clone()], // s, c
    };
    let _ = symbol_table.define("strchr".to_string(), strchr_type, true);

    // strstr
    let strstr_type = Type::Function {
        return_type: Box::new(char_ptr_type.clone()),
        params: vec![char_ptr_type.clone(), char_ptr_type.clone()], // haystack, needle
    };
    let _ = symbol_table.define("strstr".to_string(), strstr_type, true);

    // memcpy
    let memcpy_type = Type::Function {
        return_type: Box::new(void_ptr_type.clone()),
        params: vec![
            void_ptr_type.clone(),
            void_ptr_type.clone(),
            int_type.clone(),
        ], // dest, src, n
    };
    let _ = symbol_table.define("memcpy".to_string(), memcpy_type, true);

    // memset
    let memset_type = Type::Function {
        return_type: Box::new(void_ptr_type.clone()),
        params: vec![void_ptr_type.clone(), int_type.clone(), int_type.clone()], // s, c, n
    };
    let _ = symbol_table.define("memset".to_string(), memset_type, true);
}

/// Register math.h functions in the symbol table
fn register_math_functions(symbol_table: &mut SymbolTable) {
    // Define common types
    let int_type = Type::Int;
    let float_type = Type::Float;
    let double_type = Type::Double;

    // sqrt
    let sqrt_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("sqrt".to_string(), sqrt_type, true);

    // pow
    let pow_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone(), double_type.clone()], // base, exponent
    };
    let _ = symbol_table.define("pow".to_string(), pow_type, true);

    // sin
    let sin_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("sin".to_string(), sin_type, true);

    // cos
    let cos_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("cos".to_string(), cos_type, true);

    // tan
    let tan_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("tan".to_string(), tan_type, true);

    // exp
    let exp_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("exp".to_string(), exp_type, true);

    // log
    let log_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("log".to_string(), log_type, true);

    // log10
    let log10_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("log10".to_string(), log10_type, true);

    // floor
    let floor_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("floor".to_string(), floor_type, true);

    // ceil
    let ceil_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("ceil".to_string(), ceil_type, true);

    // fabs
    let fabs_type = Type::Function {
        return_type: Box::new(double_type.clone()),
        params: vec![double_type.clone()], // x
    };
    let _ = symbol_table.define("fabs".to_string(), fabs_type, true);

    // abs (integer version)
    let abs_type = Type::Function {
        return_type: Box::new(int_type.clone()),
        params: vec![int_type.clone()], // x
    };
    let _ = symbol_table.define("abs".to_string(), abs_type, true);
}
