mod advanced_optimiser;
mod basic_optimiser;
mod generator;
pub mod types;

use self::generator::IRGenerator;
use crate::parser::ast;

pub fn generate_ir(ast: &ast::ASTNode) -> Result<types::IRNode, String> {
    let mut generator = IRGenerator::new();
    let mut ir_head = generator.generate(ast)?;

    // DEBUG
    println!("IR Structure:");
    println!("└── Root");
    for (i, node) in ir_head.children.iter().enumerate() {
        let is_last = i == ir_head.children.len() - 1;
        print_ir_node(node, "", is_last);
    }
    println!("\nIR generated successfully\n");

    print!("\n---------Basic Optimised---------\n");

    ir_head = basic_optimiser::optimise_ir(ir_head.clone())?;

    // DEBUG
    println!("IR Structure:");
    println!("└── Root");
    for (i, node) in ir_head.children.iter().enumerate() {
        let is_last = i == ir_head.children.len() - 1;
        print_ir_node(node, "", is_last);
    }
    println!("\nOptimised IR generated successfully\n");

    ir_head = advanced_optimiser::optimise_ir(ir_head.clone())?;

    print!("\n---------Advanced Optimised---------\n");

    // DEBUG
    println!("IR Structure:");
    println!("└── Root");
    for (i, node) in ir_head.children.iter().enumerate() {
        let is_last = i == ir_head.children.len() - 1;
        print_ir_node(node, "", is_last);
    }
    println!("\n Advanced Optimised IR generated successfully\n");

    return Ok(ir_head); // Return the IR for the next compilation stage
}
fn print_ir_node(node: &types::IRNode, prefix: &str, is_last: bool) {
    let node_connector = if is_last { "└── " } else { "├── " };
    let node_info = format!("{:?}: {:?}", node.node_type, node.value);

    println!("{}{}{}", prefix, node_connector, node_info);

    if !node.children.is_empty() {
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };

        for (i, child) in node.children.iter().enumerate() {
            let is_last_child = i == node.children.len() - 1;
            print_ir_node(child, &child_prefix, is_last_child);
        }
    }
}
