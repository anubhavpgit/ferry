mod generator;
mod optimiser;
mod types;

use self::generator::IRGenerator;
use crate::parser::ast;

pub fn generate_ir(ast: &ast::ASTNode) -> Result<(), String> {
    let mut generator = IRGenerator::new();
    let ir_head = generator.generate(ast)?;

    // optimiser::optimize_ir(&ir_head)?;

    // DEBUG
    println!("IR Structure:");
    println!("└── Root");
    for (i, node) in ir_head.children.iter().enumerate() {
        let is_last = i == ir_head.children.len() - 1;
        let node_connector = if is_last { "└── " } else { "├── " };
        let node_info = format!("{:?}: {:?}", node.node_type, node.value);
        println!("{}{}{}", "", node_connector, node_info);
        if !node.children.is_empty() {
            let child_prefix = if is_last {
                format!("{}    ", "")
            } else {
                format!("{}│   ", "")
            };

            for (i, child) in node.children.iter().enumerate() {
                let is_last_child = i == node.children.len() - 1;
                let child_node_connector = if is_last_child {
                    "└── "
                } else {
                    "├── "
                };
                let child_node_info = format!("{:?}: {:?}", child.node_type, child.value);
                println!(
                    "{}{}{}",
                    &child_prefix, child_node_connector, child_node_info
                );
            }
        }
    }
    println!("\nIR generated successfully\n");

    Ok(())
}
