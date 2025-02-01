
use crate::{
    AttributeType, IRProgram, IRRule, LHSNode, OperationType, RHSNode, RHSVal, RelationRole,
    RelationType, SSAInstruction, SSAInstructionBlock, SSAOperation,
};

/// Formats an IR program with configurable indentation, spacing and optional syntax highlighting.
pub struct IRFormatter {
    indent_size: usize,
    indent_char: char,
    use_highlighting: bool,
}

impl Default for IRFormatter {
    fn default() -> Self {
        Self {
            indent_size: 4,
            indent_char: ' ',
            use_highlighting: false,
        }
    }
}

impl IRFormatter {
    pub fn new(indent_size: usize, indent_char: char) -> Self {
        Self {
            indent_size,
            indent_char,
            use_highlighting: false,
        }
    }

    /// Creates a new formatter with syntax highlighting enabled
    pub fn with_highlighting(indent_size: usize, indent_char: char) -> Self {
        Self {
            indent_size,
            indent_char,
            use_highlighting: true,
        }
    }

    /// Enable or disable syntax highlighting
    pub fn set_highlighting(&mut self, enabled: bool) {
        self.use_highlighting = enabled;
    }

    fn indent(&self, level: usize) -> String {
        self.indent_char.to_string().repeat(level * self.indent_size)
    }

    pub fn format(&self, program: &IRProgram) -> String {
        if self.use_highlighting {
            use crate::syntax::Highlight;
            // First format the program normally, then apply highlighting
            let highlighted = program.highlight();
            let mut output = String::new();
            
            for part in highlighted {
                output.push_str(&part.to_string());
            }
            
            // Apply indentation to each line
            output.lines()
                .map(|line| {
                    if line.trim().is_empty() {
                        line.to_string()
                    } else {
                        format!("{}{}", self.indent(1), line)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n")
        } else {
            self.format_program_structure(program)
        }
    }

    fn format_program_structure(&self, program: &IRProgram) -> String {
        let mut output = String::new();
        
        // Format relations section
        output.push_str("relations {\n");
        for relation in &program.relations {
            output.push_str(&self.format_relation(relation, 1));
            output.push('\n');
        }
        output.push_str("}\n\n");

        // Format rules section
        output.push_str("rules {\n");
        for rule in &program.rules {
            output.push_str(&self.format_rule(rule, 1));
            output.push('\n');
        }
        output.push('}');

        output
    }

    fn format_relation(&self, relation: &RelationType, level: usize) -> String {
        let indent = self.indent(level);
        let attributes = relation
            .attributes
            .iter()
            .map(|attr| format!("{}: {}", attr.name, self.format_attribute_type(&attr.attr_type)))
            .collect::<Vec<_>>()
            .join(", ");

        format!(
            "{}{}({}) : {};",
            indent,
            relation.name,
            attributes,
            self.format_relation_role(&relation.role)
        )
    }

    fn format_attribute_type(&self, attr_type: &AttributeType) -> &'static str {
        match attr_type {
            AttributeType::String => "String",
            AttributeType::Number => "Number",
            AttributeType::Boolean => "Boolean",
            AttributeType::Date => "Date",
            AttributeType::Float => "Float",
        }
    }

    fn format_relation_role(&self, role: &RelationRole) -> &'static str {
        match role {
            RelationRole::Input => "Input",
            RelationRole::Output => "Output",
            RelationRole::Intermediate => "Intermediate",
        }
    }

    fn format_rule(&self, rule: &IRRule, level: usize) -> String {
        let indent = self.indent(level);
        let mut output = format!(
            "{}{} => {} {{",
            indent,
            self.format_lhs(&rule.lhs),
            self.format_rhs(&rule.rhs)
        );

        if let Some(block) = &rule.ssa_block {
            output.push('\n');
            output.push_str(&self.format_ssa_block(block, level + 1));
            output.push_str(&format!("{}}}", indent));
        } else {
            output.push_str(" }");
        }

        output
    }

    fn format_lhs(&self, lhs: &LHSNode) -> String {
        let mut attrs: Vec<_> = lhs.output_attributes.iter().map(|s| s.as_str()).collect();
        attrs.sort(); // Sort for consistent output
        format!("{}({})", lhs.relation_name, attrs.join(", "))
    }

    fn format_rhs(&self, rhs: &RHSVal) -> String {
        match rhs {
            RHSVal::RHSNode(node) => self.format_rhs_node(node),
            RHSVal::NestedRHS(nodes) => nodes
                .iter()
                .map(|n| self.format_rhs(n))
                .collect::<Vec<_>>()
                .join(", "),
        }
    }

    fn format_rhs_node(&self, node: &RHSNode) -> String {
        let mut attrs: Vec<_> = node.attributes.iter().map(|s| s.as_str()).collect();
        attrs.sort(); // Sort for consistent output
        format!("{}({})", node.relation_name, attrs.join(", "))
    }

    fn format_ssa_block(&self, block: &SSAInstructionBlock, level: usize) -> String {
        let mut output = String::new();
        for instruction in &block.instructions {
            output.push_str(&self.format_ssa_instruction(instruction, level));
            output.push('\n');
        }
        output
    }

    fn format_ssa_instruction(&self, instruction: &SSAInstruction, level: usize) -> String {
        let indent = self.indent(level);
        match instruction {
            SSAInstruction::Label(label) => format!("{}{}:", indent, label),
            SSAInstruction::Assignment { variable, operation } => {
                format!("{}{} = {};", indent, variable, self.format_ssa_operation(operation))
            }
            SSAInstruction::Goto(label) => format!("{}goto {};", indent, label),
            SSAInstruction::Branch {
                condition,
                true_label,
                false_label,
            } => format!(
                "{}if {} goto {} else goto {};",
                indent, condition, true_label, false_label
            ),
        }
    }

    fn format_ssa_operation(&self, operation: &SSAOperation) -> String {
        format!(
            "{}({})",
            self.format_operation_type(&operation.op_type),
            operation.operands.join(", ")
        )
    }

    fn format_operation_type(&self, op_type: &OperationType) -> &'static str {
        match op_type {
            OperationType::Add => "add",
            OperationType::Sub => "sub",
            OperationType::Mul => "mul",
            OperationType::Div => "div",
            OperationType::Mod => "mod",
            OperationType::Concat => "concat",
            OperationType::Eq => "eq",
            OperationType::Neq => "neq",
            OperationType::Lt => "lt",
            OperationType::Leq => "leq",
            OperationType::Gt => "gt",
            OperationType::Geq => "geq",
            OperationType::And => "and",
            OperationType::Or => "or",
            OperationType::Not => "not",
            OperationType::Cmp => "cmp",
            OperationType::Append => "append",
            OperationType::Contains => "contains",
            OperationType::Exists => "exists",
            OperationType::In => "in",
            OperationType::Within => "within",
            OperationType::StartsWith => "starts_with",
            OperationType::EndsWith => "ends_with",
            OperationType::Len => "len",
            OperationType::Load => "load",
            OperationType::Store => "store",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AttributeType;
    use crate::RelationRole;
    use std::collections::HashSet;

    #[test]
    fn test_highlighting() {
        let program = crate::IRProgram {
            relations: vec![crate::RelationType {
                name: "test_relation".to_string(),
                attributes: vec![
                    crate::Attribute {
                        name: "x".to_string(),
                        attr_type: AttributeType::Number,
                    },
                ],
                role: RelationRole::Input,
            }],
            rules: vec![],
        };

        let mut formatter = super::IRFormatter::default();
        let plain = formatter.format(&program);
        
        formatter.set_highlighting(true);
        let highlighted = formatter.format(&program);

        // Highlighted output should contain ANSI escape codes
        assert!(highlighted.contains("\x1b["));
        // Plain output should not contain ANSI escape codes
        assert!(!plain.contains("\x1b["));
    }

    #[test]
    fn test_format_simple_program() {
        let program = IRProgram {
            relations: vec![RelationType {
                name: "test_relation".to_string(),
                attributes: vec![
                    crate::Attribute {
                        name: "x".to_string(),
                        attr_type: AttributeType::Number,
                    },
                    crate::Attribute {
                        name: "y".to_string(),
                        attr_type: AttributeType::String,
                    },
                ],
                role: RelationRole::Input,
            }],
            rules: vec![],
        };

        let formatter = IRFormatter::default();
        let formatted = formatter.format(&program);

        let expected = r#"relations {
    test_relation(x: Number, y: String) : Input;
}

rules {}"#;

        assert_eq!(formatted, expected);
    }

    #[test]
    fn test_format_program_with_ssa() {
        let program = IRProgram {
            relations: vec![],
            rules: vec![IRRule {
                lhs: LHSNode {
                    relation_name: "output".to_string(),
                    output_attributes: HashSet::from(["x".to_string()]),
                },
                rhs: RHSVal::RHSNode(RHSNode {
                    relation_name: "input".to_string(),
                    attributes: HashSet::from(["a".to_string()]),
                }),
                ssa_block: Some(SSAInstructionBlock {
                    instructions: vec![
                        SSAInstruction::Label("L1".to_string()),
                        SSAInstruction::Assignment {
                            variable: "t1".to_string(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec!["a".to_string()],
                            },
                        },
                    ],
                }),
            }],
        };

        let formatter = IRFormatter::default();
        let formatted = formatter.format(&program);

        let expected = r#"relations {
}

rules {
    output(x) => input(a) {
        L1:
        t1 = load(a)
    }
}"#;

        assert_eq!(formatted, expected);
    }

    #[test]
    fn test_custom_indentation() {
        let program = IRProgram {
            relations: vec![RelationType {
                name: "test".to_string(),
                attributes: vec![crate::Attribute {
                    name: "x".to_string(),
                    attr_type: AttributeType::Number,
                }],
                role: RelationRole::Input,
            }],
            rules: vec![],
        };

        let formatter = IRFormatter::new(2, ' ');
        let formatted = formatter.format(&program);

        let expected = r#"relations {
  test(x: Number) : Input;
}

rules {}"#;

        assert_eq!(formatted, expected);
    }
}
