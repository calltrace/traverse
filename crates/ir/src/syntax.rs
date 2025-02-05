use crate::{
    AttributeType, IRProgram, IRRule, LHSNode, OperationType, RHSNode, RHSVal, RelationRole,
    RelationType, SSAInstruction, SSAOperation,
};
use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq)]
pub enum HighlightColor {
    Keyword,   // For keywords like relations, rules
    Type,      // For types like Number, String
    Relation,  // For relation names
    Variable,  // For variables in SSA
    Label,     // For SSA labels
    Operation, // For SSA operations
    Delimiter, // For delimiters like {}, (), =>
    Role,      // For relation roles like Input, Output
}

pub struct Highlighted<T> {
    content: T,
    color: HighlightColor,
}

impl<T: Display> Display for Highlighted<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.color {
            HighlightColor::Keyword => write!(f, "\x1b[1;34m{}\x1b[0m", self.content), // Bold Blue
            HighlightColor::Type => write!(f, "\x1b[1;32m{}\x1b[0m", self.content),    // Bold Green
            HighlightColor::Relation => write!(f, "\x1b[1;35m{}\x1b[0m", self.content), // Bold Magenta
            HighlightColor::Variable => write!(f, "\x1b[0;36m{}\x1b[0m", self.content), // Cyan
            HighlightColor::Label => write!(f, "\x1b[1;33m{}\x1b[0m", self.content), // Bold Yellow
            HighlightColor::Operation => write!(f, "\x1b[0;35m{}\x1b[0m", self.content), // Magenta
            HighlightColor::Delimiter => write!(f, "\x1b[0;37m{}\x1b[0m", self.content), // White
            HighlightColor::Role => write!(f, "\x1b[1;31m{}\x1b[0m", self.content),  // Bold Red
        }
    }
}

pub trait Highlight {
    fn highlight(&self) -> Vec<Highlighted<String>> {
        self.highlight_with_indent(0)
    }

    fn highlight_with_indent(&self, indent: usize) -> Vec<Highlighted<String>>;
}

impl Highlight for IRProgram {
    fn highlight_with_indent(&self, indent: usize) -> Vec<Highlighted<String>> {
        let mut result = vec![];

        // Relations section
        result.push(Highlighted {
            content: "relations".to_string(),
            color: HighlightColor::Keyword,
        });
        result.push(Highlighted {
            content: " {\n".to_string(),
            color: HighlightColor::Delimiter,
        });

        // Add relations
        for relation in &self.relations {
            result.push(Highlighted {
                content: " ".repeat(indent + 4),
                color: HighlightColor::Delimiter,
            });
            result.extend(relation.highlight_with_indent(indent + 4));
        }

        result.push(Highlighted {
            content: "}\n\nrules".to_string(),
            color: HighlightColor::Keyword,
        });
        result.push(Highlighted {
            content: " {\n".to_string(),
            color: HighlightColor::Delimiter,
        });

        // Add rules
        for rule in &self.rules {
            result.push(Highlighted {
                content: " ".repeat(indent + 4),
                color: HighlightColor::Delimiter,
            });
            result.extend(rule.highlight_with_indent(indent + 4));
        }

        result.push(Highlighted {
            content: "}".to_string(),
            color: HighlightColor::Delimiter,
        });

        result
    }
}

impl Highlight for RelationType {
    fn highlight_with_indent(&self, indent: usize) -> Vec<Highlighted<String>> {
        let mut result = vec![];

        // Relation name
        result.push(Highlighted {
            content: self.name.clone(),
            color: HighlightColor::Relation,
        });

        result.push(Highlighted {
            content: "(".to_string(),
            color: HighlightColor::Delimiter,
        });

        // Attributes
        for (i, attr) in self.attributes.iter().enumerate() {
            if i > 0 {
                result.push(Highlighted {
                    content: ", ".to_string(),
                    color: HighlightColor::Delimiter,
                });
            }
            result.push(Highlighted {
                content: format!("{}: ", attr.name),
                color: HighlightColor::Variable,
            });
            result.push(Highlighted {
                content: format!("{:?}", attr.attr_type),
                color: HighlightColor::Type,
            });
        }

        result.push(Highlighted {
            content: ") : ".to_string(),
            color: HighlightColor::Delimiter,
        });

        // Role
        result.push(Highlighted {
            content: format!("{:?}", self.role),
            color: HighlightColor::Role,
        });

        result.push(Highlighted {
            content: ";\n".to_string(),
            color: HighlightColor::Delimiter,
        });

        result
    }
}

impl Highlight for IRRule {
    fn highlight_with_indent(&self, indent: usize) -> Vec<Highlighted<String>> {
        let mut result = vec![];

        // LHS
        result.extend(self.lhs.highlight_with_indent(indent));

        result.push(Highlighted {
            content: " => ".to_string(),
            color: HighlightColor::Delimiter,
        });

        // RHS
        match &self.rhs {
            RHSVal::RHSNode(node) => result.extend(node.highlight_with_indent(indent)),
            RHSVal::NestedRHS(nodes) => {
                for (i, node) in nodes.iter().enumerate() {
                    if i > 0 {
                        result.push(Highlighted {
                            content: ", ".to_string(),
                            color: HighlightColor::Delimiter,
                        });
                    }
                    match node {
                        RHSVal::RHSNode(n) => result.extend(n.highlight_with_indent(indent)),
                        RHSVal::NestedRHS(_) => {} // Handle nested case if needed
                    }
                }
            }
        }

        result.push(Highlighted {
            content: " {\n".to_string(),
            color: HighlightColor::Delimiter,
        });

        // SSA Block
        if let Some(block) = &self.ssa_block {
            for instruction in &block.instructions {
                match instruction {
                    SSAInstruction::Label(label) => {
                        result.push(Highlighted {
                            content: format!("{}{}:\n", " ".repeat(indent + 4), label),
                            color: HighlightColor::Label,
                        });
                    }
                    SSAInstruction::Assignment {
                        variable,
                        operation,
                    } => {
                        result.push(Highlighted {
                            content: " ".repeat(indent + 8),
                            color: HighlightColor::Delimiter,
                        });
                        result.extend(highlight_ssa_assignment(variable, operation));
                        result.push(Highlighted {
                            content: "\n".to_string(),
                            color: HighlightColor::Delimiter,
                        });
                    }
                    SSAInstruction::Goto(label) => {
                        result.push(Highlighted {
                            content: format!("{}goto ", " ".repeat(indent + 8)),
                            color: HighlightColor::Keyword,
                        });
                        result.push(Highlighted {
                            content: format!("{}\n", label),
                            color: HighlightColor::Label,
                        });
                    }
                    SSAInstruction::Branch {
                        condition,
                        true_label,
                        false_label,
                    } => {
                        result.push(Highlighted {
                            content: " ".repeat(indent + 8),
                            color: HighlightColor::Delimiter,
                        });
                        result.extend(highlight_branch(condition, true_label, false_label));
                        result.push(Highlighted {
                            content: "\n".to_string(),
                            color: HighlightColor::Delimiter,
                        });
                    }
                }
            }
        }

        // ident two spaces
        result.push(Highlighted {
            content: " ".repeat(indent),
            color: HighlightColor::Delimiter,
        });

        result.push(Highlighted {
            content: "}\n\n".to_string(),
            color: HighlightColor::Delimiter,
        });

        result
    }
}

fn highlight_ssa_assignment(variable: &str, operation: &SSAOperation) -> Vec<Highlighted<String>> {
    let mut result = vec![];

    result.push(Highlighted {
        content: variable.to_string(),
        color: HighlightColor::Variable,
    });

    result.push(Highlighted {
        content: " = ".to_string(),
        color: HighlightColor::Delimiter,
    });

    result.push(Highlighted {
        content: format!("{:?}", operation.op_type).to_lowercase(),
        color: HighlightColor::Operation,
    });

    result.push(Highlighted {
        content: "(".to_string(),
        color: HighlightColor::Delimiter,
    });

    for (i, operand) in operation.operands.iter().enumerate() {
        if i > 0 {
            result.push(Highlighted {
                content: ", ".to_string(),
                color: HighlightColor::Delimiter,
            });
        }
        result.push(Highlighted {
            content: operand.clone(),
            color: HighlightColor::Variable,
        });
    }

    result.push(Highlighted {
        content: ");".to_string(),
        color: HighlightColor::Delimiter,
    });

    result
}

fn highlight_branch(
    condition: &str,
    true_label: &str,
    false_label: &str,
) -> Vec<Highlighted<String>> {
    let mut result = vec![];

    result.push(Highlighted {
        content: "if ".to_string(),
        color: HighlightColor::Keyword,
    });

    result.push(Highlighted {
        content: condition.to_string(),
        color: HighlightColor::Variable,
    });

    result.push(Highlighted {
        content: " goto ".to_string(),
        color: HighlightColor::Keyword,
    });

    result.push(Highlighted {
        content: true_label.to_string(),
        color: HighlightColor::Label,
    });

    result.push(Highlighted {
        content: " else goto ".to_string(),
        color: HighlightColor::Keyword,
    });

    result.push(Highlighted {
        content: false_label.to_string(),
        color: HighlightColor::Label,
    });

    result
}

impl Highlight for LHSNode {
    fn highlight_with_indent(&self, _indent: usize) -> Vec<Highlighted<String>> {
        let mut result = vec![];

        result.push(Highlighted {
            content: self.relation_name.clone(),
            color: HighlightColor::Relation,
        });

        result.push(Highlighted {
            content: "(".to_string(),
            color: HighlightColor::Delimiter,
        });

        let attrs: Vec<_> = self.output_attributes.iter().collect();
        for (i, attr) in attrs.iter().enumerate() {
            if i > 0 {
                result.push(Highlighted {
                    content: ", ".to_string(),
                    color: HighlightColor::Delimiter,
                });
            }
            result.push(Highlighted {
                content: (*attr).clone(),
                color: HighlightColor::Variable,
            });
        }

        result.push(Highlighted {
            content: ")".to_string(),
            color: HighlightColor::Delimiter,
        });

        result
    }
}

impl Highlight for RHSNode {
    fn highlight_with_indent(&self, _indent: usize) -> Vec<Highlighted<String>> {
        let mut result = vec![];

        result.push(Highlighted {
            content: self.relation_name.clone(),
            color: HighlightColor::Relation,
        });

        result.push(Highlighted {
            content: "(".to_string(),
            color: HighlightColor::Delimiter,
        });

        let attrs: Vec<_> = self.attributes.iter().collect();
        for (i, attr) in attrs.iter().enumerate() {
            if i > 0 {
                result.push(Highlighted {
                    content: ", ".to_string(),
                    color: HighlightColor::Delimiter,
                });
            }
            result.push(Highlighted {
                content: (*attr).clone(),
                color: HighlightColor::Variable,
            });
        }

        result.push(Highlighted {
            content: ")".to_string(),
            color: HighlightColor::Delimiter,
        });

        result
    }
}
