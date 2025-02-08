/*
# SSA-Based Intermediate Representation for Datalog Programs

This module provides a simplified **Intermediate Representation (IR)** for Datalog programs, designed with a focus on **Static Single Assignment (SSA)** style. The IR allows the user to define and manipulate Datalog rules programmatically, ensuring ease of use and flexibility for integration with various Datalog backends such as DDLog or Soufflé.

## Key Features

1. **SSA Style**:
   - The IR incorporates SSA blocks within rules, where each variable is assigned exactly once. This style simplifies analysis, transformation, and optimization of rules while adhering to modern compiler design principles.
   - SSA instructions include assignments, branches, and operations, making the IR expressive and extensible.

2. **Textual Representation**:
   - The IR supports a clear, human-readable textual format. This format can be **serialized from the IR's AST** or **parsed into the IR AST** using the included custom parser.
   - The textual representation emphasizes readability, with features like indentation for clarity and a structured format for relations, rules, and SSA blocks.

3. **Adhoc Parser**:
   - The module includes a **custom parser** for consuming the textual representation of the IR. This parser uses the `pest` library to ensure robustness and maintainability.
   - It validates the syntax, ensuring that the IR conforms to its defined grammar.

4. **Flexible and Backend-Agnostic**:
   - The IR is designed to be backend-agnostic, meaning it can be adapted for integration with different Datalog systems.
   - It provides a foundation for performing dependency analysis, rule generation, and program transformation.

## Structure

### Relations
Relations define the schema and role of each dataset in the program. They consist of:
- Attributes (e.g., `name: String`, `age: Number`)
- Roles (Input, Output, Intermediate)

### Rules
Rules connect relations and define how data flows between them. Each rule contains:
- A **left-hand side (LHS)** representing the output relation.
- A **right-hand side (RHS)** consisting of one or more input relations.
- An optional **SSA instruction block**, which defines the computations and control flow within the rule.

### SSA Instruction Block
The SSA block allows for detailed computations and control flow. It supports:
- **Labels** for branching and goto operations.
- **Assignments** with operations such as `add`, `sub`, `mul`, and more.
- **Control Flow** constructs like branching (`if`/`else`) and unconditional jumps (`goto`).

## Example

A simplified Datalog program in textual format:
```
relations {
    input_relation(a: Number, b: Boolean) : Input;
    temp_relation(c: Float) : Intermediate;
    output_relation(x: Float) : Intermediate;
}

rules {
    output_relation(x) => input_relation(a, b), temp_relation(c) {
        L1:
        t1 = load(a)
        t2 = add(t1, b)
        t3 = mul(t2, c)
        goto L2

        L2:
        x = t3
    }
}
```
*/

mod formatter;
mod syntax;

use formatter::IRFormatter;
use indexmap::IndexSet;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use std::fmt;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use log::debug;

pub fn format_program(
    input: &str,
    highlight: bool,
    indent_size: usize,
    use_tabs: bool,
) -> Result<String, String> {
    // Create program AST
    let program = parse(input).map_err(|e| e.to_string())?;

    // Create formatter with specified options
    let indent_char = if use_tabs { '\t' } else { ' ' };
    let mut formatter = IRFormatter::new(indent_size, indent_char);
    formatter.set_highlighting(highlight);

    // Format program
    Ok(formatter.format(&program))
}

#[derive(Clone, Debug, PartialEq)]
pub struct IRProgram {
    pub rules: Vec<IRRule>,
    pub relations: Vec<RelationType>,
}

impl IRProgram {
    pub fn new() -> Self {
        Self {
            rules: vec![],
            relations: vec![],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IRRule {
    pub lhs: LHSNode,
    pub rhs: RHSVal,
    pub ssa_block: Option<SSAInstructionBlock>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AttributeType {
    String,
    Number,
    Boolean,
    Date,
    Float,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub name: String,
    pub attr_type: AttributeType,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RelationRole {
    Input,
    Output,
    Intermediate,
    Internal,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RelationType {
    pub name: String,
    pub attributes: Vec<Attribute>,
    pub role: RelationRole,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RelationRef {
    pub name: String,
    pub role: RelationRole,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LHSNode {
    pub relation_name: String,
    pub output_attributes: IndexSet<String>,
}

#[derive(Clone, Debug)]
pub enum RHSVal {
    RHSNode(RHSNode),
    NestedRHS(Vec<RHSVal>),
}

#[derive(Clone, Debug)]
pub struct RHSNode {
    pub relation_name: String,
    pub attributes: Vec<String>,
}

/// Represents a block of SSA instructions.
#[derive(Clone, Debug, PartialEq)]
pub struct SSAInstructionBlock {
    pub instructions: Vec<SSAInstruction>,
}

/// Represents an individual SSA instruction.
#[derive(Clone, Debug, PartialEq)]
pub enum SSAInstruction {
    Label(String),
    Assignment {
        variable: String,
        operation: SSAOperation,
    },
    Goto(String),
    Branch {
        condition: String,
        true_label: String,
        false_label: String,
    },
}

/// Represents an SSA operation.
#[derive(Clone, Debug, PartialEq)]
pub struct SSAOperation {
    pub op_type: OperationType,
    pub operands: Vec<String>,
}

/// Enum for supported operation types in SSA.
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum OperationType {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Concat,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    And,
    Or,
    Not,
    Cmp,
    Append,
    Contains,
    Exists,
    In,
    Within,
    StartsWith,
    EndsWith,
    Len,
    Load,
    Store,
    Noop,
}

impl From<&str> for OperationType {
    fn from(op: &str) -> Self {
        match op {
            "+" => OperationType::Add,
            "-" => OperationType::Sub,
            "*" => OperationType::Mul,
            "/" => OperationType::Div,
            "concat" => OperationType::Concat,
            "and" => OperationType::And,
            "or" => OperationType::Or,
            "not" => OperationType::Not,
            "eq" => OperationType::Eq,
            "neq" => OperationType::Neq,
            "lt" => OperationType::Lt,
            "leq" => OperationType::Leq,
            "gt" => OperationType::Gt,
            "geq" => OperationType::Geq,
            _ => OperationType::Noop,
        }
    }
}

impl RelationRef {
    pub fn new(name: String, role: RelationRole) -> Self {
        Self { name, role }
    }
}

impl PartialEq for RHSVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RHSVal::RHSNode(a), RHSVal::RHSNode(b)) => a == b,
            (RHSVal::NestedRHS(a), RHSVal::NestedRHS(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialEq for RHSNode {
    fn eq(&self, other: &Self) -> bool {
        self.relation_name == other.relation_name && self.attributes == other.attributes
    }
}

fn indent(level: usize) -> String {
    "    ".repeat(level) // Four spaces per indentation level
}

impl fmt::Display for IRProgram {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let level = 0;
        writeln!(f, "{}relations {{", indent(level))?;
        for relation in &self.relations {
            writeln!(f, "{}{}", indent(level + 1), relation)?;
        }
        writeln!(f, "{}}}\n", indent(level))?;
        writeln!(f, "{}rules {{\n", indent(level))?;
        for rule in &self.rules {
            writeln!(f, "{}\n", rule.to_string_with_indent(level + 1))?;
        }
        write!(f, "{}}}", indent(level))
    }
}

impl fmt::Display for RelationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let attributes: Vec<String> = self
            .attributes
            .iter()
            .map(|attr| attr.to_string())
            .collect();
        write!(
            f,
            "{}({}) : {};",
            self.name,
            attributes.join(", "),
            match self.role {
                RelationRole::Input => "Input",
                RelationRole::Output => "Output",
                RelationRole::Intermediate => "Intermediate",
                RelationRole::Internal => "Internal",
            }
        )
    }
}

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {}",
            self.name,
            match self.attr_type {
                AttributeType::String => "String",
                AttributeType::Number => "Number",
                AttributeType::Boolean => "Boolean",
                AttributeType::Date => "Date",
                AttributeType::Float => "Float",
            }
        )
    }
}

impl IRRule {
    fn to_string_with_indent(&self, level: usize) -> String {
        let mut result = format!("{}{} => {} {{\n", indent(level), self.lhs, self.rhs);
        if let Some(block) = &self.ssa_block {
            result.push_str(&block.to_string_with_indent(level + 1));
        }
        result.push_str(&format!("{}}}", indent(level)));
        result
    }
}

impl fmt::Display for LHSNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let attributes: Vec<String> = self.output_attributes.iter().cloned().collect();
        write!(f, "{}({})", self.relation_name, attributes.join(", "))
    }
}

impl fmt::Display for RHSVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RHSVal::RHSNode(node) => write!(f, "{}", node),
            RHSVal::NestedRHS(nodes) => {
                let parts: Vec<String> = nodes.iter().map(|node| node.to_string()).collect();
                write!(f, "{}", parts.join(", "))
            }
        }
    }
}

impl fmt::Display for RHSNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut attributes: Vec<String> = self.attributes.iter().cloned().collect();
        attributes.sort();
        write!(f, "{}({})", self.relation_name, attributes.join(", "))
    }
}

impl SSAInstructionBlock {
    fn to_string_with_indent(&self, level: usize) -> String {
        let mut result = String::new();
        for instruction in &self.instructions {
            result.push_str(&format!("{}{}\n", indent(level), instruction));
        }
        result
    }
}

impl fmt::Display for SSAInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SSAInstruction::Label(label) => write!(f, "{}:", label),
            SSAInstruction::Assignment {
                variable,
                operation,
            } => {
                write!(f, "{} = {};", variable, operation)
            }
            SSAInstruction::Goto(label) => write!(f, "goto {};", label),
            SSAInstruction::Branch {
                condition,
                true_label,
                false_label,
            } => write!(
                f,
                "if {} goto {} else goto {};",
                condition, true_label, false_label
            ),
        }
    }
}

impl fmt::Display for SSAOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let operands = self.operands.join(", ");
        write!(f, "{}({})", self.op_type, operands)
    }
}

impl fmt::Display for OperationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
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
                OperationType::Len => "len",
                OperationType::Load => "load",
                OperationType::Store => "store",
                OperationType::In => "in",
                OperationType::Within => "within",
                OperationType::Exists => "exists",
                OperationType::StartsWith => "starts_with",
                OperationType::EndsWith => "ends_with",
                OperationType::Noop => "noop",
            }
        )
    }
}

#[derive(Debug)]
pub enum IRError {
    ParseError(String),
}

pub type IRResult<T> = Result<T, IRError>;

impl std::fmt::Display for IRError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IRError::ParseError(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl std::error::Error for IRError {}

impl From<pest::error::Error<parser::Rule>> for IRError {
    fn from(err: pest::error::Error<parser::Rule>) -> Self {
        IRError::ParseError(err.to_string())
    }
}

pub fn parse(input: &str) -> IRResult<IRProgram> {
    use parser::IRParser;
    let parsed = IRParser::parse(parser::Rule::ir, input)?.next().unwrap();
    Ok(parser::parse_ir_program(parsed))
}

mod parser {
    use super::*;

    #[derive(Parser)]
    #[grammar = "./ir.pest"]
    pub(crate) struct IRParser;

    pub(crate) fn parse_ir_program(parsed: Pair<Rule>) -> IRProgram {
        match parsed.as_rule() {
            Rule::ir => {
                let mut relations = vec![];
                let mut rules = vec![];

                for section in parsed.into_inner() {
                    match section.as_rule() {
                        Rule::relations_section => {
                            for relation in section.into_inner() {
                                relations.push(parse_relation(relation));
                            }
                        }
                        Rule::datalog_rules_section => {
                            for rule in section.into_inner() {
                                rules.push(parse_rule(rule));
                            }
                        }
                        Rule::EOI => {}
                        _ => panic!("Unsupported IR section: {:?}", section.as_rule()),
                    }
                }

                IRProgram { rules, relations }
            }
            _ => unreachable!(),
        }
    }

    fn parse_relation(parsed: Pair<Rule>) -> RelationType {
        match parsed.as_rule() {
            Rule::relation => {
                let mut inner = parsed.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let attributes: Vec<Attribute> = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(parse_attribute)
                    .collect();

                let role = parse_relation_role(inner.next().unwrap().as_str());

                RelationType {
                    name,
                    attributes,
                    role,
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_attribute(parsed: Pair<Rule>) -> Attribute {
        match parsed.as_rule() {
            Rule::attribute => {
                let mut inner = parsed.into_inner();
                let name = inner.next().unwrap().as_str().to_string();
                let attr_type = parse_attribute_type(inner.next().unwrap().as_str());

                Attribute { name, attr_type }
            }
            _ => unreachable!(),
        }
    }

    fn parse_attribute_type(attr_type: &str) -> AttributeType {
        match attr_type {
            "String" => AttributeType::String,
            "Number" => AttributeType::Number,
            "Boolean" => AttributeType::Boolean,
            "Date" => AttributeType::Date,
            "Float" => AttributeType::Float,
            _ => panic!("Unsupported attribute type: {}", attr_type),
        }
    }

    fn parse_relation_role(role: &str) -> RelationRole {
        match role {
            "Input" => RelationRole::Input,
            "Output" => RelationRole::Output,
            "Intermediate" => RelationRole::Intermediate,
            "Internal" => RelationRole::Internal,
            _ => panic!("Unsupported relation role: {}", role),
        }
    }

    fn parse_rule(parsed: Pair<Rule>) -> IRRule {
        match parsed.as_rule() {
            Rule::datalog_rule => {
                let mut inner = parsed.into_inner();
                let lhs = parse_lhs(inner.next().unwrap());
                let rhs = parse_rhs(inner.next().unwrap());
                let mut labels_and_ssa_instructions = vec![];
                for label_or_instruction in inner {
                    match label_or_instruction.as_rule() {
                        Rule::label => {
                            labels_and_ssa_instructions.push(SSAInstruction::Label(
                                label_or_instruction.as_str().replace(":", ""),
                            ));
                        }
                        Rule::ssa_instruction => {
                            labels_and_ssa_instructions.push(parse_ssa_instruction(
                                label_or_instruction.into_inner().next().unwrap(),
                            ));
                        }
                        _ => panic!(
                            "Unsupported rule section: {:?}",
                            label_or_instruction.as_rule()
                        ),
                    }
                }

                IRRule {
                    lhs,
                    rhs,
                    ssa_block: Some(SSAInstructionBlock {
                        instructions: labels_and_ssa_instructions,
                    }),
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_lhs(parsed: Pair<Rule>) -> LHSNode {
        match parsed.as_rule() {
            Rule::lhs_node => {
                let mut inner = parsed.into_inner();
                let relation_name = inner.next().unwrap().as_str().to_string();
                let output_attributes = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|a| a.as_str().to_string())
                    .collect::<IndexSet<_>>();

                LHSNode {
                    relation_name,
                    output_attributes,
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_rhs(parsed: Pair<Rule>) -> RHSVal {
        let mut nodes = vec![];

        for clause in parsed.into_inner() {
            match clause.as_rule() {
                Rule::rhs_node => nodes.push(RHSVal::RHSNode(parse_rhs_node(clause))),
                _ => unreachable!(),
            }
        }

        if nodes.len() == 1 {
            nodes.into_iter().next().unwrap()
        } else {
            RHSVal::NestedRHS(nodes)
        }
    }

    fn parse_rhs_node(parsed: Pair<Rule>) -> RHSNode {
        match parsed.as_rule() {
            Rule::rhs_node => {
                let mut inner = parsed.into_inner();
                let relation_name = inner.next().unwrap().as_str().to_string();
                let attributes = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|a| a.as_str().to_string())
                    .collect::<Vec<_>>();
                RHSNode {
                    relation_name,
                    attributes,
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_ssa_block(parsed: Option<Pair<Rule>>) -> Option<SSAInstructionBlock> {
        parsed.map(|block| {
            let mut instructions = vec![];
            for pair in block.into_inner() {
                instructions.push(parse_ssa_instruction(pair));
            }

            SSAInstructionBlock { instructions }
        })
    }

    fn parse_ssa_instruction(parsed: Pair<Rule>) -> SSAInstruction {
        match parsed.as_rule() {
            Rule::ssa_assignment => {
                let mut inner = parsed.into_inner();
                let variable = inner.next().unwrap().as_str().to_string();
                let identifier_or_operation = inner.next().unwrap();
                match identifier_or_operation.as_rule() {
                    Rule::identifier => {
                        SSAInstruction::Assignment {
                            variable,
                            operation: SSAOperation {
                                op_type: OperationType::Load, // Treat variable assignment as a Load
                                operands: vec![identifier_or_operation.as_str().to_string()],
                            },
                        }
                    }
                    Rule::ssa_operation => SSAInstruction::Assignment {
                        variable,
                        operation: parse_ssa_operation(identifier_or_operation),
                    },
                    x => panic!("Unsupported SSA assignment: {:?}", x),
                }
                //let operation = parse_ssa_operation(inner.next().unwrap());
            }
            Rule::goto => {
                let label = parsed.into_inner().next().unwrap().as_str().to_string();
                SSAInstruction::Goto(label)
            }
            Rule::control_flow => {
                let inner = parsed.into_inner().next().unwrap();
                match inner.as_rule() {
                    Rule::branch => parse_branch(inner),
                    Rule::goto => parse_goto(inner),
                    _ => unreachable!(),
                }
            }
            i => panic!("Unsupported SSA instruction: {:?}", i),
        }
    }

    fn parse_branch(parsed: Pair<Rule>) -> SSAInstruction {
        let mut inner = parsed.into_inner();
        let condition = inner.next().unwrap().as_str().to_string();
        let true_label = inner.next().unwrap().as_str().to_string();
        let false_label = inner.next().unwrap().as_str().to_string();

        SSAInstruction::Branch {
            condition,
            true_label,
            false_label,
        }
    }

    fn parse_goto(parsed: Pair<Rule>) -> SSAInstruction {
        let label = parsed.into_inner().next().unwrap().as_str().to_string();
        SSAInstruction::Goto(label)
    }
    fn parse_ssa_operation(parsed: Pair<Rule>) -> SSAOperation {
        match parsed.as_rule() {
            Rule::ssa_operation => {
                let mut inner = parsed.into_inner();
                let op_type = parse_operation_type(inner.next().unwrap().as_str());
                let operands: Vec<String> = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|o| o.as_str().to_string())
                    .collect();

                SSAOperation { op_type, operands }
            }
            o => panic!("Unsupported SSA operation: {:?} {}", o, parsed.as_str()),
        }
    }

    fn parse_operation_type(op_type: &str) -> OperationType {
        match op_type {
            "add" => OperationType::Add,
            "sub" => OperationType::Sub,
            "mul" => OperationType::Mul,
            "div" => OperationType::Div,
            "mod" => OperationType::Mod,
            "concat" => OperationType::Concat,
            "eq" => OperationType::Eq,
            "neq" => OperationType::Neq,
            "lt" => OperationType::Lt,
            "leq" => OperationType::Leq,
            "gt" => OperationType::Gt,
            "geq" => OperationType::Geq,
            "and" => OperationType::And,
            "or" => OperationType::Or,
            "not" => OperationType::Not,
            "cmp" => OperationType::Cmp,
            "append" => OperationType::Append,
            "contains" => OperationType::Contains,
            "len" => OperationType::Len,
            "load" => OperationType::Load,
            "store" => OperationType::Store,
            _ => panic!("Unsupported operation type: {}", op_type),
        }
    }

    #[test]
    fn test_valid_ir_with_multiple_rhs_nodes_and_ssa_instructions() {
        let input = r#"
    relations {
        input_relation(a: Number, b: Boolean): Input;
        temp_relation(c: Float): Intermediate;
        output_relation(x: Float): Intermediate;
    }

    rules {
        output_relation(x) => input_relation(a, b), temp_relation(c) {
            L1:
            t1 = load(a);
            t2 = add(t1, b);
            t3 = mul(t2, c);
            goto L2;

            L2:
            x = t3;
        }
    }
    "#;

        IRParser::parse(Rule::ir, input)
            .expect("Parsing failed for valid IR with multiple RHS nodes and SSA instructions");
    }

    #[test]
    fn test_valid_ir_with_empty_rules_section() {
        let input = r#"
    relations {
        input_relation(a: Number, b: Boolean): Input;
        temp_relation(c: Float): Intermediate;
        output_relation(x: Float): Intermediate;
    }

    rules {
    }
    "#;

        IRParser::parse(Rule::ir, input)
            .expect("Parsing failed for valid IR with an empty rules section");
    }

    #[test]
    fn test_valid_ir_without_ssa_instructions() {
        let input = r#"
    relations {
        input_relation(a: Number, b: Boolean): Input;
        temp_relation(c: Float): Intermediate;
        output_relation(x: Float): Intermediate;
    }

    rules {
        output_relation(x) => input_relation(a, b), temp_relation(c) {
        }
    }
    "#;

        IRParser::parse(Rule::ir, input)
            .expect("Parsing failed for valid IR without SSA instructions");
    }

    #[test]
    fn test_invalid_missing_rhs_nodes() {
        let input = r#"
    relations {
        input_relation(a: Number, b: Boolean): Input;
        temp_relation(c: Float): Intermediate;
        output_relation(x: Float): Intermediate;
    }

    rules {
        output_relation(x) => {
            t1 = load(a);
        }
    }
    "#;

        assert!(
            IRParser::parse(Rule::ir, input).is_err(),
            "Expected parsing to fail for missing RHS nodes"
        );
    }

    #[test]
    fn test_invalid_relation_declaration() {
        let input = r#"
    relations {
        input_relation(a: Number, b: Boolean): Input;
        temp_relation(c: UnknownType): Intermediate; // Invalid attribute type
        output_relation(x: Float): Intermediate;
    }

    rules {
        output_relation(x) => input_relation(a, b), temp_relation(c) {
            L1:
            t1 = load(a);
        }
    }
    "#;

        assert!(
            IRParser::parse(Rule::ir, input).is_err(),
            "Expected parsing to fail for invalid relation declaration"
        );
    }

    #[test]
    fn test_ir_text_to_ast() {
        let input = r#"
    relations {
        input_relation(a: Number, b: Boolean): Input;
        temp_relation(c: Float): Intermediate;
        output_relation(x: Float): Intermediate;
    }

    rules {
        output_relation(x) => input_relation(a, b), temp_relation(c) {
            L1:
            t1 = load(a);
            t2 = add(t1, b);

            L2:
            t3 = mul(t2, c);
            goto L3;

            L3:
            x = t3;
        }
    }
    "#;

        let pair = IRParser::parse(Rule::ir, input)
            .expect("Failed to parse input")
            .next()
            .unwrap();

        let program = parse_ir_program(pair);

        let expected_ast = IRProgram {
            relations: vec![
                RelationType {
                    name: "input_relation".to_string(),
                    attributes: vec![
                        Attribute {
                            name: "a".to_string(),
                            attr_type: AttributeType::Number,
                        },
                        Attribute {
                            name: "b".to_string(),
                            attr_type: AttributeType::Boolean,
                        },
                    ],
                    role: RelationRole::Input,
                },
                RelationType {
                    name: "temp_relation".to_string(),
                    attributes: vec![Attribute {
                        name: "c".to_string(),
                        attr_type: AttributeType::Float,
                    }],
                    role: RelationRole::Intermediate,
                },
                RelationType {
                    name: "output_relation".to_string(),
                    attributes: vec![Attribute {
                        name: "x".to_string(),
                        attr_type: AttributeType::Float,
                    }],
                    role: RelationRole::Intermediate,
                },
            ],
            rules: vec![IRRule {
                lhs: LHSNode {
                    relation_name: "output_relation".to_string(),
                    output_attributes: IndexSet::from(["x".to_string()]),
                },
                rhs: RHSVal::NestedRHS(vec![
                    RHSVal::RHSNode(RHSNode {
                        relation_name: "input_relation".to_string(),
                        attributes: IndexSet::from(["b".to_string(), "a".to_string()]),
                    }),
                    RHSVal::RHSNode(RHSNode {
                        relation_name: "temp_relation".to_string(),
                        attributes: IndexSet::from(["c".to_string()]),
                    }),
                ]),
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
                        SSAInstruction::Assignment {
                            variable: "t2".to_string(),
                            operation: SSAOperation {
                                op_type: OperationType::Add,
                                operands: vec!["t1".to_string(), "b".to_string()],
                            },
                        },
                        SSAInstruction::Label("L2".to_string()),
                        SSAInstruction::Assignment {
                            variable: "t3".to_string(),
                            operation: SSAOperation {
                                op_type: OperationType::Mul,
                                operands: vec!["t2".to_string(), "c".to_string()],
                            },
                        },
                        SSAInstruction::Goto("L3".to_string()),
                        SSAInstruction::Label("L3".to_string()),
                        SSAInstruction::Assignment {
                            variable: "x".to_string(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec!["t3".to_string()],
                            },
                        },
                    ],
                }),
            }],
        };

        assert_eq!(program, expected_ast);
    }

    #[test]
    fn test_serialize_relations() {
        let relations = vec![
            RelationType {
                name: "input_relation".to_string(),
                attributes: vec![
                    Attribute {
                        name: "a".to_string(),
                        attr_type: AttributeType::Number,
                    },
                    Attribute {
                        name: "b".to_string(),
                        attr_type: AttributeType::Boolean,
                    },
                ],
                role: RelationRole::Input,
            },
            RelationType {
                name: "temp_relation".to_string(),
                attributes: vec![Attribute {
                    name: "c".to_string(),
                    attr_type: AttributeType::Float,
                }],
                role: RelationRole::Intermediate,
            },
        ];

        let program = IRProgram {
            relations,
            rules: vec![], // Empty rules for this test
        };

        let expected_output = r#"relations {
    input_relation(a: Number, b: Boolean) : Input;
    temp_relation(c: Float) : Intermediate;
}

rules {
}"#;

        assert_eq!(program.to_string(), expected_output);
    }

    #[test]
    fn test_serialize_rules_with_ssa() {
        let rules = vec![IRRule {
            lhs: LHSNode {
                relation_name: "output_relation".to_string(),
                output_attributes: HashSet::from(["x".to_string()]),
            },
            rhs: RHSVal::NestedRHS(vec![
                RHSVal::RHSNode(RHSNode {
                    relation_name: "input_relation".to_string(),
                    attributes: HashSet::from(["a".to_string(), "b".to_string()]),
                }),
                RHSVal::RHSNode(RHSNode {
                    relation_name: "temp_relation".to_string(),
                    attributes: HashSet::from(["c".to_string()]),
                }),
            ]),
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
                    SSAInstruction::Assignment {
                        variable: "t2".to_string(),
                        operation: SSAOperation {
                            op_type: OperationType::Add,
                            operands: vec!["t1".to_string(), "b".to_string()],
                        },
                    },
                    SSAInstruction::Goto("L2".to_string()),
                ],
            }),
        }];

        let program = IRProgram {
            relations: vec![],
            rules,
        };

        let expected_output = r#"relations {
}

rules {
    output_relation(x) => input_relation(a, b), temp_relation(c) {
        L1:
        t1 = load(a)
        t2 = add(t1, b)
        goto L2
    }
}"#;

        assert_eq!(program.to_string(), expected_output);
    }
}
