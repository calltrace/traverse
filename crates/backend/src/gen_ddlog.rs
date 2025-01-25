use language::node_types::{ContextFreeNodeType, NodeTypeKind};
use std::collections::{BTreeMap, BTreeSet};
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

use crate::ddlog_lang::{
    Atom, DType, DatalogProgram, Delay, Expr, ExprNode, Field, ModuleName, Pos, Relation,
    RelationRole, RelationSemantics, Rule, RuleLHS, RuleRHS,
};

use ir::{
    Attribute, AttributeType, IRProgram, IRRule, LHSNode, OperationType, RHSNode, RHSVal,
    RelationRole as IRRelationRole, RelationType as IRRelationType, SSAInstruction,
    SSAInstructionBlock, SSAOperation,
};


const RESERVED_WORDS: &[&str] = &["else", "function", "type", "match", "var"];

pub struct DDlogGenerator {
    generate_input_relations: bool, // Option to control input relation generation
}

impl DDlogGenerator {
    /// Creates a new compiler with default options
    pub fn new() -> Self {
        Self {
            generate_input_relations: false, // Default to not generating input relations
        }
    }

    /// Enable or disable input relation generation
    pub fn with_input_relations(mut self, generate: bool) -> Self {
        self.generate_input_relations = generate;
        self
    }

    ///
    /// Converts an IRProgram into a DatalogProgram, handling nested RHSVal structures.
    pub fn generate(&self, ir: IRProgram) -> IrToDdlogResult {

        fn expand_rhs_val(val: &RHSVal) -> Vec<RuleRHS> {
            match val {
                RHSVal::RHSNode(node) => {
                    // Create a RHSLiteral referencing the input relation
                    let literal = RuleRHS::RHSLiteral {
                        pos: Pos::nopos(),
                        polarity: true,
                        atom: Atom {
                            pos: Pos::nopos(),
                            relation: node.relation_name.clone(),
                            delay: Delay::zero(),
                            diff: false,
                            value: Expr::new(ExprNode::EVar {
                                pos: Pos::nopos(),
                                name: node
                                    .attributes
                                    .iter()
                                    .cloned()
                                    .map(|s| s.to_lowercase())
                                    .collect::<Vec<_>>()
                                    .join(", "),
                            }),
                        },
                    };

                    vec![literal]
                }

                RHSVal::NestedRHS(vals) => {
                    let mut rhs_clauses = Vec::new();
                    for val in vals {
                        rhs_clauses.extend(expand_rhs_val(val));
                    }
                    rhs_clauses
                }
            }
        }

        let mut program = DatalogProgram::new();
        for relation in &ir.relations {
            if let Some(lhs_relation) = self.lookup_relation(&ir, &relation.name, None) {
                let relation_fields = lhs_relation
                    .attributes
                    .iter()
                    .map(|attr| Field {
                        pos: Pos::nopos(),
                        name: attr.name.clone(),
                        ftype: DType::TString { pos: Pos::nopos() },
                    })
                    .collect::<Vec<_>>();

                match relation.role {
                    IRRelationRole::Output => {
                        program.add_relation(Relation {
                            pos: Pos::nopos(),
                            role: RelationRole::RelOutput, // All are output relations
                            semantics: RelationSemantics::RelSet, // Defaulting to set semantics
                            name: relation.name.clone(),
                            rtype: DType::TStruct {
                                pos: Pos::nopos(),
                                name: relation.name.clone(),
                                fields: relation_fields,
                            }, // Assuming attributes are strings
                            primary_key: None,
                        });
                    }
                    IRRelationRole::Input => {
                        // Optionally add input relations based on the role
                        if self.generate_input_relations {
                            program.add_relation(Relation {
                                pos: Pos::nopos(),
                                role: RelationRole::RelInput,
                                semantics: RelationSemantics::RelSet,
                                name: relation.name.clone(),
                                rtype: DType::TStruct {
                                    pos: Pos::nopos(),
                                    name: relation.name.clone(),
                                    fields: relation_fields,
                                }, // Assuming attributes are strings
                                primary_key: None,
                            });
                        }
                    }
                    IRRelationRole::Intermediate => {
                        unreachable!("Intermediate relations are not supported in DDlog");
                    }
                }
            }
        }

        for rule in &ir.rules {
            if let Some(lhs_relation) =
                self.lookup_relation(&ir, &rule.lhs.relation_name, Some(IRRelationRole::Output))
            {
                let lhs_attributes = lhs_relation
                    .attributes
                    .iter()
                    .cloned()
                    .map(|s| s.name)
                    .collect::<HashSet<_>>();

                let lhs = RuleLHS {
                    pos: Pos::nopos(),
                    atom: Atom {
                        pos: Pos::nopos(),
                        relation: rule.lhs.relation_name.clone(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::new(ExprNode::EVar {
                            pos: Pos::nopos(),
                            name: lhs_attributes
                                .iter()
                                .cloned()
                                .map(|s| format!("lhs_{}", s))
                                .collect::<Vec<_>>()
                                .join(", "),
                        }),
                    },
                    location: None,
                };

                let rhs_clauses = expand_rhs_val(&rule.rhs);
                let rhs_attributes = self.collect_all_rhs_attributes(&ir, &rule.rhs);

                let mut conditions = Vec::new();
                // HACK: the RI grammar should allow for disambiguating relations in terms of their
                // role (e.g. emission, capturing)
                if lhs_relation.name.starts_with("Emit") {
                    let condition_exprs = Self::process_labeled_blocks_to_mapping_expr(
                        rule.ssa_block.as_ref().unwrap(),
                    );

                    for condition_expr in condition_exprs {
                        conditions.push(RuleRHS::RHSCondition {
                            pos: Pos::nopos(),
                            expr: Expr::new(ExprNode::EVar {
                                pos: Pos::nopos(),
                                name: condition_expr,
                            }),
                        });
                    }

                    let mapping_expr = self.generate_ddlog_interpolation(
                        &format!("lhs_{}", lhs_relation.attributes[0].name),
                        rule.ssa_block
                            .as_ref()
                            .map(|block| block.instructions.clone())
                            .unwrap_or_default(),
                    );

                    conditions.push(RuleRHS::RHSCondition {
                        pos: Pos::nopos(),
                        expr: Expr::new(ExprNode::EVar {
                            pos: Pos::nopos(),
                            name: mapping_expr,
                        }),
                    });
                } else {
                    // Capture form
                    let lhs_rhs_pairs = lhs_attributes.iter().zip(rhs_attributes);

                    // actions override automatic RHS to LHS assignments
                    if rule.ssa_block.is_some() {
                        let mapping_expr = self.generate_ddlog_interpolation(
                            &format!("lhs_{}", lhs_relation.attributes[0].name),
                            rule.ssa_block
                                .as_ref()
                                .map(|block| block.instructions.clone())
                                .unwrap_or_default(),
                        );

                        conditions.push(RuleRHS::RHSCondition {
                            pos: Pos::nopos(),
                            expr: Expr::new(ExprNode::EVar {
                                pos: Pos::nopos(),
                                name: mapping_expr,
                            }),
                        });
                    } else {
                        for (lhs_attr, rhs_attr) in lhs_rhs_pairs {
                            let mapping_expr = format!("var lhs_{} = {}", lhs_attr, rhs_attr);
                            conditions.push(RuleRHS::RHSCondition {
                                pos: Pos::nopos(),
                                expr: Expr::new(ExprNode::EVar {
                                    pos: Pos::nopos(),
                                    name: mapping_expr,
                                }),
                            });
                        }
                    }
                }

                let rhs_clauses = rhs_clauses.into_iter().chain(conditions).collect();

                program.add_rule(Rule {
                    pos: Pos::nopos(),
                    module: ModuleName { path: vec![] }, // Default module
                    lhs: vec![lhs],
                    rhs: rhs_clauses,
                });
            }
        }

        Ok(program.into())
    }

    fn collect_all_rhs_attributes(&self, ir: &IRProgram, val: &RHSVal) -> HashSet<String> {
        match val {
            RHSVal::RHSNode(node) => {
                if self
                    .lookup_relation(ir, &node.relation_name, None)
                    .is_some()
                {
                    node.attributes.clone()
                } else {
                    HashSet::new()
                }
            }
            RHSVal::NestedRHS(vals) => {
                let mut collected_attributes = HashSet::new();
                for val in vals {
                    collected_attributes.extend(self.collect_all_rhs_attributes(ir, val));
                }
                collected_attributes
            }
        }
    }

    fn lookup_relation<'a>(
        &self,
        ir: &'a IRProgram,
        rel_name: &str,
        rel_role: Option<IRRelationRole>,
    ) -> Option<&'a IRRelationType> {
        ir.relations.iter().find(|rel| {
            rel.name == rel_name && rel_role.as_ref().map(|r| *r == rel.role).unwrap_or(true)
        })
    }

    fn process_labeled_blocks_to_mapping_expr(ssa_block: &SSAInstructionBlock) -> Vec<String> {
        let mut mapping_expressions = Vec::new();
        let mut current_label = None;
        let mut current_instructions = Vec::new();

        for instruction in &ssa_block.instructions {
            match instruction {
                SSAInstruction::Label(label) => {
                    // If there is a current block being processed, check if it ends with a predicate
                    if let Some(curr_label) = &current_label {
                        if let Some(expr) =
                            Self::process_block_for_predicate(curr_label, &current_instructions)
                        {
                            mapping_expressions.push(expr);
                        }
                    }
                    // Start a new labeled block
                    current_label = Some(label.clone());
                    current_instructions.clear();
                }
                _ => {
                    // Collect instructions for the current block
                    current_instructions.push(instruction.clone());
                }
            }
        }

        // Process the last block after the loop
        if let Some(curr_label) = &current_label {
            if let Some(expr) = Self::process_block_for_predicate(curr_label, &current_instructions)
            {
                mapping_expressions.push(expr);
            }
        }

        mapping_expressions
    }

    /// Checks if a block ends with a predicate operator and converts it to a mapping expression.
    fn process_block_for_predicate(
        label: &String,
        instructions: &[SSAInstruction],
    ) -> Option<String> {
        let mut operand_mapping: HashMap<String, String> = HashMap::new();
        let mut operator_mapping: HashMap<OperationType, String> = HashMap::new();
        operator_mapping.insert(OperationType::Eq, "==".to_string());

        // iterate over the instructions and create variables for each variable assignment and operand
        for instr in instructions {
            match instr {
                SSAInstruction::Assignment {
                    variable,
                    operation,
                } if operation.op_type == OperationType::Load => {
                    operand_mapping.insert(variable.clone(), operation.operands[0].clone());
                }
                _ => {
                    // Ignore other types of instructions
                }
            }
        }

        if let Some(SSAInstruction::Assignment {
            variable: _,
            operation,
            ..
        }) = instructions.last()
        {
            // Infix
            if matches!(
                operation.op_type,
                OperationType::Eq
                    | OperationType::Neq
                    | OperationType::Lt
                    | OperationType::Leq
                    | OperationType::Gt
                    | OperationType::Geq
                    | OperationType::And
                    | OperationType::Or
                    | OperationType::Not
            ) {
                // Generate the mapping expression
                // create infix operator (e.g. a == b)
                return Some(format!(
                    "{} {} {}",
                    operand_mapping.get(&operation.operands[0]).unwrap(),
                    operator_mapping.get(&operation.op_type).unwrap(),
                    operand_mapping.get(&operation.operands[1]).unwrap()
                ));

                /*
                 */
            }

            // prefix operator (e.g. string_contains())
            if matches!(operation.op_type, OperationType::Contains) {
                let operands_str = operation
                    .operands
                    .iter()
                    .map(|op| operand_mapping.get(op).cloned().unwrap())
                    .collect::<Vec<_>>()
                    .join(", ");
                return Some(format!(
                    "{}({})",
                    operator_mapping.get(&operation.op_type).unwrap(),
                    operands_str
                ));
            }
        }
        None
    }
    /// Generate a DDLog-style string interpolation statement
    fn generate_ddlog_interpolation(
        &self,
        lhs_attribute: &str,
        ssa_instructions: Vec<SSAInstruction>,
    ) -> String {
        // Map to store the operations for each SSA variable
        let mut instruction_map: HashMap<String, SSAOperation> = HashMap::new();

        // Populate the map with SSA instructions
        let mut final_var = String::new();
        for instr in &ssa_instructions {
            match instr {
                SSAInstruction::Assignment {
                    variable,
                    operation,
                } => {
                    instruction_map.insert(variable.clone(), operation.clone());

                    // if it's the last instruction
                    if ssa_instructions.last().unwrap() == instr {
                        final_var = variable.clone();
                    }
                }
                _ => {
                    // Ignore other types of instructions
                }
            }
        }

        // Recursively resolve the operands into a DDLog-style concatenation statement
        fn resolve_operands(var: &str, map: &HashMap<String, SSAOperation>) -> String {
            if let Some(operation) = map.get(var) {
                if operation.op_type == OperationType::Concat {
                    let resolved_operands: Vec<String> = operation
                        .operands
                        .iter()
                        // remove $ prefix
                        .map(|operand| operand.trim_start_matches('$'))
                        .map(|operand| resolve_operands(operand, map))
                        .collect();
                    return resolved_operands.join(" ++ ");
                }
            }
            // If it's not an SSA variable (e.g., a static string), return as-is
            var.to_string()
        }

        // Generate the DDLog interpolation statement
        format!(
            "var {} = {}",
            lhs_attribute,
            resolve_operands(&final_var, &instruction_map)
        )
    }

    pub(crate) fn to_pascal_case(s: &str) -> String {
        s.split('_')
            .filter(|part| !part.is_empty())
            .map(|part| {
                let mut chars = part.chars();
                match chars.next() {
                    None => String::new(),
                    Some(c) => c.to_ascii_uppercase().to_string() + chars.as_str(),
                }
            })
            .collect()
    }

    pub(crate) fn sanitize_reserved(name: &str) -> String {
        let lower = name.to_lowercase();
        if RESERVED_WORDS.contains(&lower.as_str()) {
            format!("_{}", name)
        } else {
            name.to_string()
        }
    }

    fn is_leaf_node(node: &ContextFreeNodeType) -> bool {
        match &node.kind {
            NodeTypeKind::Regular { fields, children } => fields.is_empty() && children.is_empty(),
            NodeTypeKind::Supertype { subtypes } => subtypes.is_empty(),
        }
    }

    pub fn generate_ddlog_file(&self, nodes: &[ContextFreeNodeType]) -> String {
        let mut output = String::new();
        let mut distinct_types = BTreeSet::new();
        for node in nodes {
            distinct_types.insert(node.name.sexp_name.clone());
            match &node.kind {
                NodeTypeKind::Supertype { subtypes } => {
                    for st in subtypes {
                        distinct_types.insert(st.sexp_name.clone());
                    }
                }
                NodeTypeKind::Regular { fields, children } => {
                    for spec in fields.values() {
                        for ctype in &spec.types {
                            distinct_types.insert(ctype.sexp_name.clone());
                        }
                    }
                    for ctype in &children.types {
                        distinct_types.insert(ctype.sexp_name.clone());
                    }
                }
            }
        }
        let mut is_leaf_map = BTreeMap::<String, bool>::new();
        for node in nodes {
            let leaf = Self::is_leaf_node(node);
            is_leaf_map.insert(node.name.sexp_name.clone(), leaf);
        }
        let mut main_relation_schemas = BTreeMap::<String, Vec<String>>::new();
        let mut link_relation_fields = BTreeMap::<String, BTreeSet<String>>::new();
        for tname in &distinct_types {
            let is_leaf = is_leaf_map.get(tname).copied().unwrap_or(false);
            if !is_leaf {
                let pascal = Self::to_pascal_case(tname);
                let rel_name = Self::sanitize_reserved(&pascal);
                main_relation_schemas.insert(rel_name.clone(), vec!["node_id: bigint".to_string()]);
                link_relation_fields.insert(rel_name, BTreeSet::new());
            }
        }
        for node in nodes {
            let parent_tname = &node.name.sexp_name;
            let parent_is_leaf = is_leaf_map.get(parent_tname).copied().unwrap_or(false);
            if parent_is_leaf {
                continue;
            }
            let parent_pascal = Self::to_pascal_case(parent_tname);
            let parent_rel = Self::sanitize_reserved(&parent_pascal);
            match &node.kind {
                NodeTypeKind::Supertype { subtypes } => {
                    if !subtypes.is_empty() {
                        link_relation_fields
                            .get_mut(&parent_rel)
                            .unwrap()
                            .insert("subtypes".to_string());
                    }
                }
                NodeTypeKind::Regular { fields, children } => {
                    for (field_name, child_spec) in fields {
                        let all_leaf = child_spec
                            .types
                            .iter()
                            .all(|ty| is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false));
                        if child_spec.multiple {
                            link_relation_fields
                                .get_mut(&parent_rel)
                                .unwrap()
                                .insert(field_name.clone());
                        } else {
                            let col_type = if all_leaf { "string" } else { "bigint" };
                            let safe_field_name = Self::sanitize_reserved(field_name);
                            let col_def = format!("{}: {}", safe_field_name, col_type);
                            main_relation_schemas
                                .get_mut(&parent_rel)
                                .unwrap()
                                .push(col_def);
                        }
                    }
                    if !children.types.is_empty() {
                        let all_leaf = children
                            .types
                            .iter()
                            .all(|ty| is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false));
                        if children.multiple {
                            link_relation_fields
                                .get_mut(&parent_rel)
                                .unwrap()
                                .insert("children".to_string());
                        } else {
                            let col_type = if all_leaf { "string" } else { "bigint" };
                            let col_def = format!("children: {}", col_type);
                            main_relation_schemas
                                .get_mut(&parent_rel)
                                .unwrap()
                                .push(col_def);
                        }
                    }
                }
            }
        }
        for (rel_name, columns) in &main_relation_schemas {
            writeln!(output, "input relation {}(", rel_name).unwrap();
            for (i, col) in columns.iter().enumerate() {
                if i < columns.len() - 1 {
                    writeln!(output, "    {},", col).unwrap();
                } else {
                    writeln!(output, "    {}", col).unwrap();
                }
            }
            writeln!(output, ")\n").unwrap();
        }
        for (rel_name, fields) in &link_relation_fields {
            for field_name in fields {
                let mut all_leaf = true;
                'outer: for node in nodes {
                    let node_parent_pascal = Self::to_pascal_case(&node.name.sexp_name);
                    let node_parent_rel = Self::sanitize_reserved(&node_parent_pascal);
                    if node_parent_rel == *rel_name {
                        match &node.kind {
                            NodeTypeKind::Supertype { subtypes } => {
                                if field_name == "subtypes" {
                                    for st in subtypes {
                                        if !is_leaf_map.get(&st.sexp_name).copied().unwrap_or(false)
                                        {
                                            all_leaf = false;
                                            break 'outer;
                                        }
                                    }
                                }
                            }
                            NodeTypeKind::Regular { fields, children } => {
                                if let Some(ch_spec) = fields.get(field_name) {
                                    let cstr = ch_spec.types.iter().all(|ty| {
                                        is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false)
                                    });
                                    if !cstr {
                                        all_leaf = false;
                                        break 'outer;
                                    }
                                }
                                if field_name == "children" {
                                    let cstr = children.types.iter().all(|ty| {
                                        is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false)
                                    });
                                    if !cstr {
                                        all_leaf = false;
                                        break 'outer;
                                    }
                                }
                            }
                        }
                    }
                }
                let sanitized_field = Self::sanitize_reserved(field_name);
                let link_name = format!("{}_{}", rel_name, sanitized_field);
                writeln!(output, "input relation {}(", link_name).unwrap();
                writeln!(output, "    parent_id: bigint,").unwrap();
                if all_leaf {
                    writeln!(output, "    value: string").unwrap();
                } else {
                    writeln!(output, "    child_id: bigint").unwrap();
                }
                writeln!(output, ")\n").unwrap();
            }
        }
        output
    }
}

#[derive(Debug)]
pub enum Error {
    GenerationError(String)
}

pub type Result<T> = std::result::Result<T, Error>;
pub type IrToDdlogResult = Result<Box<DatalogProgram>>;



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_end_to_end_single_capture() {
        use super::*;

        // Define a simple CaptureForm Lval
        let lval = Lval::CaptureForm(
            "TestNode".to_string(),
            HashMap::from([
                ("attr1".to_string(), Lval::num(42)),
                ("attr2".to_string(), Lval::sym("value")),
            ]),
            None, // No nested captures
            None, // No q-expressions
        );

        // Use the Compiler fluent API
        let compiler = DDlogGenerator::new().with_input_relations(true);
        let ddlog_program = compiler.compile(&lval);

        // Verify the DDlog output
        let expected_ddlog_output = r#"
        input relation TestNode(val: string);
        output relation TestNode_attr1(val: string);
        output relation TestNode_attr2(val: string);

        TestNode_attr1(var out_attr1 = attr1) :- TestNode(var attr1).
        TestNode_attr2(var out_attr2 = attr2) :- TestNode(var attr2).
    "#;

        assert_eq!(
            normalize_whitespace(&format!("{}", ddlog_program)),
            normalize_whitespace(expected_ddlog_output)
        );
    }

    #[test]
    fn test_output_relations_have_rules() {
        // Define a CaptureForm with a nested CaptureForm
        let lval = Lval::CaptureForm(
            "ParentNode".to_string(),
            HashMap::from([
                ("parent_attr1".to_string(), Lval::num(10)),
                ("parent_attr2".to_string(), Lval::sym("parent_value")),
            ]),
            Some(Box::new(Lval::CaptureForm(
                "ChildNode".to_string(),
                HashMap::from([
                    ("child_attr1".to_string(), Lval::num(20)),
                    ("child_attr2".to_string(), Lval::sym("child_value")),
                ]),
                None,
                None,
            ))),
            None,
        );

        // Create the Compiler and generate IR
        let compiler = DDlogGenerator::new();
        let ir_program = compiler.lval_to_ir(&lval);

        // Verify output relations
        let mut expected_relations = vec![
            "ParentNode_parent_attr1".to_string(),
            "ParentNode_parent_attr2".to_string(),
            "ChildNode_child_attr1".to_string(),
            "ChildNode_child_attr2".to_string(),
        ];
        let actual_relations = ir_program.relations.clone();

        // Sort both vectors for comparison
        expected_relations.sort();
        let mut actual_relations = actual_relations
            .into_iter()
            .map(|r| r.name)
            .collect::<Vec<_>>();
        actual_relations.sort();

        assert_eq!(actual_relations, expected_relations);

        // Verify rules
        assert_eq!(ir_program.rules.len(), expected_relations.len());
        for relation in &expected_relations {
            let (expected_node_type, expected_attr_name) =
                parse_relation_name(relation).expect("Invalid relation name format");
            assert!(ir_program.rules.iter().any(|rule| {
                rule.lhs.relation_name == *relation
                    && rule.rhs
                        == RHSVal::RHSNode(RHSNode {
                            relation_name: expected_node_type.clone(),
                            attributes: HashSet::from([expected_attr_name.clone()]),
                        })
            }));
        }
    }

    #[test]
    fn test_single_capture_form() {
        // Define a simple CaptureForm Lval
        let lval = Lval::CaptureForm(
            "TestNode".to_string(),
            HashMap::from([
                ("attr1".to_string(), Lval::num(42)),
                ("attr2".to_string(), Lval::sym("value")),
            ]),
            None, // No nested captures
            None, // No q-expressions
        );

        // Create the Compiler and generate IR
        let compiler = DDlogGenerator::new();
        let ir_program = compiler.lval_to_ir(&lval);

        // Verify output relations
        let expected_relations = vec!["TestNode_attr1".to_string(), "TestNode_attr2".to_string()];

        // Verify rules
        assert_eq!(ir_program.rules.len(), expected_relations.len());
        for relation in &expected_relations {
            let (expected_node_type, expected_attr_name) =
                parse_relation_name(relation).expect("Invalid relation name format");
            assert!(ir_program.rules.iter().any(|rule| {
                rule.lhs.relation_name == *relation
                    && rule.rhs
                        == RHSVal::RHSNode(RHSNode {
                            relation_name: expected_node_type.clone(),
                            attributes: HashSet::from([expected_attr_name.clone()]),
                        })
            }));
        }
    }

    #[test]
    fn test_nested_capture_forms() {
        // Define a CaptureForm with a nested CaptureForm
        let lval = Lval::CaptureForm(
            "ParentNode".to_string(),
            HashMap::from([
                ("parent_attr1".to_string(), Lval::num(10)),
                ("parent_attr2".to_string(), Lval::sym("parent_value")),
            ]),
            Some(Box::new(Lval::CaptureForm(
                "ChildNode".to_string(),
                HashMap::from([
                    ("child_attr1".to_string(), Lval::num(20)),
                    ("child_attr2".to_string(), Lval::sym("child_value")),
                ]),
                None,
                None,
            ))),
            None,
        );

        // Create the Compiler and generate IR
        let compiler = DDlogGenerator::new();
        let ir_program = compiler.lval_to_ir(&lval);

        // Verify output relations
        let mut expected_relations = vec![
            "ParentNode_parent_attr1".to_string(),
            "ParentNode_parent_attr2".to_string(),
            "ChildNode_child_attr1".to_string(),
            "ChildNode_child_attr2".to_string(),
        ];
        let mut actual_relations = ir_program.relations.clone();

        // Sort both vectors for comparison
        expected_relations.sort();
        let mut actual_relations = actual_relations
            .into_iter()
            .map(|r| r.name)
            .collect::<Vec<_>>();
        actual_relations.sort();

        assert_eq!(actual_relations, expected_relations);

        // Verify rules
        assert_eq!(ir_program.rules.len(), expected_relations.len());
        for relation in &expected_relations {
            let (expected_node_type, expected_attr_name) =
                parse_relation_name(relation).expect("Invalid relation name format");
            assert!(ir_program.rules.iter().any(|rule| {
                rule.lhs.relation_name == *relation
                    && rule.rhs
                        == RHSVal::RHSNode(RHSNode {
                            relation_name: expected_node_type.clone(),
                            attributes: HashSet::from([expected_attr_name.clone()]),
                        })
            }));
        }
    }

    #[test]
    fn test_end_to_end_single_capture() {
        // Start with an Lval for a single capture form
        let lval = Lval::CaptureForm(
            "TestNode".to_string(),
            HashMap::from([
                ("attr1".to_string(), Lval::num(42)),
                ("attr2".to_string(), Lval::sym("value")),
            ]),
            None, // No nested captures
            None, // No q-expressions
        );

        // Create the Compiler
        let compiler = DDlogGenerator::new();

        // Compile Lval to DDlog
        let ddlog_program = compiler.compile(&lval);

        // Verify the DDlog output as a block
        let expected_ddlog_output = r#"
            output relation TestNode_attr1(val: string);
            output relation TestNode_attr2(val: string);

            TestNode_attr1(var out_attr1 = attr1) :- TestNode(var attr1).
            TestNode_attr2(var out_attr2 = attr2) :- TestNode(var attr2).
        "#;

        let actual_ddlog_output = format!("{}", ddlog_program);
        assert_eq!(
            normalize_whitespace(&actual_ddlog_output),
            normalize_whitespace(expected_ddlog_output)
        );
    }

    /// Helper to parse a relation name into node type and attribute name
    fn parse_relation_name(relation: &str) -> Option<(String, String)> {
        let mut parts = relation.splitn(2, '_'); // Split into two parts: {node_type} and {attr_name}
        let node_type = parts.next()?.to_string();
        let attr_name = parts.next()?.to_string();
        Some((node_type, attr_name))
    }

    /// Utility function to normalize whitespace for easier comparison of generated output
    fn normalize_whitespace(input: &str) -> String {
        input
            .split_whitespace()
            .collect::<Vec<_>>()
            .join(" ")
            .trim()
            .to_string()
    }
}
