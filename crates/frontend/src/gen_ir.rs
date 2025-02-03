use std::collections::{HashMap, HashSet};

use crate::{dsl::Lval, parser::parse};

use ir::{
    Attribute, AttributeType, IRProgram, IRRule, LHSNode, OperationType, RHSNode, RHSVal,
    RelationRole as IRRelationRole, RelationType as IRRelationType, SSAInstruction,
    SSAInstructionBlock, SSAOperation,
};

const RESERVED_WORDS: &[&str] = &["else", "function", "type", "match", "var"];

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

struct SSAContext {
    temp_counter: usize,
    label_counter: usize,
}

impl SSAContext {
    // Constructor to initialize the counters
    pub fn new() -> Self {
        SSAContext {
            temp_counter: 0,
            label_counter: 0,
        }
    }

    // Generate a temporary variable name
    pub fn generate_temp_var(&mut self) -> String {
        let temp_var = format!("t{}", self.temp_counter);
        self.temp_counter += 1;
        temp_var
    }

    // Generate a label
    pub fn generate_label(&mut self) -> String {
        let label = format!("L{}", self.label_counter);
        self.label_counter += 1;
        label
    }

    // Reset the counters if needed
    pub fn reset(&mut self) {
        self.temp_counter = 0;
        self.label_counter = 0;
    }
}

pub struct IrGenerator {
    generate_input_relations: bool, // Option to control input relation generation
}

impl IrGenerator {
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

    /// Converts an Lval AST into an IRProgram.
    pub fn lval_to_ir(&self, lval: &Lval) -> DslToIrResult {
        let mut ir_program = IRProgram {
            rules: Vec::new(),
            relations: Vec::new(),
        };

        let mut context = SSAContext::new();

        self.process_lval(lval, &mut ir_program, &mut context);

        Ok(Box::new(ir_program))
    }

    /// Recursively processes an Lval and populates the IRProgram.
    fn process_lval(&self, lval: &Lval, ir_program: &mut IRProgram, context: &mut SSAContext) {
        fn collect_outbound_attributes_from_descendants(lval: &Lval) -> Vec<Attribute> {
            let mut attrs = Vec::new();

            if let Lval::CaptureForm(_, attrs_map, capture_refs, desc, _) = lval {
                attrs_map.iter().for_each(|(_, lval_box)| {
                    if let Lval::Capture(capture_name) = lval_box.as_ref() {
                        attrs.push(Attribute {
                            name: capture_name[1..].to_string(),
                            attr_type: AttributeType::String,
                        });
                    }
                });
                if let Some(desc) = desc {
                    let descendant_attributes = collect_outbound_attributes_from_descendants(desc);
                    attrs.extend(descendant_attributes);
                }
            }

            attrs
        }

        let mut descendant_output_rels = Vec::new();
        match lval {
            Lval::Query(items) => {
                for item in items {
                    self.process_lval(item, ir_program, context);
                }
            }
            /*
             * Capture Form Semantics
             *
             * The capture form handling process is responsible for collecting, processing,
             * and transforming attributes and relations within the intermediate representation (IR).
             * The following rules and conventions govern its behavior and structure.
             *
             * 1. Attribute Collection
             * ------------------------
             * - Inbound attributes are collected from the input attributes map and represent
             *   the attributes available for input relations.
             * - Outbound attributes are derived from capture forms and represent the
             *   attributes produced as output relations.
             * - Descendant attributes are collected separately and integrated with outbound
             *   attributes to ensure complete representation of all relevant outputs.
             *
             * 2. Relation Construction
             * ------------------------
             * - Output relations are constructed using outbound and descendant attributes.
             * - Input relations are optionally generated from inbound attributes, depending
             *   on configuration.
             * - All relations are assigned specific roles (e.g., input or output) to clearly
             *   define their purpose within the system.
             *
             * 3. Transformation Rules
             * -----------------------
             * - Transformation rules are generated to map input relations to output
             *   relations.
             * - The Left-Hand Side (LHS) defines the output relation and its attributes.
             * - The Right-Hand Side (RHS) specifies the input relations and any additional
             *   attributes derived from descendants.
             * - These rules ensure consistent and structured transformations between
             *   relations.
             *
             * 4. Attribute Naming and Structure
             * ---------------------------------
             * - All attributes are assigned standardized names and types to maintain
             *   uniformity throughout the IR.
             * - Outbound attributes include contributions from descendants to ensure all
             *   relevant outputs are captured.
             *
             * 5. Invariants
             * -------------
             * - Every inbound attribute must correspond to a valid entry in the input
             *   attributes map.
             * - Outbound attributes must include all relevant descendant attributes.
             * - Relations must be categorized into well-defined roles (e.g., input or
             *   output).
             * - Transformation rules must correctly map all LHS and RHS attributes to
             *   preserve semantic consistency.
             */
            Lval::CaptureForm(rel_name, attrs_map, capture_refs, descendant, do_block) => {
                let descendant_outbound_attrs = if let Some(desc) = descendant {
                    // TODO: avoid double traversal
                    self.process_lval(desc, ir_program, context);
                    collect_outbound_attributes_from_descendants(desc)
                } else {
                    vec![]
                };

                for descendant_output_attr in descendant_outbound_attrs {
                    descendant_output_rels.push((
                        to_pascal_case(&descendant_output_attr.name),
                        descendant_output_attr.name.to_lowercase(),
                    ))
                }

                let mut inbound_attributes = Vec::new();
                attrs_map.iter().for_each(|(attr_name, _)| {
                    inbound_attributes.push(Attribute {
                        name: attr_name.clone(),
                        attr_type: AttributeType::String,
                    });
                });

                let mut outbound_attrs = Vec::new();
                for lval_box in attrs_map.values() {
                    if let Lval::Capture(capture_name) = lval_box.as_ref() {
                        outbound_attrs.push(Attribute {
                            name: capture_name[1..].to_string(),
                            attr_type: AttributeType::String,
                        });
                    }
                }

                /*
                let mut all_outbound_attrs = outbound_attributes.clone();
                for (_, attr_name) in descendant_output_rels.iter() {
                    all_outbound_attrs.push(Attribute {
                        name: attr_name.clone(),
                        attr_type: AttributeType::String,
                    });
                }
                */

                let intermediate_relation_name = {
                    let combined_attrs = outbound_attrs
                        .iter()
                        .map(|attr| attr.name.clone())
                        .collect::<Vec<_>>()
                        .join("_");

                    // Create a unique hash based on relation name and attributes
                    let hash_input = format!("{}_{}", rel_name, combined_attrs);
                    let hash = {
                        use std::hash::{Hash, Hasher};
                        let mut hasher = std::collections::hash_map::DefaultHasher::new();
                        hash_input.hash(&mut hasher);
                        format!("{:x}", hasher.finish())
                    };

                    // Take first 8 chars of hash
                    let short_hash = &hash[0..8];
                    format!("{}__{}", to_pascal_case(&combined_attrs), short_hash)
                };

                ir_program.relations.push(IRRelationType {
                    name: intermediate_relation_name.clone(),
                    attributes: outbound_attrs.clone(),
                    role: IRRelationRole::Intermediate,
                });

                if self.generate_input_relations {
                    ir_program.relations.push(IRRelationType {
                        name: to_pascal_case(rel_name),
                        attributes: inbound_attributes.clone(),
                        role: IRRelationRole::Input,
                    });
                }

                let lhs_node = LHSNode {
                    relation_name: intermediate_relation_name,
                    output_attributes: outbound_attrs
                        .iter()
                        .map(|attr| format!("lhs_{}", attr.name.clone()))
                        .collect(),
                };

                // compute RHS values
                // Input relations
                let mut rhs_vals = vec![RHSVal::RHSNode(RHSNode {
                    relation_name: to_pascal_case(rel_name),
                    attributes: inbound_attributes
                        .iter()
                        .map(|attr| format!("rhs_{}", attr.name.clone()))
                        .collect(),
                })];

                // Intermediate relations for any captures in the attributes map
                // extract captures from the attribute map
                let intermediate_rhs_vals = capture_refs
                    .iter()
                    .filter_map(|capture_ref_name| {
                        // find the relation within the IR Program that corresponds to the capture
                        ir_program
                            .relations
                            .iter()
                            .filter(|rel| rel.role == IRRelationRole::Intermediate)
                            .find(|relation| {
                                relation
                                    .attributes
                                    .iter()
                                    .any(|attr| attr.name == &capture_ref_name[1..])
                            })
                            .map(|relation| {
                                (relation.name.clone(), format!("rhs_{}", &capture_ref_name[1..]))
                            })
                    })
                    .fold(
                        HashMap::<String, HashSet<String>>::new(),
                        |mut acc: HashMap<String, HashSet<String>>, (rel_name, attr)| {
                            acc.entry(rel_name)
                                .or_insert_with(HashSet::new)
                                .insert(attr);
                            acc
                        },
                    )
                    .into_iter()
                    .map(|(relation_name, attributes)| {
                        RHSVal::RHSNode(RHSNode {
                            relation_name,
                            attributes,
                        })
                    })
                    .collect::<Vec<_>>();

                let descendant_rhs_vals = descendant_output_rels
                    .iter()
                    .map(|(rel_name, attr_name)| {
                        RHSVal::RHSNode(RHSNode {
                            relation_name: rel_name.clone(),
                            attributes: HashSet::from([format!("rhs_{}", attr_name.clone())]),
                        })
                    })
                    .collect::<Vec<_>>();

                rhs_vals.extend(intermediate_rhs_vals /*descendant_rhs_vals*/);

                let mut instructions: Vec<SSAInstruction> = vec![];
                if let Some(do_block) = do_block {
                    process_do_block(do_block, context, &mut instructions);
                }

                let ssa_block = if instructions.is_empty() {
                    None
                } else {
                    Some(SSAInstructionBlock {
                        instructions: instructions.clone(),
                    })
                };

                let ir_rule = IRRule {
                    lhs: lhs_node,
                    rhs: RHSVal::NestedRHS(rhs_vals),
                    ssa_block,
                };

                ir_program.rules.push(ir_rule);
            }
            /*
             * Emit Form Semantics
             *
             * The `emit` operation is a core component of the transformation system,
             * defining how data is captured, processed, and output. The following rules
             * and conventions govern the behavior and structure of `emit` forms to ensure
             * consistency and correctness.
             *
             * 1. Constraints on `emit` Operations
             * -----------------------------------
             * - Single Capture Reference:
             *   Each `emit` operation references exactly one capture. It cannot reference
             *   multiple captures or proceed without a reference.
             *   Example:
             *       Valid:   emit(CaptureA)
             *       Invalid: emit(CaptureA, CaptureB) or emit()
             *
             * - Output Value Structure:
             *   An `emit` operation can only produce an output relation that consists of
             *   one single string value. This output is intended to simplify downstream
             *   processing.
             *   Example:
             *       Valid:   EmitRelation("output_value")
             *       Invalid: EmitRelation("output_value1", "output_value2") or EmitRelation(123)
             *
             * 2. Clause Generation
             * --------------------
             * For every capture referenced by an `emit` operation, a corresponding clause
             * is automatically generated. This clause binds the capture to the
             * transformation logic, making it usable within the system.
             *
             * Key Properties:
             * - Each capture has exactly one associated clause.
             * - The clause ensures seamless integration of captures into the transformation
             *   pipeline.
             *
             * 3. Emit Output Relations
             * ------------------------
             * - Naming Convention:
             *   Emit output relations are always prefixed with `Emit`, clearly identifying
             *   them as aggregators and entry points for the output data.
             * - Role in the System:
             *   These relations act as top-level aggregators and serve as the primary
             *   output for the transformation process.
             *
             * Example:
             *   Valid Emit Output Relation: EmitResult
             *   Invalid Output Relation:    Result
             *
             * 4. Transformation Logic
             * -----------------------
             * - The logic associated with an `emit` operation, represented as a `QExpr`,
             *   must always be transformed into a valid DDLog expression.
             * - This transformation ensures that the logic operates correctly on the
             *   referenced capture.
             *
             * Invariants:
             * - Every `emit` operation must adhere to these constraints.
             * - Violations (e.g., multiple captures, invalid output structures, or
             *   transformation errors) must be detected and handled to maintain system
             *   integrity.
             */
            Lval::Emit(rel_name, attrs_map, when_block, do_block) => {
                let output_relation_name = format!("Emit{}", to_pascal_case(rel_name));
                ir_program.relations.push(IRRelationType {
                    name: output_relation_name.clone(),
                    attributes: vec![Attribute {
                        name: "val".to_string(),
                        attr_type: AttributeType::String,
                    }],
                    role: IRRelationRole::Output,
                });

                let lhs_node = LHSNode {
                    relation_name: output_relation_name,
                    output_attributes: HashSet::from(["val".to_string()]),
                };

                let mut instructions: Vec<SSAInstruction> = vec![];
                if let Some(logical) = when_block {
                    if let Lval::Logical(po, operands) = &**logical {
                        // Generate SSA instructions for logical operations
                        let ssa_instructions = logical_to_ssa(po, operands, context);
                        instructions.extend(ssa_instructions.clone());
                        instructions.push(SSAInstruction::Label(context.generate_label()));
                    }
                }

                if let Some(do_block) = do_block {
                    process_do_block(do_block, context, &mut instructions);
                }

                for capture in attrs_map.values() {
                    if let Lval::Capture(capture_name) = &**capture {
                        let referenced_rel = to_pascal_case(&capture_name[1..]);
                        if let Some(relation) = self.lookup_relation(
                            ir_program,
                            &referenced_rel,
                            Some(IRRelationRole::Intermediate),
                        ) {
                            let ssa_block = if instructions.is_empty() {
                                None
                            } else {
                                Some(SSAInstructionBlock {
                                    instructions: instructions.clone(),
                                })
                            };

                            let ir_rule = IRRule {
                                lhs: lhs_node.clone(),
                                rhs: RHSVal::RHSNode(RHSNode {
                                    relation_name: to_pascal_case(&referenced_rel),
                                    attributes: relation
                                        .attributes
                                        .iter()
                                        .cloned()
                                        .map(|s| s.name.to_lowercase())
                                        .collect::<HashSet<String>>(),
                                }),
                                ssa_block,
                            };

                            ir_program.rules.push(ir_rule);
                        }
                    }
                }
            }
            _ => {
                // Handle other Lval variants as needed
                // For this transformation, other variants are ignored
            }
        }

        fn process_do_block(
            do_block: &Lval,
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
        ) {
            if let Lval::DoForm(actions) = &do_block {
                for action in actions {
                    if let Lval::Qexpr(cells) = &**action {
                        for cell in cells {
                            match &**cell {
                                Lval::Sexpr(lvals) => {
                                    if let Some(Lval::Sym(operation)) = lvals.first().map(|v| &**v)
                                    {
                                        match operation.as_str() {
                                            "format" => {
                                                let operands = lvals
                                                    .iter()
                                                    .skip(1)
                                                    .map(|lval| match lval.as_ref() {
                                                        Lval::Sym(operand) => {
                                                            if let Some(first_char) =
                                                                operand.chars().next()
                                                            {
                                                                if first_char == '$' {
                                                                    StringPart::Dynamic(
                                                                        operand.clone(),
                                                                    )
                                                                } else {
                                                                    StringPart::Static(
                                                                        operand.clone(),
                                                                    )
                                                                }
                                                            } else {
                                                                StringPart::Static("".to_string())
                                                            }
                                                        }
                                                        _ => StringPart::Static("".to_string()),
                                                    })
                                                    .collect::<Vec<_>>();

                                                let mut last_var = String::new();

                                                for operand in operands {
                                                    let variable = context.generate_temp_var();

                                                    match operand {
                                                        StringPart::Static(text) => {
                                                            instructions.push(
                                                                SSAInstruction::Assignment {
                                                                    variable: variable.clone(),
                                                                    operation: SSAOperation {
                                                                        op_type:
                                                                            OperationType::Concat,
                                                                        operands: if last_var
                                                                            .is_empty()
                                                                        {
                                                                            vec![format!(
                                                                                "\"{}\"",
                                                                                text
                                                                            )]
                                                                        } else {
                                                                            vec![
                                                                                last_var.clone(),
                                                                                format!(
                                                                                    "\"{}\"",
                                                                                    text
                                                                                ),
                                                                            ]
                                                                        },
                                                                    },
                                                                },
                                                            );
                                                        }
                                                        StringPart::Dynamic(var) => {
                                                            instructions.push(
                                                                SSAInstruction::Assignment {
                                                                    variable: variable.clone(),
                                                                    operation: SSAOperation {
                                                                        op_type:
                                                                            OperationType::Concat,
                                                                        operands: if last_var
                                                                            .is_empty()
                                                                        {
                                                                            vec![var.clone()]
                                                                        } else {
                                                                            vec![
                                                                                last_var.clone(),
                                                                                var.clone(),
                                                                            ]
                                                                        },
                                                                    },
                                                                },
                                                            );
                                                        }
                                                    }

                                                    last_var = variable;
                                                }
                                            }
                                            _ => {
                                                println!("Unsupported operation: {}", operation);
                                            }
                                        }
                                    }
                                }
                                unsupported_cell => {
                                    println!("Unsupported QExpr cell type: {:?}", unsupported_cell);
                                }
                            }
                        }
                    }
                }
            }
        }

        /// Maps a logical operator and operands to SSAOperation, updating counters.
        fn logical_to_ssa(
            operator: &Lval,
            operands: &[Box<Lval>],
            context: &mut SSAContext,
        ) -> Vec<SSAInstruction> {
            let label = context.generate_label();
            //
            //
            //
            // Extract operator type
            let op_type = match operator {
                Lval::PredicateOperator(op) => match op.as_str() {
                    // Logical Operators
                    "and" => OperationType::And,
                    "or" => OperationType::Or,
                    "not" => OperationType::Not,

                    // Comparison Operators
                    "eq" => OperationType::Eq,
                    "neq" => OperationType::Neq,
                    "lt" => OperationType::Lt,
                    "leq" => OperationType::Leq,
                    "gt" => OperationType::Gt,
                    "geq" => OperationType::Geq,

                    // Membership Operators
                    "in" => OperationType::In,
                    "within" => OperationType::Within,

                    // Null/Existence Checks
                    "exists" => OperationType::Exists,

                    // String Comparisons
                    "contains" => OperationType::Contains,
                    "startswith" => OperationType::StartsWith,
                    "endswith" => OperationType::EndsWith,
                    _ => panic!("Unknown logical operator: {}", op),
                },
                _ => panic!("Expected a logical operator."),
            };

            let mut instructions = Vec::new();

            instructions.push(SSAInstruction::Label(label.clone()));
            // Extract operand variables and generate instructions
            let mut operand_vars = Vec::new();
            for operand in operands {
                let temp_var = context.generate_temp_var();
                match operand.as_ref() {
                    Lval::Sym(sym) => {
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![sym.clone()],
                            },
                        });
                    }
                    Lval::Num(num) => {
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![num.to_string()],
                            },
                        });
                    }
                    Lval::String(s) => {
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![s.clone()],
                            },
                        });
                    }
                    _ => {
                        panic!(
                            "Unsupported operand type for logical operator: {:?}",
                            operand
                        );
                    }
                }

                operand_vars.push(temp_var);
            }
            //
            // Create the logical/comparison instruction
            let result_var = context.generate_temp_var();
            instructions.push(SSAInstruction::Assignment {
                variable: result_var.clone(),
                operation: SSAOperation {
                    op_type, // Use the extracted operator type (e.g., Eq, And, etc.)
                    operands: operand_vars,
                },
            });
            instructions
        }
    }

    pub fn generate(&self, dsl_program: &str) -> DslToIrResult {
        parse(dsl_program)
            .map_err(Error::ParserError)
            .and_then(|lval| self.lval_to_ir(&lval))
    }
}

impl Default for IrGenerator {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
enum StringPart {
    Static(String),  // Static text
    Dynamic(String), // Placeholder (e.g., $var)
}

#[derive(Debug)]
pub enum Error {
    ParserError(crate::parser::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
pub type DslToIrResult = Result<Box<IRProgram>>;

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
        let compiler = IrGenerator::new().with_input_relations(true);
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
        let compiler = IrGenerator::new();
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
        let compiler = IrGenerator::new();
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
        let compiler = IrGenerator::new();
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
        let compiler = IrGenerator::new();

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
