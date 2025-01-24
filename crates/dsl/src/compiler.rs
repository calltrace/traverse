use std::collections::{HashMap, HashSet};

use serde::de::Error;

use crate::{
    ast::Lval,
    ddlog_gen::{sanitize_reserved, to_pascal_case},
    ddlog_lang::{
        Atom, DType, DatalogProgram, Delay, Expr, ExprNode, Field, ModuleName, Pos, Relation,
        RelationRole, RelationSemantics, Rule, RuleLHS, RuleRHS,
    },
    ir::{
        Attribute, AttributeType, IRProgram, IRRule, LHSNode, OperationType, RHSNode, RHSVal,
        RelationRole as IRRelationRole, RelationType as IRRelationType, SSAInstruction,
        SSAInstructionBlock, SSAOperation,
    },
};

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

pub struct Compiler {
    generate_input_relations: bool, // Option to control input relation generation
}

impl Compiler {
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

    /// Converts an Lval AST into an IRProgram.
    pub fn lval_to_ir(&self, lval: &Lval) -> IRProgram {
        let mut ir_program = IRProgram {
            rules: Vec::new(),
            relations: Vec::new(),
        };

        let mut context = SSAContext::new();

        self.process_lval(lval, &mut ir_program, &mut context);

        ir_program
    }

    /// Recursively processes an Lval and populates the IRProgram.
    fn process_lval(&self, lval: &Lval, ir_program: &mut IRProgram, context: &mut SSAContext) {
        fn collect_outbound_attributes_from_descendants(lval: &Lval) -> Vec<Attribute> {
            let mut attrs = Vec::new();

            if let Lval::CaptureForm(_, attrs_map, desc, _) = lval {
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
            Lval::CaptureForm(rel_name, attrs_map, descendant, do_block) => {
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

                let mut outbound_attributes = Vec::new();
                for lval_box in attrs_map.values() {
                    if let Lval::Capture(capture_name) = lval_box.as_ref() {
                        outbound_attributes.push(Attribute {
                            name: capture_name[1..].to_string(),
                            attr_type: AttributeType::String,
                        });
                    }
                }

                let mut all_outbound_attrs = outbound_attributes.clone();
                for (_, attr_name) in descendant_output_rels.iter() {
                    all_outbound_attrs.push(Attribute {
                        name: attr_name.clone(),
                        attr_type: AttributeType::String,
                    });
                }

                ir_program.relations.push(IRRelationType {
                    name: to_pascal_case(
                        &outbound_attributes
                            .first()
                            .map(|attr| attr.name.clone())
                            .unwrap(),
                    ),
                    attributes: all_outbound_attrs.clone(),
                    role: IRRelationRole::Output,
                });

                if self.generate_input_relations {
                    ir_program.relations.push(IRRelationType {
                        name: to_pascal_case(rel_name),
                        attributes: inbound_attributes.clone(),
                        role: IRRelationRole::Input,
                    });
                }

                let lhs_node = LHSNode {
                    relation_name: to_pascal_case(
                        &outbound_attributes
                            .first()
                            .map(|attr| attr.name.clone())
                            .unwrap(),
                    ),
                    output_attributes: outbound_attributes
                        .iter()
                        .map(|attr| format!("lhs_{}", attr.name.clone()))
                        .collect(),
                };

                let mut rhs_vals = vec![RHSVal::RHSNode(RHSNode {
                    relation_name: to_pascal_case(rel_name),
                    attributes: inbound_attributes
                        .iter()
                        .map(|attr| format!("rhs_{}", attr.name.clone()))
                        .collect(),
                })];

                let descendant_rhs_vals = descendant_output_rels
                    .iter()
                    .map(|(rel_name, attr_name)| {
                        RHSVal::RHSNode(RHSNode {
                            relation_name: rel_name.clone(),
                            attributes: HashSet::from([format!("rhs_{}", attr_name.clone())]),
                        })
                    })
                    .collect::<Vec<_>>();

                rhs_vals.extend(descendant_rhs_vals);

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
                            Some(IRRelationRole::Output),
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

    /// Converts an IRProgram into a DatalogProgram, handling nested RHSVal structures.
    pub fn ir_to_ddlog(&self, ir: IRProgram) -> DatalogProgram {
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

        program
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
    ///
    /// Full pipeline: Convert Lval -> IRProgram -> DatalogProgram
    pub fn compile(&self, lval: &Lval) -> DatalogProgram {
        let ir_program = self.lval_to_ir(lval);
        self.ir_to_ddlog(ir_program)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
enum StringPart {
    Static(String),  // Static text
    Dynamic(String), // Placeholder (e.g., $var)
}

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
        let compiler = Compiler::new().with_input_relations(true);
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
        let compiler = Compiler::new();
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
        let compiler = Compiler::new();
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
        let compiler = Compiler::new();
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
        let compiler = Compiler::new();

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
