use indexmap::IndexSet;
use language::node_types::{ContextFreeNodeType, NodeTypeKind};
use std::collections::HashMap;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::path::PathBuf;

use crate::ddlog_lang::{
    Atom, DType, DatalogProgram, Delay, Expr, ExprNode, Field, ModuleName, Pos, Relation,
    RelationRole, RelationSemantics, Rule, RuleLHS, RuleRHS,
};

use ir::{
    Attribute, AttributeType, IRProgram, IRRule, LHSNode, Operand, OperationType, RHSNode, RHSVal,
    Reference, RelationRole as IRRelationRole, RelationType as IRRelationType, SSAInstruction,
    SSAInstructionBlock, SSAOperation,
};

const RESERVED_WORDS: &[&str] = &["else", "function", "type", "match", "var", "string", "String"];

enum RelationType {
    Input,
    Intermediate,
}

pub struct DDlogGenerator {
    generate_input_relations: bool, // Option to control input relation generation
    input_ts_grammars: Vec<PathBuf>, // Paths to input tree-sitter grammars
    intermediate_ts_grammars: Vec<PathBuf>, // Paths to intermediate/internal tree-sitter grammars
    embed_primitives: bool,         // Option to embed primitive types in the generated DDlog
}

impl DDlogGenerator {
    /// Creates a new compiler with default options
    pub fn new() -> Self {
        Self {
            generate_input_relations: false, // Default to not generating input relations
            input_ts_grammars: Vec::new(),   // Default to no parser homes
            intermediate_ts_grammars: Vec::new(), // Default to no intermediate grammars
            embed_primitives: false,
        }
    }

    /// Enable or disable input relation generation
    pub fn with_input_relations(mut self, generate: bool) -> Self {
        self.generate_input_relations = generate;
        self
    }

    /// Add a tree-sitter grammar to generate from
    pub fn with_input_treesitter_grammar(mut self, input_ts_grammar: PathBuf) -> Self {
        self.input_ts_grammars.push(input_ts_grammar);
        self
    }

    pub fn with_input_treesitter_grammars(mut self, input_ts_grammars: Vec<PathBuf>) -> Self {
        self.input_ts_grammars
            .extend(input_ts_grammars.into_iter().map(|h| h.into()));
        self
    }

    pub fn with_intermediate_treesitter_grammar(
        mut self,
        intermediate_ts_grammar: PathBuf,
    ) -> Self {
        self.intermediate_ts_grammars.push(intermediate_ts_grammar);
        self
    }

    /// Add multiple tree-sitter grammars to generate from
    pub fn with_intermediate_treesitter_grammars(
        mut self,
        intermediate_ts_grammars: Vec<PathBuf>,
    ) -> Self {
        self.intermediate_ts_grammars
            .extend(intermediate_ts_grammars.into_iter().map(|h| h.into()));
        self
    }

    pub fn embed_primitives(mut self) -> Self {
        self.embed_primitives = true;
        self
    }

    ///
    ///
    ///
    ///
    /// Converts an IRProgram into a DatalogProgram, handling nested RHSVal structures.
    pub fn generate(&self, ir: IRProgram) -> IrToDdlogResult {
        fn expand_rhs_val(val: &RHSVal) -> Vec<RuleRHS> {
            match val {
                RHSVal::RHSNode(node) => {
                    // Check if any attribute has a negation prefix and modify relation name accordingly
                    let has_negation = node.attributes.iter().any(|attr| {
                        if let Some(ref_name) = attr.strip_prefix('$') {
                            ref_name.starts_with("not_")
                        } else {
                            false
                        }
                    });

                    // Create a RHSLiteral referencing the input relation
                    let literal = RuleRHS::RHSLiteral {
                        pos: Pos::nopos(),
                        polarity: true,
                        atom: Atom {
                            pos: Pos::nopos(),
                            relation: if has_negation {
                                format!("Not{}", node.relation_name)
                            } else {
                                node.relation_name.clone()
                            },
                            delay: Delay::zero(),
                            diff: false,
                            value: Expr::new(ExprNode::EVar {
                                pos: Pos::nopos(),
                                name: node
                                    .attributes
                                    .iter()
                                    .cloned()
                                    .map(|s| {
                                        if s.starts_with("unused_") {
                                            "_".to_string()
                                        } else if let Some(ref_name) = s.strip_prefix('$') {
                                            // Remove the negation prefix if present
                                            let ref_name = ref_name.to_lowercase();
                                            if let Some(sanitized_ref_name) =
                                                ref_name.strip_prefix("not_")
                                            {
                                                sanitized_ref_name.to_string()
                                            } else {
                                                ref_name
                                            }
                                        } else {
                                            s.to_lowercase()
                                        }
                                    })
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

        let ddlog_home = std::env::var_os("DDLOG_HOME")
            .ok_or_else(|| Error::MissingEnvironmentVar("DDLOG_HOME".to_string()))?;
        let ddlog_path = std::path::Path::new(&ddlog_home);
        if !ddlog_path.exists() {
            return Err(Error::InvalidPath(format!(
                "DDLOG_HOME path does not exist: {}",
                ddlog_path.display()
            )));
        }
        if !ddlog_path.is_dir() {
            return Err(Error::InvalidPath(format!(
                "DDLOG_HOME is not a directory: {}",
                ddlog_path.display()
            )));
        }

        //
        // Collect all node types from all grammars
        let mut input_node_types = Vec::new();
        for parser_home in &self.input_ts_grammars {
            println!(
                "Processing parser home (input): {}",
                parser_home.to_string_lossy()
            );
            let node_types_path = format!("{}/src/node-types.json", parser_home.to_string_lossy());

            if !std::path::Path::new(&node_types_path).exists() {
                return Err(Error::InvalidPath(format!(
                    "node-types.json not found at {}",
                    node_types_path
                )));
            }

            let content = std::fs::read_to_string(&node_types_path).map_err(Error::IoError)?;
            let node_types: Vec<ContextFreeNodeType> =
                serde_json::from_str(&content).map_err(|e| Error::JsonParseError(e.to_string()))?;

            input_node_types.extend(node_types);
        }

        if input_node_types.is_empty() {
            eprintln!(
                "Warning: No input tree sitter grammars specified. Use with_treesitter_grammar()"
            );
        } else {
            eprintln!(
                "Loaded {} node types from {} grammars",
                input_node_types.len(),
                self.input_ts_grammars.len()
            );
        }

        let mut intermediate_node_types = Vec::new();
        for parser_home in &self.intermediate_ts_grammars {
            println!(
                "Processing parser home (intermediate): {}",
                parser_home.to_string_lossy()
            );
            let node_types_path = format!("{}/src/node-types.json", parser_home.to_string_lossy());
            if !std::path::Path::new(&node_types_path).exists() {
                return Err(Error::GenerationError(format!(
                    "node-types.json not found at {}",
                    node_types_path
                )));
            }
            let node_types: Vec<ContextFreeNodeType> = std::fs::read_to_string(&node_types_path)
                .map_err(|e| {
                    Error::GenerationError(format!("Failed to read node-types.json: {}", e))
                })
                .and_then(|content| {
                    serde_json::from_str(&content).map_err(|e| {
                        Error::GenerationError(format!("Failed to parse node-types.json: {}", e))
                    })
                })?;
            intermediate_node_types.extend(node_types);
        }

        let mut program = DatalogProgram::new();

        /* Note: This code was commented out as it has to be removed or refactored. We want relations to be defined in the DSL
        ONLY, not allow the IR to receive arbitrary relations from the DDlog generator.


        for input_ts_ir in self.generate_ddlog_relations(&input_node_types, RelationType::Input) {
            program.add_relation(input_ts_ir);
        }

        for intermediate_ts_ir in
            self.generate_ddlog_relations(&intermediate_node_types, RelationType::Intermediate)
        {
            program.add_relation(intermediate_ts_ir);
        }*/

        for relation in &ir.relations {
            if let Some(lhs_relation) = self.lookup_relation(&ir, &relation.name, None) {
                let relation_fields = lhs_relation
                    .attributes
                    .iter()
                    .map(|attr| {
                        let ftype = match &attr.attr_type {
                            AttributeType::String => DType::TString { pos: Pos::nopos() },
                            AttributeType::Number => DType::TInt { pos: Pos::nopos() },
                            AttributeType::Boolean => DType::TBool { pos: Pos::nopos() },
                            e => unimplemented!("Unsupported attribute type: {:?}", e),
                        };

                        Field {
                            pos: Pos::nopos(),
                            name: attr.name.clone(),
                            ftype,
                        }
                    })
                    .collect::<Vec<_>>();

                let sanitized_fields = relation_fields
                    .iter()
                    .cloned()
                    .map(|f| Field {
                        pos: f.pos.clone(),
                        name: Self::sanitize_reserved(&f.name),
                        ftype: f.ftype.clone(),
                    })
                    .collect::<Vec<Field>>();

                match relation.role {
                    // sanitize relation fields
                    IRRelationRole::Output => {
                        program.add_relation(Relation {
                            pos: Pos::nopos(),
                            role: RelationRole::RelOutput, // All are output relations
                            semantics: RelationSemantics::RelSet, // Defaulting to set semantics
                            name: relation.name.clone(),
                            rtype: DType::TStruct {
                                pos: Pos::nopos(),
                                name: relation.name.clone(),
                                fields: sanitized_fields,
                            }, // Assuming attributes are strings
                            primary_key: None,
                        });
                    }
                    IRRelationRole::Input => {
                        program.add_relation(Relation {
                            pos: Pos::nopos(),
                            role: RelationRole::RelInput,
                            semantics: RelationSemantics::RelSet,
                            name: relation.name.clone(),
                            rtype: DType::TStruct {
                                pos: Pos::nopos(),
                                name: relation.name.clone(),
                                fields: sanitized_fields,
                            }, // Assuming attributes are strings
                            primary_key: None,
                        });
                    }
                    IRRelationRole::Intermediate => {
                        program.add_relation(Relation {
                            pos: Pos::nopos(),
                            role: RelationRole::RelInternal,
                            semantics: RelationSemantics::RelSet,
                            name: relation.name.clone(),
                            rtype: DType::TStruct {
                                pos: Pos::nopos(),
                                name: relation.name.clone(),
                                fields: sanitized_fields,
                            }, // Assuming attributes are strings
                            primary_key: None,
                        });
                    }
                    IRRelationRole::Internal => {
                        // Internal relations don't have visibility behind the IR level
                    }
                }
            }
        }

        for rule in &ir.rules {
            if let Some(lhs_relation) = self.lookup_relation(&ir, &rule.lhs.relation_name, None) {
                let lhs_attributes = lhs_relation
                    .attributes
                    .iter()
                    .cloned()
                    .map(|s| s.name)
                    .collect::<IndexSet<_>>();

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
                                .map(|s| {
                                    if lhs_relation.role == IRRelationRole::Intermediate {
                                        format!("lhs_{}", s)
                                    } else {
                                        s
                                    }
                                })
                                .collect::<Vec<_>>()
                                .join(", "),
                        }),
                    },
                    location: None,
                };

                let mut rhs_clauses = expand_rhs_val(&rule.rhs);
                let rhs_attributes = self.collect_all_rhs_attributes(&ir, &rule.rhs);

                // Process SSA block to identify negations and modify relation names
                if let Some(ssa_block) = &rule.ssa_block {
                    // Build a map of variable assignments to track data flow
                    let mut var_assignments: HashMap<String, Operand> = HashMap::new();
                    let mut negated_vars: HashSet<String> = HashSet::new();

                    // First pass: collect negated variables and build assignment map
                    for instruction in &ssa_block.instructions {
                        if let SSAInstruction::Assignment {
                            variable,
                            operation,
                        } = instruction
                        {
                            match &operation.op_type {
                                OperationType::Not => {
                                    // Mark the output variable as negated
                                    negated_vars.insert(variable.clone());
                                    // Store the input operand for tracking
                                    if let Some(operand) = operation.operands.first() {
                                        var_assignments.insert(variable.clone(), operand.clone());
                                    }
                                }
                                OperationType::Load => {
                                    // Track load operations to map variables to their sources
                                    if let Some(operand) = operation.operands.first() {
                                        var_assignments.insert(variable.clone(), operand.clone());
                                    }
                                }
                                _ => {}
                            }
                        }
                    }

                    let mut negated_positions = HashSet::new();
                    for nv in negated_vars {
                        if let Some(va) = var_assignments.get(&nv) {
                            if let Operand::Reference(Reference::Named(name)) = va {
                                // resolve the variable given it's name so that we can obtain it's
                                // position within the RHS
                                var_assignments.get(name).map(|v| {
                                    if let Operand::Reference(Reference::Position(pos)) = v {
                                        negated_positions.insert(*pos);
                                    }
                                });
                            }
                        } else {
                            continue;
                        }
                    }

                    // Second pass: modify relation names for negated positions
                    if !negated_positions.is_empty() {
                        // enumerate clauses and tthen iterate over the negated position to see if
                        // there the index matches
                        for (i, clause) in rhs_clauses.iter_mut().enumerate() {
                            if let RuleRHS::RHSLiteral { atom, .. } = clause {
                                // Check if this clause corresponds to a negated position
                                if negated_positions.contains(&i) {
                                    atom.relation = format!("not {}", atom.relation);
                                }
                            }
                        }
                    }
                }

                if let Some(ssa_block) = &rule.ssa_block {
                    let mut negated_positions = HashSet::new();

                    // First pass: collect negated positions
                    for instruction in &ssa_block.instructions {
                        if let SSAInstruction::Assignment { operation, .. } = instruction {
                            if operation.op_type == OperationType::Not {
                                for operand in &operation.operands {
                                    if let Operand::Reference(Reference::Position(pos)) = operand {
                                        negated_positions.insert(*pos);
                                    }
                                }
                            }
                        }
                    }

                    // Second pass: modify relation names for negated positions
                    if !negated_positions.is_empty() {
                        for clause in &mut rhs_clauses {
                            if let RuleRHS::RHSLiteral { atom, .. } = clause {
                                // Check if this clause corresponds to a negated position
                                if let Ok(pos) = atom.value.to_string().parse::<usize>() {
                                    if negated_positions.contains(&pos) {
                                        atom.relation = format!("Not{}", atom.relation);
                                    }
                                }
                            }
                        }
                    }
                }

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
                    // Capture form and inferences
                    if let Some(ssa_block) = &rule.ssa_block {
                        // First process any labeled blocks for predicates
                        let condition_exprs =
                            Self::process_labeled_blocks_to_mapping_expr(ssa_block);

                        // Create a set to track processed attributes and conditions
                        let mut processed_attrs = HashSet::new();
                        let mut unique_conditions = HashSet::new();

                        // Add predicate conditions first
                        for condition_expr in condition_exprs {
                            // Extract attribute name from condition if it's a mapping
                            if let Some(attr_name) =
                                condition_expr.split('=').next().map(|s| s.trim())
                            {
                                if attr_name.starts_with("var lhs_") {
                                    let attr = attr_name.trim_start_matches("var lhs_");
                                    processed_attrs.insert(attr.to_string());
                                }
                            }

                            // Only add condition if we haven't seen it before
                            if unique_conditions.insert(condition_expr.clone()) {
                                conditions.push(RuleRHS::RHSCondition {
                                    pos: Pos::nopos(),
                                    expr: Expr::new(ExprNode::EVar {
                                        pos: Pos::nopos(),
                                        name: condition_expr,
                                    }),
                                });
                            }
                        }
                    }

                    // Intermediate relations are defined by captures so we want to make the
                    // RHS (body) to LHS (head) attributes one-to-one. However, output relations such as the ones for inference
                    // rules, mappings are determined by the user so we want to honor those.
                    //
                    if lhs_relation.role == IRRelationRole::Intermediate {
                        for lhs_attr in lhs_attributes.iter() {
                            if let Some(rhs_attr) = rhs_attributes
                                .iter()
                                .find(|r| r.starts_with("rhs_") && r[4..] == *lhs_attr)
                            {
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

        if self.embed_primitives {
            //
            let node_rel = {
                let fields = vec![
                    Field {
                        pos: Pos::nopos(),
                        name: "node_id".to_string(),
                        ftype: DType::TInt { pos: Pos::nopos() },
                    },
                    Field {
                        pos: Pos::nopos(),
                        name: "parent_id".to_string(),
                        ftype: DType::TInt { pos: Pos::nopos() },
                    },
                    Field {
                        pos: Pos::nopos(),
                        name: "value".to_string(),
                        ftype: DType::TString { pos: Pos::nopos() },
                    },
                ];
                Relation {
                    pos: Pos::nopos(),
                    role: RelationRole::RelOutput,
                    semantics: RelationSemantics::RelSet,
                    name: "Node".to_string(),
                    rtype: DType::TStruct {
                        pos: Pos::nopos(),
                        name: "Node".to_string(),
                        fields,
                    },
                    primary_key: None,
                }
            };

            let ancestor_rel = {
                let fields = vec![
                    Field {
                        pos: Pos::nopos(),
                        name: "node_id".to_string(),
                        ftype: DType::TInt { pos: Pos::nopos() },
                    },
                    Field {
                        pos: Pos::nopos(),
                        name: "ancestor_id".to_string(),
                        ftype: DType::TInt { pos: Pos::nopos() },
                    },
                ];
                Relation {
                    pos: Pos::nopos(),
                    role: RelationRole::RelOutput,
                    semantics: RelationSemantics::RelSet,
                    name: "Ancestor".to_string(),
                    rtype: DType::TStruct {
                        pos: Pos::nopos(),
                        name: "Ancestor".to_string(),
                        fields,
                    },
                    primary_key: None,
                }
            };
            program.add_relation(node_rel);
            program.add_relation(ancestor_rel);

            // Add base ancestor rule
            program.add_rule(Rule {
                pos: Pos::nopos(),
                module: ModuleName { path: vec![] },
                lhs: vec![RuleLHS {
                    pos: Pos::nopos(),
                    atom: Atom {
                        pos: Pos::nopos(),
                        relation: "Ancestor".to_string(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::new(ExprNode::EVar {
                            pos: Pos::nopos(),
                            name: "node_id, ancestor_id".to_string(),
                        }),
                    },
                    location: None,
                }],
                rhs: vec![RuleRHS::RHSLiteral {
                    pos: Pos::nopos(),
                    polarity: true,
                    atom: Atom {
                        pos: Pos::nopos(),
                        relation: "Node".to_string(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::new(ExprNode::EVar {
                            pos: Pos::nopos(),
                            name: "node_id, ancestor_id, _".to_string(),
                        }),
                    },
                }],
            });

            // Add transitive closure rule
            program.add_rule(Rule {
                pos: Pos::nopos(),
                module: ModuleName { path: vec![] },
                lhs: vec![RuleLHS {
                    pos: Pos::nopos(),
                    atom: Atom {
                        pos: Pos::nopos(),
                        relation: "Ancestor".to_string(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::new(ExprNode::EVar {
                            pos: Pos::nopos(),
                            name: "node_id, ancestor_id".to_string(),
                        }),
                    },
                    location: None,
                }],
                rhs: vec![
                    RuleRHS::RHSLiteral {
                        pos: Pos::nopos(),
                        polarity: true,
                        atom: Atom {
                            pos: Pos::nopos(),
                            relation: "Ancestor".to_string(),
                            delay: Delay::zero(),
                            diff: false,
                            value: Expr::new(ExprNode::EVar {
                                pos: Pos::nopos(),
                                name: "node_id, parent_id".to_string(),
                            }),
                        },
                    },
                    RuleRHS::RHSLiteral {
                        pos: Pos::nopos(),
                        polarity: true,
                        atom: Atom {
                            pos: Pos::nopos(),
                            relation: "Node".to_string(),
                            delay: Delay::zero(),
                            diff: false,
                            value: Expr::new(ExprNode::EVar {
                                pos: Pos::nopos(),
                                name: "parent_id, ancestor_id, _".to_string(),
                            }),
                        },
                    },
                ],
            });

            // Create Node rules for each tree-sitter node type
            for node_type in &input_node_types {
                if !Self::is_leaf_node(node_type) {
                    let pascal_name = Self::to_pascal_case(&node_type.name.sexp_name);
                    if !RESERVED_WORDS.iter().any(|w| w.eq_ignore_ascii_case(&pascal_name)) {
                        program.add_rule(Rule {
                            pos: Pos::nopos(),
                            module: ModuleName { path: vec![] },
                            lhs: vec![RuleLHS {
                                pos: Pos::nopos(),
                                atom: Atom {
                                    pos: Pos::nopos(),
                                    relation: "Node".to_string(),
                                    delay: Delay::zero(),
                                    diff: false,
                                    value: Expr::new(ExprNode::EVar {
                                        pos: Pos::nopos(),
                                        name: "node_id, parent_id, value".to_string(),
                                    }),
                                },
                                location: None,
                            }],
                            rhs: vec![RuleRHS::RHSLiteral {
                                pos: Pos::nopos(),
                                polarity: true,
                                atom: Atom {
                                    pos: Pos::nopos(),
                                    relation: pascal_name,
                                    delay: Delay::zero(),
                                    diff: false,
                                    value: Expr::new(ExprNode::EVar {
                                        pos: Pos::nopos(),
                                        name: "node_id, parent_id, value".to_string(),
                                    }),
                                },
                            }],
                        });
                    }
                }
            }
        }

        Ok(program.into())
    }

    fn collect_all_rhs_attributes(&self, ir: &IRProgram, val: &RHSVal) -> Vec<String> {
        match val {
            RHSVal::RHSNode(node) => {
                if self
                    .lookup_relation(ir, &node.relation_name, None)
                    .is_some()
                {
                    node.attributes.clone()
                } else {
                    Vec::new()
                }
            }
            RHSVal::NestedRHS(vals) => {
                let mut collected_attributes = Vec::new();
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
                    // Process previous block if it exists
                    if let Some(curr_label) = &current_label {
                        if let Some(expr) =
                            Self::process_block_for_predicate(curr_label, &current_instructions)
                        {
                            mapping_expressions.push(expr);
                        }
                    }
                    // Start new block
                    current_label = Some(label.clone());
                    current_instructions.clear();
                }
                _ => {
                    current_instructions.push(instruction.clone());
                }
            }
        }

        // Process final block
        if let Some(curr_label) = &current_label {
            if let Some(expr) = Self::process_block_for_predicate(curr_label, &current_instructions)
            {
                mapping_expressions.push(expr);
            }
        }

        mapping_expressions
    }

    fn process_block_for_predicate(
        label: &String,
        instructions: &[SSAInstruction],
    ) -> Option<String> {
        let mut operand_mapping: HashMap<String, String> = HashMap::new();
        let mut negation_refs: HashSet<String> = HashSet::new();
        let operator_mapping: HashMap<OperationType, String> = HashMap::from([
            // Comparison Operators
            (OperationType::Eq, "==".to_string()),
            (OperationType::Neq, "!=".to_string()),
            (OperationType::Lt, "<".to_string()),
            (OperationType::Leq, "<=".to_string()),
            (OperationType::Gt, ">".to_string()),
            (OperationType::Geq, ">=".to_string()),
            // Logical Operators
            (OperationType::And, "&&".to_string()),
            (OperationType::Or, "||".to_string()),
            (OperationType::Not, "!".to_string()),
            // String Functions
            (OperationType::Contains, "string_contains".to_string()),
            (OperationType::StartsWith, "string_starts_with".to_string()),
            (OperationType::EndsWith, "string_ends_with".to_string()),
            // Collection Operations
            (OperationType::In, "contains".to_string()),
            (OperationType::Within, "within".to_string()),
            // Existence Check
            (OperationType::Exists, "exists".to_string()),
            // Arithmetic Operators
            (OperationType::Add, "+".to_string()),
            (OperationType::Sub, "-".to_string()),
            (OperationType::Mul, "*".to_string()),
            (OperationType::Div, "/".to_string()),
            (OperationType::Mod, "%".to_string()),
            // String Operations
            (OperationType::Concat, "++".to_string()),
            (OperationType::Len, "string_length".to_string()),
        ]);

        // First pass: collect all operand mappings and identify relation references
        let mut relation_refs = HashSet::new();
        for instr in instructions {
            if let SSAInstruction::Assignment {
                variable,
                operation,
            } = instr
            {
                if operation.op_type == OperationType::Load {
                    if let Some(first_operand) = operation.operands.first() {
                        match first_operand {
                            Operand::Identifier(id) => {
                                operand_mapping.insert(variable.clone(), id.clone());
                            }
                            Operand::Reference(Reference::Named(name)) => {
                                let ref_name = if operation.op_type == OperationType::Not {
                                    format!("$not_{}", name)
                                } else {
                                    format!("${}", name)
                                };
                                operand_mapping.insert(variable.clone(), ref_name.clone());
                                relation_refs.insert(ref_name);
                                if operation.op_type == OperationType::Not {
                                    negation_refs.insert(name.clone());
                                }
                            }
                            Operand::Reference(Reference::Position(pos)) => {
                                let ref_name = if operation.op_type == OperationType::Not {
                                    format!("$not_{}", pos)
                                } else {
                                    format!("${}", pos)
                                };
                                operand_mapping.insert(variable.clone(), ref_name.clone());
                                relation_refs.insert(ref_name);
                                if operation.op_type == OperationType::Not {
                                    negation_refs.insert(pos.to_string());
                                }
                            }
                            Operand::StringLiteral(s) => {
                                operand_mapping.insert(variable.clone(), format!("\"{}\"", s));
                            }
                            Operand::NumberLiteral(n) => {
                                operand_mapping.insert(variable.clone(), n.to_string());
                            }
                        }
                    }
                }
            }
        }

        // Helper function to format operand
        let format_operand = |op: &Operand| -> String {
            match op {
                Operand::Identifier(id) => operand_mapping.get(id).unwrap_or(id).clone(),
                Operand::Reference(Reference::Named(name)) => {
                    if relation_refs.contains(name) {
                        format!("{}.{}", label, name) // Prefix with predicate name for relation refs
                    } else {
                        format!("rhs_{}", name)
                    }
                }
                Operand::Reference(Reference::Position(pos)) => {
                    let ref_name = format!("${}", pos);
                    if relation_refs.contains(&ref_name) {
                        format!("{}.{}", label, ref_name) // Prefix with predicate name for positional refs
                    } else {
                        ref_name
                    }
                }
                Operand::StringLiteral(s) => format!("\"{}\"", s),
                Operand::NumberLiteral(n) => n.to_string(),
            }
        };

        // Second pass: collect all predicate operations
        let mut expressions = Vec::new();
        for instr in instructions {
            if let SSAInstruction::Assignment {
                variable,
                operation,
            } = instr
            {
                if let Some(operator) = operator_mapping.get(&operation.op_type) {
                    let expr = match operation.operands.len() {
                        // Unary operators (not, len, etc)
                        1 => {
                            // for now we assume thatcan only be applied to
                            // input relations referenced on the RHS (body of the rule)
                            let operand = format_operand(&operation.operands[0]);
                            match operation.op_type {
                                OperationType::Not => None,
                                OperationType::Len => Some(format!("{}({})", operator, operand)),
                                _ => Some(format!("{}({})", operator, operand)),
                            }
                        }
                        // Binary operators (arithmetic, comparison, logical)
                        2 => {
                            let left = format_operand(&operation.operands[0]);
                            let right = format_operand(&operation.operands[1]);
                            match operation.op_type {
                                // Infix operators
                                OperationType::Add
                                | OperationType::Sub
                                | OperationType::Mul
                                | OperationType::Div
                                | OperationType::Mod
                                | OperationType::Eq
                                | OperationType::Neq
                                | OperationType::Lt
                                | OperationType::Leq
                                | OperationType::Gt
                                | OperationType::Geq
                                | OperationType::And
                                | OperationType::Or
                                | OperationType::Concat => {
                                    Some(format!("{} {} {}", left, operator, right))
                                }
                                // Function-style operators
                                _ => Some(format!("{}({}, {})", operator, left, right)),
                            }
                        }
                        // Variadic operators (contains, within, etc)
                        _ => {
                            let operands = operation
                                .operands
                                .iter()
                                .map(format_operand)
                                .collect::<Vec<_>>()
                                .join(", ");
                            Some(format!("{}({})", operator, operands))
                        }
                    };

                    if let Some(expr) = expr {
                        // For assignments, wrap in var declaration
                        if operation.op_type == OperationType::Load
                            || operation.op_type == OperationType::Concat
                            || operation.op_type == OperationType::Add
                        {
                            expressions.push(format!("var {} = {}", variable, expr));
                        } else {
                            expressions.push(expr);
                        }
                    }
                }
            }
        }

        if expressions.is_empty() {
            None
        } else {
            // Join all expressions with AND operator
            Some(expressions.join(" and "))
        }
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
                        .map(|operand| match operand {
                            Operand::Identifier(id) => resolve_operands(id, map),
                            Operand::Reference(ref_val) => match ref_val {
                                Reference::Named(name) => format!("rhs_{}", name),
                                Reference::Position(pos) => format!("${}", pos),
                            },
                            Operand::StringLiteral(s) => format!("\"{}\"", s),
                            Operand::NumberLiteral(n) => n.to_string(),
                        })
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
        !node.name.is_named
    }
}

#[derive(Debug)]
pub enum Error {
    GenerationError(String),
    IoError(std::io::Error),
    JsonParseError(String),
    MissingEnvironmentVar(String),
    InvalidPath(String),
    UnsupportedOperation(String),
    ValidationError(String),
    RelationNotFound(String),
    InvalidOperator(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::GenerationError(msg) => write!(f, "Generation error: {}", msg),
            Error::IoError(err) => write!(f, "IO error: {}", err),
            Error::JsonParseError(msg) => write!(f, "JSON parse error: {}", msg),
            Error::MissingEnvironmentVar(var) => write!(f, "Missing environment variable: {}", var),
            Error::InvalidPath(path) => write!(f, "Invalid path: {}", path),
            Error::UnsupportedOperation(op) => write!(f, "Unsupported operation: {}", op),
            Error::ValidationError(msg) => write!(f, "Validation error: {}", msg),
            Error::RelationNotFound(name) => write!(f, "Relation not found: {}", name),
            Error::InvalidOperator(op) => write!(f, "Invalid operator: {}", op),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::IoError(err) => Some(err),
            _ => None,
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::IoError(err)
    }
}

pub type Result<T> = std::result::Result<T, Error>;
pub type IrToDdlogResult = Result<Box<DatalogProgram>>;

#[cfg(test)]
mod tests {
    use super::*;

    use frontend::dsl::Lval;
    use frontend::gen_ir::IrGenerator;

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

        // convert to IR
        let irgen = IrGenerator::new();
        let ir_program = irgen.lval_to_ir(&lval);

        // Use the Compiler fluent API
        let to_ddlog = DDlogGenerator::new().with_input_relations(true);
        let ddlog_program = ir_program
            .map(|ir| to_ddlog.generate(*ir))
            .expect("Failed to generate DDlog");

        // Verify the DDlog output
        let expected_ddlog_output = r#"
        input relation TestNode(val: string);
        output relation TestNode_attr1(val: string);
        output relation TestNode_attr2(val: string);

        TestNode_attr1(var out_attr1 = attr1) :- TestNode(var attr1).
        TestNode_attr2(var out_attr2 = attr2) :- TestNode(var attr2).
    "#;

        ddlog_program
            .map(|ddlog| {
                assert_eq!(
                    normalize_whitespace(&format!("{}", ddlog)),
                    normalize_whitespace(expected_ddlog_output)
                );
            })
            .expect("Failed to generate DDlog");
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

        // Create IR generator and generate IR
        let irgen = IrGenerator::new();
        let ir_program = irgen.lval_to_ir(&lval).expect("IR generation failed");

        // Generate DDlog program
        let compiler = DDlogGenerator::new();
        let ddlog_program = compiler
            .generate(*ir_program)
            .expect("DDlog generation failed");

        // Verify each output relation has at least one matching rule
        let relation_names: HashSet<String> = ddlog_program
            .relations
            .iter()
            .filter(|(_, rel)| matches!(rel.role, RelationRole::RelOutput))
            .map(|(rname, rel)| rname.clone())
            .collect();

        assert!(!relation_names.is_empty(), "Should have output relations");

        for rule in &ddlog_program.rules {
            assert!(
                relation_names.contains(&rule.lhs[0].atom.relation),
                "Each rule should target one of our output relations"
            );
        }

        // There should be at least one rule per output relation
        for rel_name in relation_names {
            assert!(
                ddlog_program
                    .rules
                    .iter()
                    .any(|rule| rule.lhs[0].atom.relation == rel_name),
                "Output relation {} should have at least one rule",
                rel_name
            );
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

        // Create IR generator and generate IR
        let irgen = IrGenerator::new();
        let ir_program = irgen.lval_to_ir(&lval).expect("IR generation failed");

        // Generate DDlog program
        let compiler = DDlogGenerator::new();
        let ddlog_program = compiler
            .generate(*ir_program)
            .expect("DDlog generation failed");

        // Get output relations
        let output_relations: HashSet<String> = ddlog_program
            .relations
            .iter()
            .filter(|(_, rel)| matches!(rel.role, RelationRole::RelOutput))
            .map(|(rname, _)| rname.clone())
            .collect();

        // Verify expected relations exist
        let expected_relations = vec!["TestNode_attr1", "TestNode_attr2"];
        assert_eq!(
            output_relations.len(),
            expected_relations.len(),
            "Should have correct number of output relations"
        );

        for expected in &expected_relations {
            assert!(
                output_relations.contains(*expected),
                "Missing expected relation: {}",
                expected
            );
        }

        // Verify rules exist for each relation
        for relation in expected_relations {
            assert!(
                ddlog_program
                    .rules
                    .iter()
                    .any(|rule| rule.lhs[0].atom.relation == relation),
                "Missing rule for relation: {}",
                relation
            );
        }

        // Verify the DDlog output structure
        let expected_ddlog = normalize_whitespace(
            r#"
              output relation TestNode_attr1(val: string);
              output relation TestNode_attr2(val: string);
  
              TestNode_attr1(var out_attr1 = attr1) :- TestNode(var attr1).
              TestNode_attr2(var out_attr2 = attr2) :- TestNode(var attr2).
          "#,
        );

        assert_eq!(
            normalize_whitespace(&format!("{}", ddlog_program)),
            expected_ddlog,
            "DDlog output doesn't match expected structure"
        );
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

        // Create IR generator and generate IR
        let irgen = IrGenerator::new();
        let ir_program = irgen.lval_to_ir(&lval).expect("IR generation failed");

        // Generate DDlog program
        let compiler = DDlogGenerator::new();
        let ddlog_program = compiler
            .generate(*ir_program)
            .expect("DDlog generation failed");

        // Get output relations
        let output_relations: HashSet<String> = ddlog_program
            .relations
            .iter()
            .filter(|(_, rel)| matches!(rel.role, RelationRole::RelOutput))
            .map(|(rname, _)| rname.clone())
            .collect();

        // Expected relations for both parent and child nodes
        let expected_relations = vec![
            "ParentNode_parent_attr1",
            "ParentNode_parent_attr2",
            "ChildNode_child_attr1",
            "ChildNode_child_attr2",
        ];

        // Verify we have all expected relations
        assert_eq!(
            output_relations.len(),
            expected_relations.len(),
            "Should have correct number of output relations"
        );

        for expected in &expected_relations {
            assert!(
                output_relations.contains(*expected),
                "Missing expected relation: {}",
                expected
            );
        }

        // Verify rules exist for each relation
        for relation in expected_relations {
            assert!(
                ddlog_program
                    .rules
                    .iter()
                    .any(|rule| rule.lhs[0].atom.relation == relation),
                "Missing rule for relation: {}",
                relation
            );
        }

        // Verify the DDlog output structure
        let expected_ddlog = normalize_whitespace(
            r#"
              output relation ParentNode_parent_attr1(val: string);
              output relation ParentNode_parent_attr2(val: string);
              output relation ChildNode_child_attr1(val: string);
              output relation ChildNode_child_attr2(val: string);
  
              ParentNode_parent_attr1(var out_parent_attr1 = parent_attr1) :- ParentNode(var parent_attr1).
              ParentNode_parent_attr2(var out_parent_attr2 = parent_attr2) :- ParentNode(var parent_attr2).
              ChildNode_child_attr1(var out_child_attr1 = child_attr1) :- ChildNode(var child_attr1).
              ChildNode_child_attr2(var out_child_attr2 = child_attr2) :- ChildNode(var child_attr2).
          "#,
        );

        assert_eq!(
            normalize_whitespace(&format!("{}", ddlog_program)),
            expected_ddlog,
            "DDlog output doesn't match expected structure"
        );
    }

    #[test]
    fn test_end_to_end_single_capture() {
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

        // Create IR generator and generate IR
        let irgen = IrGenerator::new();
        let ir_program = irgen.lval_to_ir(&lval).expect("IR generation failed");

        // Generate DDlog program
        let compiler = DDlogGenerator::new();
        let ddlog_program = compiler
            .generate(*ir_program)
            .expect("DDlog generation failed");

        // Get output relations
        let output_relations: HashSet<String> = ddlog_program
            .relations
            .iter()
            .filter(|(_, rel)| matches!(rel.role, RelationRole::RelOutput))
            .map(|(rname, _)| rname.clone())
            .collect();

        // Expected relations
        let expected_relations = vec!["TestNode_attr1", "TestNode_attr2"];

        // Verify we have all expected relations
        assert_eq!(
            output_relations.len(),
            expected_relations.len(),
            "Should have correct number of output relations"
        );

        for expected in &expected_relations {
            assert!(
                output_relations.contains(*expected),
                "Missing expected relation: {}",
                expected
            );
        }

        // Verify rules exist for each relation
        for relation in expected_relations {
            assert!(
                ddlog_program
                    .rules
                    .iter()
                    .any(|rule| rule.lhs[0].atom.relation == relation),
                "Missing rule for relation: {}",
                relation
            );
        }

        // Verify the DDlog output structure
        let expected_ddlog = normalize_whitespace(
            r#"
              output relation TestNode_attr1(val: string);
              output relation TestNode_attr2(val: string);
  
              TestNode_attr1(var out_attr1 = attr1) :- TestNode(var attr1).
              TestNode_attr2(var out_attr2 = attr2) :- TestNode(var attr2).
          "#,
        );

        assert_eq!(
            normalize_whitespace(&format!("{}", ddlog_program)),
            expected_ddlog,
            "DDlog output doesn't match expected structure"
        );
    }

    fn normalize_whitespace(s: &str) -> String {
        s.split_whitespace().collect::<Vec<&str>>().join(" ")
    }
}
