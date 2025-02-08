use crate::{dsl::Lval, parser::parse};

use ir::RelationRef;
use ir::{
    Attribute, AttributeType, IRProgram, IRRule, LHSNode, OperationType, RHSNode, RHSVal,
    RelationRole as IRRelationRole, RelationType as IRRelationType, SSAInstruction,
    SSAInstructionBlock, SSAOperation,
};

use core::fmt;
use indexmap::IndexMap;
use indexmap::IndexSet;
use language::node_types::{ContextFreeNodeType, NodeTypeKind};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Display;
use std::path::PathBuf;

const RESERVED_WORDS: &[&str] = &["String"];

#[derive(Debug, Clone)]
pub struct CaptureSymbol {
    name: String,
    relationref: RelationRef,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    // Maps capture name to its full symbol info
    captures: HashMap<String, CaptureSymbol>,
    // Maps relation names to sets of capture names for that relation
    relation_captures: HashMap<String, HashSet<String>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// Attempts to insert a new capture symbol.
    /// Returns Ok(()) if successful, or Err with description if the capture name is already taken.
    pub fn insert_capture(&mut self, capture_name: &str, relationref: RelationRef) -> Result<()> {
        // Check if capture name is already used in the same relation
        if let Some(existing_symbol) = self.captures.get(capture_name) {
            if existing_symbol.relationref.name == relationref.name {
                return Err(Error::ResolveError(format!(
                    "Capture name '{}' is already used in relation '{}'",
                    capture_name, relationref.name
                )));
            }
        }

        // Create new capture symbol
        let symbol = CaptureSymbol {
            name: capture_name.to_string(),
            relationref: relationref.clone(),
        };

        // Insert into global captures map
        self.captures.insert(capture_name.to_string(), symbol);

        // Add to relation-specific set
        self.relation_captures
            .entry(relationref.name.clone())
            .or_default()
            .insert(capture_name.to_string());

        Ok(())
    }

    /// Looks up a capture by name, returning its full symbol info if found
    pub fn lookup_capture(&self, capture_name: &str) -> Option<&CaptureSymbol> {
        self.captures.get(capture_name)
    }

    pub fn lookup_capture_for_relation(&self, relation_name: &str) -> Option<&CaptureSymbol> {
        self.relation_captures
            .get(relation_name)
            .and_then(|captures| captures.iter().next())
            .and_then(|capture_name| self.captures.get(capture_name))
    }

    /// Gets all captures for a specific relation
    pub fn get_relation_captures(&self, relation_name: &str) -> Vec<&CaptureSymbol> {
        self.relation_captures
            .get(relation_name)
            .map(|names| {
                names
                    .iter()
                    .filter_map(|name| self.captures.get(name))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Gets all captures for a specific relation role
    pub fn get_captures_by_role(&self, role: IRRelationRole) -> Vec<&CaptureSymbol> {
        self.captures
            .values()
            .filter(|symbol| symbol.relationref.role == role)
            .collect()
    }

    /// Gets all captures across all relations
    pub fn get_all_captures(&self) -> Vec<&CaptureSymbol> {
        self.captures.values().collect()
    }

    /// Gets the relation type for a given capture
    pub fn get_capture_relation(&self, capture_name: &str) -> Option<&RelationRef> {
        self.captures
            .get(capture_name)
            .map(|symbol| &symbol.relationref)
    }
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
    input_ts_grammars: Vec<PathBuf>, // Paths to input tree-sitter grammars
    intermediate_ts_grammars: Vec<PathBuf>, // Paths to intermediate/internal tree-sitter grammars
    symbol_table: SymbolTable,      // Track captures across relations
}

impl IrGenerator {
    /// Computes RHS values for a rule by combining input relations and intermediate relations
    /// Creates a new compiler with default options
    pub fn new() -> Self {
        Self {
            generate_input_relations: false, // Default to not generating input relations
            input_ts_grammars: Vec::new(),
            intermediate_ts_grammars: Vec::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    /// Add a tree-sitter grammar to generate from
    pub fn with_input_treesitter_grammar(mut self, input_ts_grammar: PathBuf) -> Self {
        self.input_ts_grammars.push(input_ts_grammar);
        self
    }

    pub fn with_input_treesitter_grammars(mut self, input_ts_grammars: Vec<PathBuf>) -> Self {
        self.input_ts_grammars.extend(input_ts_grammars);
        self
    }

    pub fn with_intermediate_treesitter_grammar(
        mut self,
        intermediate_ts_grammar: PathBuf,
    ) -> Self {
        self.intermediate_ts_grammars.push(intermediate_ts_grammar);
        self
    }

    pub fn with_intermediate_treesitter_grammars(
        mut self,
        intermediate_ts_grammars: Vec<PathBuf>,
    ) -> Self {
        self.intermediate_ts_grammars
            .extend(intermediate_ts_grammars);
        self
    }

    /// Enable or disable input relation generation
    pub fn with_input_relations(mut self, generate: bool) -> Self {
        self.generate_input_relations = generate;
        self
    }

    fn lookup_relation<'a>(
        ir: &'a IRProgram,
        rel_name: &str,
        rel_role: Option<IRRelationRole>,
    ) -> Option<&'a IRRelationType> {
        ir.relations.iter().find(|rel| {
            rel.name == rel_name && rel_role.as_ref().map(|r| *r == rel.role).unwrap_or(true)
        })
    }

    /// Converts an Lval AST into an IRProgram.
    pub fn lval_to_ir(&mut self, lval: &Lval) -> DslToIrResult {
        let mut ir_program = IRProgram {
            rules: Vec::new(),
            relations: Vec::new(),
        };

        // Add relations from tree-sitter grammars
        for grammar_path in &self.input_ts_grammars {
            let node_types_path = grammar_path.join("src/node-types.json");
            if node_types_path.exists() {
                let node_types: Vec<ContextFreeNodeType> =
                    std::fs::read_to_string(&node_types_path)
                        .map_err(|e| {
                            Error::GenerationError(format!("Failed to read node-types.json: {}", e))
                        })
                        .and_then(|content| {
                            serde_json::from_str(&content).map_err(|e| {
                                Error::GenerationError(format!(
                                    "Failed to parse node-types.json: {}",
                                    e
                                ))
                            })
                        })?;

                for node_type in node_types {
                    if !node_type.name.is_named {
                        continue;
                    }

                    // Skip node types with reserved keyword identifiers
                    let pascal_name = to_pascal_case(&node_type.name.sexp_name);
                    if RESERVED_WORDS.iter().any(|word| *word == pascal_name) {
                        println!(
                            "Skipping reserved word: {} (from {})",
                            pascal_name, node_type.name.sexp_name
                        );
                        continue;
                    }

                    let relation_name = pascal_name;
                    let mut attributes = IndexSet::from([
                        Attribute {
                            name: "node_id".to_string(),
                            attr_type: AttributeType::Number,
                        },
                        Attribute {
                            name: "parent_id".to_string(),
                            attr_type: AttributeType::Number,
                        },
                        Attribute {
                            name: "value".to_string(),
                            attr_type: AttributeType::String,
                        },
                    ]);

                    if let NodeTypeKind::Regular { fields, children } = &node_type.kind {
                        // Comment out the following line as the nodes on the TS AST
                        // do not include the extra fields specified in their node-types
                        // definition. Therefore, the AST node we get from TS will not have the
                        // expected shape and the insertion will fail.
                        //
                        // We're adopting for now a uniform shape for TS types - see above - namely :
                        // node_id: Number, parent_id: Number, value: String
                        /*
                        for field_name in fields.keys() {
                            attributes.insert(Attribute {
                                name: field_name.clone(),
                                attr_type: AttributeType::String,
                            });
                        }*/
                    }

                    ir_program.relations.push(IRRelationType {
                        name: relation_name,
                        attributes: attributes.into_iter().collect(),
                        role: IRRelationRole::Input,
                    });
                }
            }
        }

        let mut context = SSAContext::new();

        self.process_lval(lval, &mut ir_program, &mut context)?;

        Ok(Box::new(ir_program))
    }

    /// Recursively processes an Lval and populates the IRProgram.
    fn process_lval(
        &mut self,
        lval: &Lval,
        ir_program: &mut IRProgram,
        context: &mut SSAContext,
    ) -> Result<()> {
        //let mut descendant_output_rels = Vec::new();
        match lval {
            Lval::Rulebook(items) => {
                for item in items {
                    self.process_lval(item, ir_program, context)?;
                }
            }

            Lval::RulesBlock(rules) => {
                for rule in rules {
                    self.process_lval(rule, ir_program, context)?;
                }
            }

            /*
             *
             * Inference rules semantics
             *
             * The inference rule handling process manages the transformation of inference rules
             * into the intermediate representation (IR). The following rules and conventions
             * govern its behavior:
             *
             * 1. Relation Registration
             * ------------------------
             * - Creates an input relation for the inference rule if it doesn't exist
             * - Registers attributes based on the inference parameters
             *
             * 2. Rule Processing
             * -----------------
             * - Processes each inference path into IR rules
             * - Handles predicates and their arguments
             * - Manages computation blocks and their SSA transformations
             *
             */
            Lval::Inference(rel_name, params, inference_paths) => {
                let mut infer_rules = VecDeque::new();
                collect_rules_from_inference(lval, context, ir_program, &mut infer_rules)?;

                // iterate over the produced rules and register those in the IR program
                // TODO: predicate and computation handling
                for rule in infer_rules.iter() {
                    ir_program.rules.push(rule.clone());
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
            Lval::CaptureForm(
                rel_name,
                attrs_map,
                capture_refs,
                descendant,
                when_block,
                do_block,
            ) => {
                //
                // 1. Collect all capture rules in the current capture form and register
                //    the corresponding intermediary and internal relations.
                //
                let mut capture_rules = VecDeque::new();
                collect_rules_from_capture(
                    lval,
                    context,
                    ir_program,
                    &mut capture_rules,
                    &mut self.symbol_table,
                )?;

                // the first rule in the queue is the topmost capture rule.
                // We promote it from an internal relation to an intermediate one.
                if let Some(mut top_most_capture_rule) = capture_rules.pop_front() {
                    // fetch the internal relation associated with the top most capture rule
                    // and promote it to intermediate
                    if let Some(internal_relation) = ir_program.relations.iter_mut().find(|rel| {
                        rel.name == top_most_capture_rule.lhs.relation_name
                            && rel.role == IRRelationRole::Internal
                    }) {
                        internal_relation.role = IRRelationRole::Intermediate;
                    }

                    //
                    // 2. Make sure to satisfy the constraints expressed as predicates in the when block
                    //
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

                    // attach the instructions emitted for the constraints and qexprs to the top most capture rule
                    let ssa_block = if instructions.is_empty() {
                        None
                    } else {
                        Some(SSAInstructionBlock {
                            instructions: instructions.clone(),
                        })
                    };

                    top_most_capture_rule.ssa_block = ssa_block;

                    // Adding top most rule.
                    ir_program.rules.push(top_most_capture_rule);
                }
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
            Lval::Emit(rel_name, captures, when_block, do_block) => {
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
                    output_attributes: IndexSet::from(["val".to_string()]),
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

                let mut rhs_nodes = Vec::new();

                for capture_ref in captures {
                    let capture_name = &capture_ref[1..];

                    // Look up the capture's relation
                    let symbol =
                        self.symbol_table
                            .lookup_capture(capture_name)
                            .ok_or_else(|| {
                                Error::ResolveError(format!(
                                    "Referenced capture '{}' not found",
                                    capture_name
                                ))
                            })?;
                    let referenced_rel = &symbol.relationref.name;
                    // if we've already "hidrated" the intermediate relation, we can use it directly
                    let exists = rhs_nodes.iter().any(|rhs_node| {
                        if let RHSVal::RHSNode(rhs_node) = rhs_node {
                            rhs_node.relation_name == *referenced_rel
                        } else {
                            false
                        }
                    });
                    // if not, we need to look it up and add it to the body of the rule.
                    // Note: we're hydrating all the attributes of the intermediary relation, not
                    // only the captureref's ones.
                    if !exists {
                        if let Some(relation) = Self::lookup_relation(
                            ir_program,
                            referenced_rel,
                            Some(IRRelationRole::Intermediate),
                        ) {
                            let rhs_node = RHSNode {
                                relation_name: referenced_rel.clone(),
                                attributes: relation
                                    .attributes
                                    .iter()
                                    .cloned()
                                    .map(|s| format!("rhs_{}", s.name.to_lowercase()))
                                    .collect::<Vec<String>>(),
                            };
                            rhs_nodes.push(RHSVal::RHSNode(rhs_node));
                        } else {
                            return Err(Error::ResolveError(format!(
                                "Referenced relation '{}' not found",
                                referenced_rel
                            )));
                        }
                    }
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
                    rhs: RHSVal::NestedRHS(rhs_nodes),
                    ssa_block,
                };

                ir_program.rules.push(ir_rule);
            }
            _ => {
                // Handle other Lval variants as needed
                // For this transformation, other variants are ignored
            }
        }
        ///
        /// Walks through an Lval and collects all captures, generating corresponding IRRules
        /// that reference only input relations.
        fn collect_rules_from_capture(
            lval: &Lval,
            context: &mut SSAContext,
            ir_program: &mut IRProgram,
            capture_rules: &mut VecDeque<IRRule>,
            symbol_table: &mut SymbolTable,
        ) -> Result<()> {
            if let Lval::CaptureForm(
                rel_name,
                attrs_map,
                capture_refs,
                descendant,
                when,
                do_block,
            ) = lval
            {
                let mut outbound_attrs = VecDeque::new();
                if let Some(desc) = descendant {
                    // Fetch the rules emitted by out descendants. We'll use the LHS side of there
                    // rules to grab the attributes of our descendants that need to be exposed on
                    // our LHS side (in addition to our own ones)
                    collect_rules_from_capture(
                        desc,
                        context,
                        ir_program,
                        capture_rules,
                        symbol_table,
                    )?;

                    // get the upmost capture rule.
                    if let Some(last_capture_rule) = capture_rules
                        .iter()
                        .cloned()
                        .collect::<Vec<IRRule>>()
                        .first()
                    {
                        for attr_name in last_capture_rule.lhs.output_attributes.iter() {
                            let clean_name = attr_name.trim_start_matches("lhs_").to_string();

                            // Only add if it's referenced in our captures or capture_refs
                            let is_in_captures = attrs_map.values().any(|lval| {
                                if let Lval::Capture(capture_name) = lval.as_ref() {
                                    capture_name[1..].to_string() == clean_name
                                } else {
                                    false
                                }
                            });

                            let is_in_capture_refs = capture_refs
                                .iter()
                                .any(|cr| cr[1..].to_string() == clean_name);

                            if is_in_captures || is_in_capture_refs {
                                // HACK: We're assuming all the _id attributes are bigint
                                let attr_type = if clean_name.ends_with("_id") {
                                    AttributeType::Number
                                } else {
                                    AttributeType::String
                                };
                                outbound_attrs.push_front(Attribute {
                                    name: clean_name,
                                    attr_type,
                                });
                            }
                        }
                    }
                }
                // 1. there are no more descendants. Process current capture form.
                // collect the attributes that need to be exposed by the rule
                for (attr_name, lval_box) in attrs_map {
                    if let Lval::Capture(capture_name) = lval_box.as_ref() {
                        let capture_name = &capture_name[1..]; // Remove $ prefix

                        // Register the capture in the symbol table
                        if let Err(e) = symbol_table.insert_capture(
                            capture_name,
                            RelationRef::new(rel_name.clone(), IRRelationRole::Input),
                        ) {
                            panic!("Failed to insert capture: {}", e);
                        }

                        // HACK: We're assuming all the _id attributes are bigint
                        let attr_type = if attr_name.ends_with("_id") {
                            AttributeType::Number
                        } else {
                            AttributeType::String
                        };
                        outbound_attrs.push_front(Attribute {
                            name: capture_name.to_string(),
                            attr_type,
                        });
                    }
                }
                // 2. Merge any capture refs which also need to be exposed
                for capture_ref in capture_refs {
                    let capture_name = &capture_ref[1..]; // Remove $ prefix

                    // Verify the capture reference exists
                    if let Some(symbol) = symbol_table.lookup_capture(capture_name) {
                        let attr_type = if capture_name.ends_with("_id") {
                            AttributeType::Number
                        } else {
                            AttributeType::String
                        };
                        outbound_attrs.push_front(Attribute {
                            name: capture_name.to_string(),
                            attr_type,
                        });
                    } else {
                        return Err(Error::ResolveError(format!(
                            "Referenced capture '{}' not found",
                            capture_name
                        )));
                    }
                }

                // 3. Obtain the referenced input relation's attribute for the rule
                //    with it
                let input_relation = ir_program.relations.iter().find(|rel| {
                    rel.name == to_pascal_case(rel_name) && rel.role == IRRelationRole::Input
                });

                let mut inbound_attrs = Vec::new();
                for attr in input_relation.unwrap().attributes.iter() {
                    inbound_attrs.push(attr.name.clone());
                }

                // 4. Generate an RHS node. Only bind an inbound element (by using the
                //    'rhs_' prefix) if it meant to be exposed as an outbound attribute.
                //    Attributes that are not going to be exposed will be prefixed with
                //    'unused_' as a placeholder so that we won't violate the relation's schema.
                //
                let rhs_attrs = inbound_attrs
                    .iter()
                    .map(|in_rel_attr| {
                        if let Some(attr) = attrs_map.get(in_rel_attr) {
                            if let Lval::Capture(capture_name) = attr.as_ref() {
                                format!("rhs_{}", &capture_name[1..]) // Remove the $ prefix
                            } else {
                                // If it's not a capture, treat it as unused
                                format!("unused_{}", in_rel_attr)
                            }
                        } else {
                            // This attribute exists in input relation but is not referenced
                            format!("unused_{}", in_rel_attr)
                        }
                    })
                    .collect();
                //
                // 5. The RHS value of our rule will comprise our own plus all the RHS of our descendants.
                //    Therefore compute it.
                //
                let mut descendant_rhs_vals = vec![];
                for rule in capture_rules.iter() {
                    descendant_rhs_vals.push(rule.rhs.clone());
                }

                let parent_rhs_val = RHSVal::RHSNode(RHSNode {
                    relation_name: to_pascal_case(rel_name),
                    attributes: rhs_attrs,
                });

                //
                // 5.1 Combine all RHS values into a single NestedRHS value.
                //     This will be the RHS value of our rule.
                //     Note: for some yet unknown reason when the RHS includes the input relation
                //     more than one, we get a duplicated RHS.
                //     This is the workaround for this however the root cause would need to be
                //     addressed.
                let rhs_vals_with_lineage = {
                    let mut unique_rhs_vals = Vec::new();
                    unique_rhs_vals.push(parent_rhs_val);

                    for rhs_val in descendant_rhs_vals {
                        match rhs_val {
                            RHSVal::NestedRHS(nested_vals) => {
                                for val in nested_vals {
                                    if !unique_rhs_vals.contains(&val) {
                                        unique_rhs_vals.push(val);
                                    }
                                }
                            }
                            non_nested_val => {
                                if !unique_rhs_vals.contains(&non_nested_val) {
                                    unique_rhs_vals.push(non_nested_val);
                                }
                            }
                        }
                    }
                    RHSVal::NestedRHS(unique_rhs_vals)
                };

                // 6. Now that we have our RHS lineage, we can compute the LHS node.
                //    We have to build a relation which will be
                //    registered as an internal relation as only have visibility to
                //    consumers. The top level capture form will be the one
                //    that will create an intermediary relation comprising al the outbound
                //    attributes from the descendant internal relations.
                let internal_relation_name = {
                    // combine outbound attrs with capture refs
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

                // Deduplicate attributes for the relation
                let mut unique_relation_attrs = IndexSet::new();
                for attr in outbound_attrs.iter() {
                    unique_relation_attrs.insert(Attribute {
                        name: attr.name.clone(),
                        attr_type: attr.attr_type.clone(),
                    });
                }

                let internal_relation = IRRelationType {
                    name: internal_relation_name.clone(),
                    attributes: unique_relation_attrs.iter().cloned().collect(),
                    role: IRRelationRole::Internal,
                };

                ir_program.relations.push(internal_relation);

                // create a symbol table entry for each capture in the outbound attributes
                // of the internal relation. This is necessary to be able to refer to these
                // captures in the emit forms.
                for attr in unique_relation_attrs.iter() {
                    if let Err(e) = symbol_table.insert_capture(
                        &attr.name,
                        RelationRef::new(
                            internal_relation_name.clone(),
                            IRRelationRole::Intermediate,
                        ),
                    ) {
                        return Err(Error::SymbolTableError(format!(
                            "Failed to insert capture {} for internal relation {}: {}",
                            attr.name, internal_relation_name, e
                        )));
                    }
                }

                //
                // 6. Create the LHS node of our rule bound to the Internal Relation
                //
                // Validate all capture references before creating the LHS node
                for capture_ref in capture_refs {
                    let capture_name = &capture_ref[1..]; // Remove $ prefix
                    if symbol_table.lookup_capture(capture_name).is_none() {
                        return Err(Error::ResolveError(format!(
                            "Referenced capture '{}' not found",
                            capture_name
                        )));
                    }
                }

                let lhs_node = LHSNode {
                    relation_name: internal_relation_name,
                    output_attributes: outbound_attrs
                        .iter()
                        .map(|attr| format!("lhs_{}", attr.name.clone()))
                        .collect(),
                };

                //
                // 7. Finally, with our LHS, RHS and associated internal relation registered, we can
                //    create the rule.
                //    Rules will be added to the IR Program by the consumer of this function. As
                //    far as state is concerned, this function can only register internal
                //    relations which we can consider as disposable state.
                //
                let ir_rule = IRRule {
                    lhs: lhs_node,
                    rhs: rhs_vals_with_lineage,
                    ssa_block: None, // TODO: implement
                };

                capture_rules.push_front(ir_rule);
            }
            Ok(())
        }

        /// Maps inference predicates to SSA operations
        fn inference_predicates_to_ssa(
            predicates: &[Box<Lval>],
            context: &mut SSAContext,
        ) -> Vec<SSAInstruction> {
            let mut instructions = Vec::new();
            let mut operand_map = HashMap::new();

            let predicate_group_label = context.generate_label();
            instructions.push(SSAInstruction::Label(predicate_group_label.clone()));
            for predicate in predicates {
                if let Lval::Predicate(pred_name, arguments) = &**predicate {
                    // generate a label for the predicate
                    // Generate a unique temp var for this predicate
                    let result_var = context.generate_temp_var();

                    let op_type = match pred_name.as_str() {
                        // Comparison predicates
                        "equals" => OperationType::Eq,
                        "not_equals" => OperationType::Neq,
                        "less_than" => OperationType::Lt,
                        "less_equals" => OperationType::Leq,
                        "greater_than" => OperationType::Gt,
                        "greater_equals" => OperationType::Geq,

                        // String predicates
                        "contains" => OperationType::Contains,
                        "starts_with" => OperationType::StartsWith,
                        "ends_with" => OperationType::EndsWith,

                        // Collection predicates
                        "in" => OperationType::In,
                        "within" => OperationType::Within,

                        // Existence predicates
                        "exists" => OperationType::Exists,

                        // Logical predicates
                        "and" => OperationType::And,
                        "or" => OperationType::Or,
                        "not" => OperationType::Not,

                        // For unknown predicates, treat as a relation lookup
                        _ => {
                            // Store relation lookup result in temp var
                            let temp_var = context.generate_temp_var();
                            instructions.push(SSAInstruction::Assignment {
                                variable: temp_var.clone(),
                                operation: SSAOperation {
                                    op_type: OperationType::Load,
                                    operands: vec![pred_name.clone()],
                                },
                            });
                            operand_map.insert(result_var.clone(), temp_var);
                            continue;
                        }
                    };

                    // Process arguments into operands
                    let operands: Vec<String> = arguments
                        .iter()
                        .map(|arg| {
                            if arg.starts_with('?') {
                                // For variables, use as-is but remove ? prefix
                                arg[1..].to_string()
                            } else {
                                // For constants, create a temp var with Load operation
                                let temp_var = context.generate_temp_var();
                                instructions.push(SSAInstruction::Assignment {
                                    variable: temp_var.clone(),
                                    operation: SSAOperation {
                                        op_type: OperationType::Load,
                                        operands: vec![arg.clone()],
                                    },
                                });
                                temp_var
                            }
                        })
                        .collect();

                    // Add the predicate operation
                    instructions.push(SSAInstruction::Assignment {
                        variable: result_var.clone(),
                        operation: SSAOperation { op_type, operands },
                    });
                }
            }

            instructions
        }

        fn collect_rules_from_inference(
            lval: &Lval,
            context: &mut SSAContext,
            ir_program: &mut IRProgram,
            infer_rules: &mut VecDeque<IRRule>,
        ) -> Result<()> {
            if let Lval::Inference(relation_name, params, inference_paths) = lval {
                // Create the output relation if it doesn't exist
                if !ir_program
                    .relations
                    .iter()
                    .any(|r| r.name == *relation_name)
                {
                    // First collect all predicates to find input relations
                    let mut input_relations = HashMap::new();
                    for path in inference_paths {
                        if let Lval::InferencePath(predicates, _) = &**path {
                            for predicate in predicates {
                                if let Lval::Predicate(pred_name, _) = &**predicate {
                                    if let Some(rel) = ir_program
                                        .relations
                                        .iter()
                                        .find(|r| r.name == to_pascal_case(pred_name))
                                    {
                                        input_relations.insert(pred_name.clone(), rel);
                                    }
                                }
                            }
                        }
                    }

                    // Map parameters to their types based on input relations
                    let attributes = params
                        .iter()
                        .map(|p| {
                            let param_name = &p[1..];

                            // TODO: implement type annotations for parameters. 
                            // HACK: anything that has 'id' in it's name is a number, and the rest is string. 
                            let attr_type = if param_name.ends_with("_id") {
                                AttributeType::Number
                            } else {
                                AttributeType::String
                            };
                            Attribute {
                                name: param_name.to_string(),
                                attr_type
                            }
                        })
                        .collect();

                    let relation = IRRelationType {
                        name: to_pascal_case(relation_name),
                        attributes,
                        role: IRRelationRole::Output,
                    };
                    ir_program.relations.push(relation);
                }

                // Process each inference path
                for path in inference_paths {
                    if let Lval::InferencePath(predicates, computation) = &**path {
                        // Create RHS nodes and collect constraints from predicates
                        let mut rhs_nodes = Vec::new();
                        let mut instructions = Vec::new();

                        // Convert predicates to SSA instructions
                        let predicate_instructions =
                            inference_predicates_to_ssa(predicates, context);
                        instructions.extend(predicate_instructions);


                        // Add RHS nodes for relation lookups
                        for predicate in predicates {
                            if let Lval::Predicate(pred_name, arguments) = &**predicate {
                                // Only create RHS nodes for actual relations, not built-in predicates
                                if !matches!(
                                    pred_name.as_str(),
                                    "equals"
                                        | "not_equals"
                                        | "less_than"
                                        | "less_equals"
                                        | "greater_than"
                                        | "greater_equals"
                                        | "contains"
                                        | "starts_with"
                                        | "ends_with"
                                        | "in"
                                        | "within"
                                        | "exists"
                                        | "and"
                                        | "or"
                                        | "not"
                                ) {
                                    let rhs_node = RHSNode {
                                        relation_name: to_pascal_case(pred_name),
                                        attributes: arguments
                                            .iter()
                                            .map(|arg| {
                                                if arg.len() > 1 {
                                                    arg[1..].to_string()
                                                } else {
                                                    arg.to_string()
                                                }
                                            })
                                            .collect::<Vec<String>>(),
                                    };
                                    rhs_nodes.push(RHSVal::RHSNode(rhs_node));
                                }
                            }
                        }

                        // Add computation block instructions if present
                        if let Some(comp) = computation {
                            if let Lval::Computation(var_name, qexpr) = &**comp {
                                if let Lval::Qexpr(exprs) = &**qexpr {
                                    for expr in exprs {
                                        if let Lval::Sexpr(cells) = &**expr {
                                            if let Some(Lval::Sym(operation)) =
                                                cells.first().map(|v| &**v)
                                            {
                                                let operands: Vec<String> = cells
                                                    .iter()
                                                    .skip(1)
                                                    .map(|cell| match &**cell {
                                                        Lval::Sym(s) => s.clone(),
                                                        Lval::Capture(c) => {
                                                            format!("rhs_{}", &c[1..])
                                                        }
                                                        _ => "".to_string(),
                                                    })
                                                    .collect();

                                                let temp_var = context.generate_temp_var();
                                                instructions.push(SSAInstruction::Assignment {
                                                    variable: temp_var,
                                                    operation: SSAOperation {
                                                        op_type: OperationType::from(
                                                            operation.as_str(),
                                                        ),
                                                        operands,
                                                    },
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Create LHS node
                        let lhs_node = LHSNode {
                            relation_name: to_pascal_case(relation_name),
                            output_attributes: params.iter().map(|p| p[1..].to_string()).collect(),
                        };

                        // Create SSA block if we have instructions
                        let ssa_block = if !instructions.is_empty() {
                            Some(SSAInstructionBlock { instructions })
                        } else {
                            None
                        };

                        // Create and add the IR rule
                        let ir_rule = IRRule {
                            lhs: lhs_node,
                            rhs: RHSVal::NestedRHS(rhs_nodes),
                            ssa_block,
                        };

                        infer_rules.push_back(ir_rule);
                    }
                }
            }
            Ok(())
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
                                                        Lval::Capture(capture) => {
                                                            StringPart::Dynamic(format!(
                                                                "rhs_{}",
                                                                &capture[1..]
                                                            ))
                                                        }
                                                        Lval::String(s) => {
                                                            // strip quotes
                                                            let s = &s[1..s.len() - 1];
                                                            StringPart::Static(s.to_string())
                                                        }
                                                        e => unimplemented!(
                                                            "Unknown operand: {:?}",
                                                            e
                                                        ),
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
                o => panic!("Expected a logical operator {:?}", o),
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
                    Lval::Capture(capture) => {
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![format!("rhs_{}", capture[1..].to_string())],
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
                    Lval::Logical(predicate, operands) => {
                        let ssa_instructions = logical_to_ssa(predicate, operands, context);
                        instructions.extend(ssa_instructions.clone());
                        instructions.push(SSAInstruction::Label(context.generate_label()));
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

        Ok(())
    }

    pub fn generate(&mut self, dsl_program: &str) -> DslToIrResult {
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

impl Display for StringPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringPart::Static(s) => write!(f, "{}", s),
            StringPart::Dynamic(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    ParserError(crate::parser::Error),
    DivideByZero,
    EmptyList,
    FunctionFormat,
    NoChildren,
    NotANumber,
    NumArguments(usize, usize),
    Parse(String),
    Readline(String),
    WrongType(String, String),
    UnknownFunction(String),
    IoError(std::io::Error),
    GenerationError(String),
    ResolveError(String),
    SymbolTableError(String),
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::IoError(err)
    }
}

impl From<crate::parser::Error> for Error {
    fn from(err: crate::parser::Error) -> Self {
        Error::ParserError(err)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ParserError(e) => write!(f, "Parser error: {}", e),
            Error::DivideByZero => write!(f, "Divide by zero"),
            Error::EmptyList => write!(f, "Empty list"),
            Error::FunctionFormat => write!(
                f,
                "Function format invalid. Symbol '&' not followed by a single symbol"
            ),
            Error::NoChildren => write!(f, "Lval has no children"),
            Error::NotANumber => write!(f, "Not a number"),
            Error::NumArguments(expected, received) => write!(
                f,
                "Wrong number of arguments: expected {}, received {}",
                expected, received
            ),
            Error::Parse(s) => write!(f, "Parse error: {}", s),
            Error::Readline(s) => write!(f, "Readline error: {}", s),
            Error::WrongType(expected, received) => write!(
                f,
                "Wrong type: expected {}, received {}",
                expected, received
            ),
            Error::UnknownFunction(name) => write!(f, "Unknown function: {}", name),
            Error::IoError(e) => write!(f, "IO error: {}", e),
            Error::GenerationError(e) => write!(f, "Generation error: {}", e),
            Error::ResolveError(e) => write!(f, "Resolve error: {}", e),
            Error::SymbolTableError(e) => write!(f, "Symbol table error: {}", e),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
pub type DslToIrResult = Result<Box<IRProgram>>;

#[cfg(test)]
mod tests {
    #[test]
    fn test_symbol_table() {
        let mut table = SymbolTable::new();

        // Test successful insertions
        assert!(table.insert_capture("var1", "relation1").is_ok());
        assert!(table.insert_capture("var2", "relation1").is_ok());
        assert!(table.insert_capture("var3", "relation2").is_ok());

        // Test duplicate capture name
        assert!(table.insert_capture("var1", "relation2").is_err());

        // Test lookups
        let symbol = table.lookup_capture("var1").unwrap();
        assert_eq!(symbol.name, "var1");
        assert_eq!(symbol.relation_name, "relation1");

        // Test relation-specific lookups
        let rel1_captures = table.get_relation_captures("relation1");
        assert_eq!(rel1_captures.len(), 2);
        assert!(rel1_captures.iter().any(|s| s.name == "var1"));
        assert!(rel1_captures.iter().any(|s| s.name == "var2"));

        // Test all captures
        let all_captures = table.get_all_captures();
        assert_eq!(all_captures.len(), 3);
    }
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
