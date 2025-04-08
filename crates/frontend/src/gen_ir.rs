use crate::dsl::ProvenanceType;
use crate::{dsl::Lval, parser::parse};

use ir::{
    Attribute, AttributeType, IRProgram, IRRule, LHSNode, OperationType, RHSNode, RHSVal,
    RelationRole as IRRelationRole, RelationType as IRRelationType, SSAInstruction,
    SSAInstructionBlock, SSAOperation,
};
use ir::{Operand, Reference, RelationRef};

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
        self.captures
            .insert(capture_name.to_string(), symbol.clone());

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

    /// Looks up a capture by name and relation role, returning its full symbol info if found
    pub fn lookup_capture_by_role(
        &self,
        capture_name: &str,
        role: IRRelationRole,
    ) -> Option<&CaptureSymbol> {
        self.captures
            .get(capture_name)
            .filter(|symbol| symbol.relationref.role == role)
    }

    /// Looks up a capture by name and relation name, returning its full symbol info if found
    pub fn lookup_capture_by_relation(
        &self,
        capture_name: &str,
        relation_name: &str,
    ) -> Option<&CaptureSymbol> {
        self.captures
            .get(capture_name)
            .filter(|symbol| symbol.relationref.name == relation_name)
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

pub(crate) fn normalize_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len() * 2);
    let chars = s.chars().peekable();

    for c in chars {
        if c == '_' {
            result.push('_');
        } else if c.is_uppercase() {
            if !result.is_empty() && !result.ends_with('_') {
                result.push('_');
            }
            result.push(c.to_ascii_lowercase());
        } else {
            result.push(c.to_ascii_lowercase());
        }
    }
    result
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

#[derive(Debug)]
struct CaptureMapping {
    relation_name: String,
    symbol: String,
    alias: String,
}

struct SSAContext {
    temp_counter: usize,
    label_counter: usize,
    // Maps capture name to a vector of mappings since a capture can have multiple mappings
    capture_mappings: HashMap<String, Vec<CaptureMapping>>,
}

impl SSAContext {
    // Constructor to initialize the counters
    pub fn new() -> Self {
        SSAContext {
            temp_counter: 0,
            label_counter: 0,
            capture_mappings: HashMap::new(),
        }
    }

    // Add a new capture mapping
    pub fn add_capture_mapping(
        &mut self,
        capture_name: String,
        relation_name: String,
        symbol: String,
    ) {
        self.capture_mappings
            .entry(capture_name.clone())
            .or_default()
            .push(CaptureMapping {
                relation_name,
                symbol: symbol.clone(),
                alias: normalize_string(&capture_name),
            });
    }

    // Look up all capture mappings for a given capture name
    pub fn get_capture_mappings(&self, capture_name: &str) -> Option<&Vec<CaptureMapping>> {
        self.capture_mappings.get(capture_name)
    }

    // Reset capture mappings if needed
    pub fn clear_capture_mappings(&mut self) {
        self.capture_mappings.clear();
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
        self.capture_mappings.clear();
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
                        category: Some(ir::RelationCategory::Internal),
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
                        process_do_block(do_block, context, &mut instructions, &self.symbol_table)?
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

                // Collect all attributes including provenance from referenced captures
                let mut relation_attributes = IndexSet::new();
                relation_attributes.insert(Attribute {
                    name: "val".to_string(),
                    attr_type: AttributeType::String,
                });

                // Add attributes for all captures (base + provenance) to the relation definition
                for capture_ref in captures {
                    if let Lval::Capture(capture_name, provenance_type) = &**capture_ref {
                        // Add the base attribute for the capture itself
                        if let Some(capture_symbol) = self.symbol_table.lookup_capture(capture_name)
                        {
                            if let Some(original_relation) = Self::lookup_relation(
                                ir_program,
                                &capture_symbol.relationref.name,
                                None,
                            ) {
                                // Find the specific attribute mapping in SSA context
                                if let Some(mappings) = context.get_capture_mappings(capture_name) {
                                    if let Some(mapping) = mappings.first() { // Use the first mapping
                                        if let Some(original_attr) = original_relation
                                            .attributes
                                            .iter()
                                            .find(|a| a.name == mapping.symbol)
                                        {
                                            relation_attributes.insert(Attribute {
                                                name: normalize_string(capture_name), // Use capture name for the attribute
                                                attr_type: original_attr.attr_type.clone(),
                                            });
                                        }
                                    }
                                }
                            }
                        }

                        // Add provenance attributes if specified
                        if let Some(mappings) = context.get_capture_mappings(capture_name) {
                            for mapping in mappings {
                                match provenance_type {
                                    Some(ProvenanceType::Path) => {
                                        relation_attributes.insert(Attribute {
                                            name: format!(
                                                "{}_path",
                                                normalize_string(&mapping.symbol)
                                            ),
                                            attr_type: AttributeType::String,
                                        });
                                    }
                                    Some(ProvenanceType::Downstream) => {
                                        relation_attributes.insert(Attribute {
                                            name: format!(
                                                "{}_downstream",
                                                normalize_string(&mapping.symbol)
                                            ),
                                            attr_type: AttributeType::String,
                                        });
                                    }
                                    Some(ProvenanceType::Dependency) => {
                                        relation_attributes.insert(Attribute {
                                            name: format!(
                                                "{}_dependency",
                                                normalize_string(&mapping.symbol)
                                            ),
                                            attr_type: AttributeType::String,
                                        });
                                    }
                                    Some(ProvenanceType::Upstream) => {
                                        relation_attributes.insert(Attribute {
                                            name: format!(
                                                "{}_upstream",
                                                normalize_string(&mapping.symbol)
                                            ),
                                            attr_type: AttributeType::String,
                                        });
                                    }
                                    Some(ProvenanceType::Span) => {
                                        relation_attributes.insert(Attribute {
                                            name: format!(
                                                "{}_span",
                                                normalize_string(&mapping.symbol)
                                            ),
                                            attr_type: AttributeType::String,
                                        });
                                    }
                                    Some(ProvenanceType::Full) => {
                                        relation_attributes.insert(Attribute {
                                            name: format!(
                                                "{}_path",
                                                normalize_string(&mapping.symbol)
                                            ),
                                            attr_type: AttributeType::String,
                                        });
                                        relation_attributes.insert(Attribute {
                                            name: format!(
                                                "{}_span",
                                                normalize_string(&mapping.symbol)
                                            ),
                                            attr_type: AttributeType::String,
                                        });
                                    }
                                    Some(ProvenanceType::Default) | None => {
                                        // No provenance attributes needed
                                    }
                                }
                            }
                        }
                    }
                }

                if captures.is_empty() {
                    return Err(Error::GenerationError(
                        format!("Emit operation '{}' needs to have at least one associated capture reference", rel_name)
                            .to_string(),
                    ));
                }

                // check that at least one of the defined capture references has an associated
                // provenance
                /*
                captures.iter()
                    .find_map(|c| match c.as_ref() {
                        Lval::Capture(_, provenance_type) => provenance_type.clone(),
                        _ => None
                    })
                    .ok_or_else(|| {
                        Error::GenerationError(
                            format!("Emit operation '{}' needs to have at least one associated capture reference with provenance", rel_name)
                                .to_string()
                        )
                    })?;
                 */

                // Find and update existing relation if it exists, otherwise create a new one
                let relation_exists = ir_program
                    .relations
                    .iter()
                    .position(|r| r.name == output_relation_name);
                if let Some(index) = relation_exists {
                    // Update existing relation with additional attributes
                    let existing_attributes = &mut ir_program.relations[index].attributes;
                    for attr in relation_attributes {
                        if !existing_attributes.contains(&attr) {
                            existing_attributes.push(attr);
                        }
                    }
                } else {
                    // Create new relation
                    ir_program.relations.push(IRRelationType {
                        name: output_relation_name.clone(),
                        attributes: relation_attributes.into_iter().collect(),
                        role: IRRelationRole::Output,
                        category: Some(ir::RelationCategory::Structural),
                    });
                }

                // Create LHS attributes including both regular and provenance fields
                let mut lhs_attributes = IndexSet::new();
                lhs_attributes.insert("val".to_string());

                // For each capture, add its base attribute and any specified provenance attributes
                for capture_ref in captures {
                    if let Lval::Capture(capture_name, provenance_type) = &**capture_ref {
                        // Add the base attribute for the capture
                        lhs_attributes.insert(format!("lhs_{}", normalize_string(capture_name)));

                        // Look up the specific attribute mapping for this capture for provenance
                        if let Some(mappings) = context.get_capture_mappings(capture_name) {
                            // Use only the first mapping for each capture to avoid duplicates in provenance
                            if let Some(mapping) = mappings.first() {
                                // Add provenance attributes if specified
                                match provenance_type {
                                    Some(ProvenanceType::Path) => {
                                        lhs_attributes.insert(format!(
                                            "lhs_{}_path",
                                            normalize_string(&mapping.symbol)
                                        ));
                                    }
                                    Some(ProvenanceType::Downstream) => {
                                        lhs_attributes.insert(format!(
                                            "lhs_{}_downstream",
                                            normalize_string(&mapping.symbol)
                                        ));
                                    }
                                    Some(ProvenanceType::Dependency) => {
                                        lhs_attributes.insert(format!(
                                            "lhs_{}_dependency",
                                            normalize_string(&mapping.symbol)
                                        ));
                                    }
                                    Some(ProvenanceType::Upstream) => {
                                        lhs_attributes.insert(format!(
                                            "lhs_{}_upstream",
                                            normalize_string(&mapping.symbol)
                                        ));
                                    }
                                    Some(ProvenanceType::Span) => {
                                        lhs_attributes.insert(format!(
                                            "lhs_{}_span",
                                            normalize_string(&mapping.symbol)
                                        ));
                                    }
                                    Some(ProvenanceType::Full) => {
                                        lhs_attributes.insert(format!(
                                            "lhs_{}_path",
                                            normalize_string(&mapping.symbol)
                                        ));
                                        lhs_attributes.insert(format!(
                                            "lhs_{}_span",
                                            normalize_string(&mapping.symbol)
                                        ));
                                    }
                                    Some(ProvenanceType::Default) | None => {
                                        // by default or in case provenance is not
                                        // specified, we do not update our rule's head
                                    }
                                }
                            }
                        }
                    }
                }

                let lhs_node = LHSNode {
                    relation_name: output_relation_name,
                    output_attributes: lhs_attributes,
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
                    process_do_block(do_block, context, &mut instructions, &self.symbol_table)?
                }

                let mut rhs_nodes = Vec::new();

                for capture_ref in captures {
                    if let Lval::Capture(capture_name, provenance_type) = &**capture_ref {
                        // Look up the capture's relation, prioritizing internal relations
                        let symbol = self
                            .symbol_table
                            .lookup_capture_by_role(capture_name, IRRelationRole::Internal)
                            .or_else(|| {
                                self.symbol_table.lookup_capture_by_role(
                                    capture_name,
                                    IRRelationRole::Intermediate,
                                )
                            })
                            .or_else(|| self.symbol_table.lookup_capture(capture_name))
                            .ok_or_else(|| {
                                Error::ResolveError(format!(
                                    "Referenced capture '{}' not found",
                                    capture_name
                                ))
                            })?;
                        let referenced_rel = &symbol.relationref.name;

                        if let Some(relation) =
                            Self::lookup_relation(ir_program, referenced_rel, None)
                        {
                            // if we've already "hidrated" the intermediate (structural) relation, we can use it directly and just
                            // avoid adding it to the predicate of the rule.
                            // Predicates for internal relations will still be added in the RHS of the rule.
                            let exists = rhs_nodes.iter().any(|rhs_node| {
                                if let RHSVal::RHSNode(rhs_node) = rhs_node {
                                    rhs_node.relation_name == *referenced_rel
                                } else {
                                    false
                                }
                            });
                            //
                            // if not, we need to look it up and add it to the body of the rule.
                            // Note: we're hydrating all the attributes of the intermediary relation, not
                            // only the captureref's ones.
                            if !exists {
                                // Get all attributes from the intermediary relation
                                let attributes: Vec<String> = relation
                                    .attributes
                                    .iter()
                                    .map(|attr| format!("rhs_{}", normalize_string(&attr.name)))
                                    .collect();

                                let rhs_node = RHSNode {
                                    relation_name: referenced_rel.clone(),
                                    attributes,
                                };

                                rhs_nodes.push(RHSVal::RHSNode(rhs_node));
                            }

                            // Look up the specific attribute mapping for this capture and augment
                            // with provenance.
                            if let Some(mappings) = context.get_capture_mappings(capture_name) {
                                for mapping in mappings {
                                    // Add Path relation node only for the mapped attribute
                                    let path_node = provenance_type
                                        .as_ref()
                                        .and_then(|p| match p {
                                            ProvenanceType::Path => Some(format!(
                                                "rhs_{}_path",
                                                mapping.symbol.to_lowercase()
                                            )),
                                            ProvenanceType::Downstream => Some(format!(
                                                "rhs_{}_downstream",
                                                mapping.symbol.to_lowercase()
                                            )),
                                            ProvenanceType::Dependency => Some(format!(
                                                "rhs_{}_dependency",
                                                mapping.symbol.to_lowercase()
                                            )),
                                            ProvenanceType::Upstream => Some(format!(
                                                "rhs_{}_upstream",
                                                mapping.symbol.to_lowercase()
                                            )),
                                            ProvenanceType::Span => Some(format!(
                                                "rhs_{}_span",
                                                mapping.symbol.to_lowercase()
                                            )),
                                            ProvenanceType::Full => Some(format!(
                                                "rhs_{}_path",
                                                mapping.symbol.to_lowercase()
                                            )),
                                            ProvenanceType::Default => None,
                                        })
                                        .map(|prv_attr_name| RHSNode {
                                            relation_name: "PathExpr".to_string(),
                                            attributes: vec![
                                                prv_attr_name,
                                                format!(
                                                    "rhs_{}_id",
                                                    mapping
                                                        .alias
                                                        .to_lowercase()
                                                        .trim_end_matches("_id")
                                                ),
                                            ],
                                        });

                                    if let Some(path_node) = path_node {
                                        rhs_nodes.push(RHSVal::RHSNode(path_node));
                                    }
                                }
                            } else {
                                return Err(Error::ResolveError(format!(
                                    "Referenced relation '{}' not found",
                                    referenced_rel
                                )));
                            }
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
                // Process descendants first to collect their attributes
                let mut outbound_attrs = process_descendants(
                    descendant,
                    context,
                    ir_program,
                    capture_rules,
                    symbol_table,
                    attrs_map,
                    capture_refs,
                )?;

                // Process current capture form's attributes
                let capture_decls = process_current_captures(
                    attrs_map,
                    rel_name,
                    symbol_table,
                    &mut outbound_attrs,
                    context,
                )?;

                // Process capture references
                process_capture_refs(capture_refs, symbol_table, &mut outbound_attrs)?;

                // Get input relation attributes
                let input_relation = get_input_relation(ir_program, rel_name)?;

                // Validate that all referenced attributes exist in the input relation
                validate_attributes_exist(rel_name, input_relation, attrs_map)?;

                let rhs_attrs = generate_rhs_attributes(input_relation, attrs_map, when);

                // Build RHS value with lineage
                let rhs_vals_with_lineage =
                    build_rhs_with_lineage(rel_name, rhs_attrs, capture_rules);

                // Generate internal relation
                let (internal_relation_name, internal_relation) =
                    generate_internal_relation(attrs_map, input_relation)?;

                // Register internal relation
                ir_program.relations.push(internal_relation.clone());

                // Update symbol table with internal relation captures
                register_internal_relation_captures(
                    capture_decls,
                    &internal_relation_name,
                    symbol_table,
                )?;

                // Validate capture references
                validate_capture_refs(capture_refs, symbol_table)?;

                // Create and register the rule. This includes creating the LHS (head) of the rule.
                let ir_rule = create_capture_rule(
                    internal_relation_name,
                    &outbound_attrs,
                    rhs_vals_with_lineage,
                );

                capture_rules.push_front(ir_rule);
            }
            Ok(())
        }

        // Helper functions

        fn process_descendants(
            descendant: &Option<Box<Lval>>,
            context: &mut SSAContext,
            ir_program: &mut IRProgram,
            capture_rules: &mut VecDeque<IRRule>,
            symbol_table: &mut SymbolTable,
            attrs_map: &IndexMap<String, Box<Lval>>,
            capture_refs: &[Box<Lval>],
        ) -> Result<VecDeque<Attribute>> {
            let mut outbound_attrs = VecDeque::new();

            if let Some(desc) = descendant {
                collect_rules_from_capture(desc, context, ir_program, capture_rules, symbol_table)?;

                if let Some(last_rule) = capture_rules.front() {
                    collect_descendant_attributes(
                        last_rule,
                        attrs_map,
                        capture_refs,
                        &mut outbound_attrs,
                    );
                }
            }

            Ok(outbound_attrs)
        }

        fn collect_descendant_attributes(
            rule: &IRRule,
            attrs_map: &IndexMap<String, Box<Lval>>,
            capture_refs: &[Box<Lval>],
            outbound_attrs: &mut VecDeque<Attribute>,
        ) {
            for attr_name in rule.lhs.output_attributes.iter() {
                let clean_name = attr_name.trim_start_matches("lhs_").to_string();

                if is_referenced_attribute(&clean_name, attrs_map, capture_refs) {
                    outbound_attrs.push_front(create_attribute(&clean_name));

                    // Check for provenance in capture refs
                    for capture_ref in capture_refs {
                        if let Lval::Capture(capture_name, provenance) = &**capture_ref {
                            if capture_name == &clean_name {
                                if let Some(prov_type) = provenance {
                                    match prov_type {
                                        ProvenanceType::Path => {
                                            outbound_attrs.push_front(Attribute {
                                                name: format!("{}_path", clean_name),
                                                attr_type: AttributeType::String,
                                            });
                                        }
                                        ProvenanceType::Downstream => {
                                            outbound_attrs.push_front(Attribute {
                                                name: format!("{}_downstream", clean_name),
                                                attr_type: AttributeType::String,
                                            });
                                        }
                                        ProvenanceType::Dependency => {
                                            outbound_attrs.push_front(Attribute {
                                                name: format!("{}_dependency", clean_name),
                                                attr_type: AttributeType::String,
                                            });
                                        }
                                        ProvenanceType::Upstream => {
                                            outbound_attrs.push_front(Attribute {
                                                name: format!("{}_upstream", clean_name),
                                                attr_type: AttributeType::String,
                                            });
                                        }
                                        ProvenanceType::Span => {
                                            outbound_attrs.push_front(Attribute {
                                                name: format!("{}_span", clean_name),
                                                attr_type: AttributeType::String,
                                            });
                                        }
                                        ProvenanceType::Full => {
                                            outbound_attrs.push_front(Attribute {
                                                name: format!("{}_path", clean_name),
                                                attr_type: AttributeType::String,
                                            });
                                            outbound_attrs.push_front(Attribute {
                                                name: format!("{}_span", clean_name),
                                                attr_type: AttributeType::String,
                                            });
                                        }
                                        ProvenanceType::Default => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        fn is_referenced_attribute(
            name: &str,
            attrs_map: &IndexMap<String, Box<Lval>>,
            capture_refs: &[Box<Lval>],
        ) -> bool {
            let is_in_captures = attrs_map.values().any(|lval| {
                if let Lval::Capture(capture_name, _) = lval.as_ref() {
                    capture_name.to_string() == name
                } else {
                    false
                }
            });

            let is_in_capture_refs = capture_refs.iter().any(|cr| {
                if let Lval::Capture(capture_name, _) = &**cr {
                    capture_name.to_string() == name
                } else {
                    false
                }
            });

            is_in_captures || is_in_capture_refs
        }

        /**
         *        * TODO: Selecting the attribute type based on the attribute name is a temporary solution.
         *        */
        fn create_attribute(name: &str) -> Attribute {
            Attribute {
                name: name.to_string(),
                attr_type: if normalize_string(name).ends_with("_id") {
                    AttributeType::Number
                } else {
                    AttributeType::String
                },
            }
        }

        fn process_current_captures(
            attrs_map: &IndexMap<String, Box<Lval>>,
            rel_name: &str,
            symbol_table: &mut SymbolTable,
            outbound_attrs: &mut VecDeque<Attribute>,
            context: &mut SSAContext,
        ) -> Result<Vec<String>> {
            let mut processed_captures = Vec::new();
            for (attr_name, lval_box) in attrs_map {
                if let Lval::Capture(capture_name, provenance) = lval_box.as_ref() {
                    symbol_table.insert_capture(
                        capture_name,
                        RelationRef::new(rel_name.to_string(), IRRelationRole::Input),
                    )?;

                    // Store the capture mapping in SSA context
                    context.add_capture_mapping(
                        capture_name.to_string(),
                        rel_name.to_string(),
                        attr_name.to_string(),
                    );

                    // Add the main attribute
                    outbound_attrs.push_front(create_attribute(capture_name));

                    // Add provenance attributes based on provenance type
                    if let Some(prov_type) = provenance {
                        match prov_type {
                            ProvenanceType::Path => {
                                outbound_attrs.push_front(Attribute {
                                    name: format!("{}_path", capture_name),
                                    attr_type: AttributeType::String,
                                });
                            }
                            ProvenanceType::Downstream => {
                                outbound_attrs.push_front(Attribute {
                                    name: format!("{}_downstream", capture_name),
                                    attr_type: AttributeType::String,
                                });
                            }
                            ProvenanceType::Dependency => {
                                outbound_attrs.push_front(Attribute {
                                    name: format!("{}_dependency", capture_name),
                                    attr_type: AttributeType::String,
                                });
                            }
                            ProvenanceType::Upstream => {
                                outbound_attrs.push_front(Attribute {
                                    name: format!("{}_upstream", capture_name),
                                    attr_type: AttributeType::String,
                                });
                            }
                            ProvenanceType::Span => {
                                outbound_attrs.push_front(Attribute {
                                    name: format!("{}_span", capture_name),
                                    attr_type: AttributeType::String,
                                });
                            }
                            ProvenanceType::Full => {
                                outbound_attrs.push_front(Attribute {
                                    name: format!("{}_path", capture_name),
                                    attr_type: AttributeType::String,
                                });
                                outbound_attrs.push_front(Attribute {
                                    name: format!("{}_span", capture_name),
                                    attr_type: AttributeType::String,
                                });
                            }
                            ProvenanceType::Default => {
                                // No additional attributes needed
                            }
                        }
                    }

                    processed_captures.push(capture_name.clone());
                }
            }
            Ok(processed_captures)
        }

        fn process_capture_refs(
            capture_refs: &[Box<Lval>],
            symbol_table: &mut SymbolTable,
            outbound_attrs: &mut VecDeque<Attribute>,
        ) -> Result<()> {
            for capture_ref in capture_refs {
                if let Lval::Capture(capture_name, provenance) = &**capture_ref {
                    if symbol_table.lookup_capture(capture_name).is_some() {
                        outbound_attrs.push_front(create_attribute(capture_name));

                        // Add provenance attributes if specified
                        if let Some(prov_type) = provenance {
                            match prov_type {
                                ProvenanceType::Path => {
                                    outbound_attrs.push_front(Attribute {
                                        name: format!("{}_path", capture_name),
                                        attr_type: AttributeType::String,
                                    });
                                }
                                ProvenanceType::Downstream => {
                                    outbound_attrs.push_front(Attribute {
                                        name: format!("{}_downstream", capture_name),
                                        attr_type: AttributeType::String,
                                    });
                                }
                                ProvenanceType::Dependency => {
                                    outbound_attrs.push_front(Attribute {
                                        name: format!("{}_dependency", capture_name),
                                        attr_type: AttributeType::String,
                                    });
                                }
                                ProvenanceType::Upstream => {
                                    outbound_attrs.push_front(Attribute {
                                        name: format!("{}_upstream", capture_name),
                                        attr_type: AttributeType::String,
                                    });
                                }
                                ProvenanceType::Span => {
                                    outbound_attrs.push_front(Attribute {
                                        name: format!("{}_span", capture_name),
                                        attr_type: AttributeType::String,
                                    });
                                }
                                ProvenanceType::Full => {
                                    outbound_attrs.push_front(Attribute {
                                        name: format!("{}_path", capture_name),
                                        attr_type: AttributeType::String,
                                    });
                                    outbound_attrs.push_front(Attribute {
                                        name: format!("{}_span", capture_name),
                                        attr_type: AttributeType::String,
                                    });
                                }
                                ProvenanceType::Default => {
                                    // No additional attributes needed
                                }
                            }
                        }
                    } else {
                        return Err(Error::ResolveError(format!(
                            "Referenced capture '{}' not found",
                            capture_name
                        )));
                    }
                }
            }
            Ok(())
        }

        fn get_input_relation<'a>(
            ir_program: &'a IRProgram,
            rel_name: &str,
        ) -> Result<&'a IRRelationType> {
            ir_program
                .relations
                .iter()
                .find(|rel| rel.name == to_pascal_case(rel_name))
                .ok_or_else(|| {
                    Error::ResolveError(format!(
                        "Relation '{}' (as '{}') not found in program",
                        rel_name,
                        to_pascal_case(rel_name)
                    ))
                })
        }

        /// Validates that all attributes referenced in the attrs_map exist in the input relation.
        /// Returns Ok(()) if all attributes are valid, or an Error if any attribute is not found.
        fn validate_attributes_exist(
            capture_relation_name: &str,
            input_relation: &IRRelationType,
            attrs_map: &IndexMap<String, Box<Lval>>,
        ) -> Result<()> {
            // Create a set of attribute names from the input relation for efficient lookup
            let relation_attrs: HashSet<String> = input_relation
                .attributes
                .iter()
                .map(|attr| attr.name.clone())
                .collect();

            // Check each attribute in the attrs_map
            for (attr_name, _) in attrs_map {
                if !relation_attrs.contains(attr_name) {
                    return Err(Error::ResolveError(format!(
                        "Attribute '{}' referenced in capture form '{}' does not exist in relation '{}'",
                        attr_name, capture_relation_name, input_relation.name
                    )));
                }
            }

            Ok(())
        }

        fn extract_attributes_from_when_block(when_block: &Option<Box<Lval>>) -> HashSet<String> {
            let mut referenced_attrs = HashSet::new();

            if let Some(logical) = when_block {
                if let Lval::Logical(_, operands) = &**logical {
                    for operand in operands {
                        extract_attributes_from_lval(operand, &mut referenced_attrs);
                    }
                }
            }

            referenced_attrs
        }

        fn extract_attributes_from_lval(lval: &Lval, referenced_attrs: &mut HashSet<String>) {
            match lval {
                Lval::Capture(capture, _provenance) => {
                    referenced_attrs.insert(capture.clone());
                }
                Lval::Sym(sym) => {
                    // Symbols in conditions often start with '@' to reference attributes
                    if sym.starts_with('@') {
                        referenced_attrs.insert(sym[1..].to_string());
                    }
                }
                Lval::Logical(_, operands) => {
                    for operand in operands {
                        extract_attributes_from_lval(operand, referenced_attrs);
                    }
                }
                _ => {}
            }
        }

        fn generate_rhs_attributes(
            input_relation: &IRRelationType,
            attrs_map: &IndexMap<String, Box<Lval>>,
            when_block: &Option<Box<Lval>>,
        ) -> Vec<String> {
            // Extract attributes referenced in conditions
            let condition_attrs = extract_attributes_from_when_block(when_block);

            input_relation
                .attributes
                .iter()
                .map(|attr| {
                    if let Some(attr_val) = attrs_map.get(&attr.name) {
                        if let Lval::Capture(capture_name, _provenance) = attr_val.as_ref() {
                            format!("rhs_{}", normalize_string(capture_name))
                        } else {
                            // Check if this attribute is used in a condition
                            if condition_attrs.contains(&attr.name) {
                                format!("rhs_{}", normalize_string(&attr.name))
                            } else {
                                format!("unused_{}", normalize_string(&attr.name))
                            }
                        }
                    } else {
                        // Check if this attribute is used in a condition
                        if condition_attrs.contains(&attr.name) {
                            format!("rhs_{}", normalize_string(&attr.name))
                        } else {
                            format!("unused_{}", normalize_string(&attr.name))
                        }
                    }
                })
                .collect()
        }

        fn build_rhs_with_lineage(
            rel_name: &str,
            rhs_attrs: Vec<String>,
            capture_rules: &VecDeque<IRRule>,
        ) -> RHSVal {
            let mut unique_rhs_vals = vec![RHSVal::RHSNode(RHSNode {
                relation_name: to_pascal_case(rel_name),
                attributes: rhs_attrs,
            })];

            for rule in capture_rules {
                if let RHSVal::NestedRHS(nested_vals) = &rule.rhs {
                    for val in nested_vals {
                        if !unique_rhs_vals.contains(val) {
                            unique_rhs_vals.push(val.clone());
                        }
                    }
                } else if !unique_rhs_vals.contains(&rule.rhs) {
                    unique_rhs_vals.push(rule.rhs.clone());
                }
            }

            RHSVal::NestedRHS(unique_rhs_vals)
        }

        fn generate_internal_relation(
            attrs_map: &IndexMap<String, Box<Lval>>,
            input_relation: &IRRelationType,
        ) -> Result<(String, IRRelationType)> {
            let unique_relation_attrs: IndexSet<_> = attrs_map.keys().cloned().collect();
            let mut attr_type_map = HashMap::new();

            for relation_attr in unique_relation_attrs.iter() {
                for input_relation_attr in &input_relation.attributes {
                    if *relation_attr == input_relation_attr.name {
                        attr_type_map.insert(relation_attr, &input_relation_attr.attr_type);
                    }
                }
            }

            let normalized_relation_attrs = unique_relation_attrs
                .iter()
                .map(|attr| {
                    let attr_type = attr_type_map.get(&attr).unwrap();

                    // remove prefix
                    let attr_name = attrs_map.get(attr).map(|a| a.to_string());
                    let attr_name = attr_name
                        .unwrap_or_else(|| attr.to_string())
                        .trim_start_matches("@")
                        .to_string();

                    Attribute {
                        name: normalize_string(&attr_name),
                        attr_type: (*attr_type).clone(),
                    }
                })
                .collect::<Vec<_>>();

            let combined_attrs = normalized_relation_attrs
                .iter()
                .map(|attr| attr.name.clone())
                .collect::<Vec<_>>()
                .join("_");

            let hash_input = format!("{}_{}", input_relation.name, combined_attrs);
            let hash = {
                use std::hash::{Hash, Hasher};
                let mut hasher = std::collections::hash_map::DefaultHasher::new();
                hash_input.hash(&mut hasher);
                format!("{:x}", hasher.finish())
            };

            let internal_relation_name =
                format!("{}__{}", to_pascal_case(&combined_attrs), &hash[0..8]);

            let internal_relation = IRRelationType {
                name: internal_relation_name.clone(),
                attributes: normalized_relation_attrs.into_iter().collect(),
                role: IRRelationRole::Internal,
                category: Some(ir::RelationCategory::Structural),
            };

            Ok((internal_relation_name, internal_relation))
        }

        fn register_internal_relation_captures(
            capture_decls: Vec<String>,
            //internal_relation: &IRRelationType,
            internal_relation_name: &str,
            symbol_table: &mut SymbolTable,
        ) -> Result<()> {
            for capture in capture_decls {
                symbol_table.insert_capture(
                    &capture,
                    RelationRef::new(internal_relation_name.to_string(), IRRelationRole::Internal),
                )?;
            }
            /*
            for attr in &internal_relation.attributes {
                symbol_table.insert_capture(
                    &normalize_string(&attr.name),
                    RelationRef::new(
                        internal_relation_name.to_string(),
                        IRRelationRole::Intermediate,
                    ),
                )?;
            }*/
            Ok(())
        }

        fn validate_capture_refs(
            capture_refs: &[Box<Lval>],
            symbol_table: &SymbolTable,
        ) -> Result<()> {
            for capture_ref in capture_refs {
                if let Lval::Capture(capture_name, _) = &**capture_ref {
                    let capture_name = &capture_name;
                    if symbol_table.lookup_capture(capture_name).is_none() {
                        return Err(Error::ResolveError(format!(
                            "Referenced capture '{}' not found",
                            capture_name
                        )));
                    }
                }
            }
            Ok(())
        }

        fn create_capture_rule(
            internal_relation_name: String,
            outbound_attrs: &VecDeque<Attribute>,
            rhs_vals_with_lineage: RHSVal,
        ) -> IRRule {
            IRRule {
                lhs: LHSNode {
                    relation_name: internal_relation_name,
                    output_attributes: outbound_attrs
                        .iter()
                        .map(|attr| format!("lhs_{}", normalize_string(&attr.name)))
                        .collect(),
                },
                rhs: rhs_vals_with_lineage,
                ssa_block: None,
            }
        }

        /// Maps inference predicates to SSA operations
        fn process_single_predicate(
            predicate: &Lval,
            context: &mut SSAContext,
        ) -> Result<(Vec<SSAInstruction>, String)> {
            let mut instructions = Vec::new();
            let result_var = context.generate_temp_var();

            match predicate {
                Lval::Predicate(pred_name, arguments) => {
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
                            return Err(Error::GenerationError(format!(
                                "Unknown predicate: {}",
                                pred_name
                            )));
                            /*
                            instructions.push(SSAInstruction::Assignment {
                                variable: result_var.clone(),
                                operation: SSAOperation {
                                    op_type: OperationType::Load,
                                    operands: vec![Operand::Reference(Reference::Named(
                                        pred_name.clone(),
                                    ))],
                                },
                            });
                            return (instructions, result_var);
                            */
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
                                        operands: vec![Operand::Reference(Reference::Named(
                                            arg.clone(),
                                        ))],
                                    },
                                });
                                temp_var
                            }
                        })
                        .collect();

                    // Add the predicate operation
                    let operands = operands
                        .iter()
                        .map(|op| Operand::Reference(Reference::Named(op.clone())))
                        .collect();
                    instructions.push(SSAInstruction::Assignment {
                        variable: result_var.clone(),
                        operation: SSAOperation { op_type, operands },
                    });
                }
                Lval::PrefixPredicate(prefix_name, inner_predicate) => {
                    // First process the inner predicate
                    if let Ok((mut inner_instructions, inner_result)) =
                        process_single_predicate(inner_predicate, context)
                    {
                        instructions.extend(inner_instructions);

                        // Then wrap it with the prefix operation
                        let op_type = match prefix_name.as_str() {
                            "not" => OperationType::Not,
                            // Add other prefix operations as needed
                            _ => panic!("Unknown prefix predicate: {}", prefix_name),
                        };

                        instructions.push(SSAInstruction::Assignment {
                            variable: result_var.clone(),
                            operation: SSAOperation {
                                op_type,
                                operands: vec![Operand::Reference(Reference::Named(inner_result))],
                            },
                        });
                    } else {
                        return Err(Error::GenerationError(format!(
                            "Invalid prefix predicate: {}",
                            prefix_name
                        )));
                    }
                }
                _ => panic!("Invalid predicate type"),
            }

            Ok((instructions, result_var))
        }

        fn inference_predicates_to_ssa(
            predicates: &[Box<Lval>],
            context: &mut SSAContext,
        ) -> Vec<SSAInstruction> {
            let mut instructions = Vec::new();
            let predicate_group_label = context.generate_label();
            instructions.push(SSAInstruction::Label(predicate_group_label.clone()));

            for predicate in predicates {
                if let Ok((mut pred_instructions, _result_var)) =
                    process_single_predicate(predicate, context)
                {
                    instructions.append(&mut pred_instructions);
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
                // Create or validate output relation
                ensure_output_relation_exists(
                    relation_name,
                    ir::RelationCategory::Domain,
                    params,
                    ir_program,
                )?;

                // Process each inference path
                if inference_paths.is_some() {
                    for path in inference_paths.as_ref().unwrap() {
                        if let Lval::InferencePath(predicates, computation) = &**path {
                            let (rhs_nodes, mut instructions) =
                                process_predicates(predicates, context)?;

                            // Process computation block if present
                            if let Some(comp) = computation {
                                process_computation(comp, context, &mut instructions)?;
                            }

                            // Create the inference rule
                            let ir_rule = create_inference_rule(
                                relation_name,
                                params.as_ref(),
                                rhs_nodes,
                                instructions,
                            );

                            infer_rules.push_back(ir_rule);
                        }
                    }
                }
            }
            Ok(())
        }

        fn ensure_output_relation_exists(
            relation_name: &str,
            relation_category: ir::RelationCategory,
            params: &Option<Vec<String>>,
            ir_program: &mut IRProgram,
        ) -> Result<()> {
            if !ir_program
                .relations
                .iter()
                .any(|r| r.name == *relation_name)
            {
                let attributes = params
                    .as_ref()
                    .map(|ps| {
                        ps.iter()
                            .map(|p| {
                                let param_name = &p[1..];
                                Attribute {
                                    name: param_name.to_string(),
                                    attr_type: if param_name.ends_with("_id") {
                                        AttributeType::Number
                                    } else {
                                        AttributeType::String
                                    },
                                }
                            })
                            .collect()
                    })
                    .unwrap_or_default();

                ir_program.relations.push(IRRelationType {
                    name: to_pascal_case(relation_name),
                    attributes,
                    role: IRRelationRole::Output,
                    category: Some(relation_category),
                });
            }
            Ok(())
        }

        fn process_predicates(
            predicates: &[Box<Lval>],
            context: &mut SSAContext,
        ) -> Result<(Vec<RHSVal>, Vec<SSAInstruction>)> {
            let mut rhs_nodes = Vec::new();
            let mut instructions = Vec::new();

            // Convert predicates to SSA instructions
            instructions.extend(inference_predicates_to_ssa(predicates, context));

            // Process each predicate
            for (position, predicate) in predicates.iter().enumerate() {
                match &**predicate {
                    Lval::PrefixPredicate(prefix_name, embedded_pred) => {
                        process_prefix_predicate(
                            prefix_name,
                            embedded_pred,
                            position,
                            context,
                            &mut rhs_nodes,
                            &mut instructions,
                        )?;
                    }
                    Lval::Predicate(pred_name, arguments) => {
                        process_regular_predicate(pred_name, arguments, &mut rhs_nodes)?;
                    }
                    _ => return Err(Error::GenerationError("Invalid predicate type".to_string())),
                }
            }

            Ok((rhs_nodes, instructions))
        }

        fn process_prefix_predicate(
            prefix_name: &str,
            embedded_pred: &Lval,
            position: usize,
            context: &mut SSAContext,
            rhs_nodes: &mut Vec<RHSVal>,
            instructions: &mut Vec<SSAInstruction>,
        ) -> Result<()> {
            if let Lval::Predicate(pred_name, arguments) = embedded_pred {
                let rhs_node = RHSVal::RHSNode(RHSNode {
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
                        .collect(),
                });
                rhs_nodes.push(rhs_node);

                // Generate SSA instructions for prefix operation
                let load_var = context.generate_temp_var();
                let result_var = context.generate_temp_var();

                instructions.push(SSAInstruction::Assignment {
                    variable: load_var.clone(),
                    operation: SSAOperation {
                        op_type: OperationType::Load,
                        operands: vec![Operand::Reference(Reference::Position(position))],
                    },
                });

                let op_type = match prefix_name {
                    "not" => OperationType::Not,
                    _ => {
                        return Err(Error::GenerationError(format!(
                            "Unknown prefix predicate: {}",
                            prefix_name
                        )))
                    }
                };

                instructions.push(SSAInstruction::Assignment {
                    variable: result_var,
                    operation: SSAOperation {
                        op_type,
                        operands: vec![Operand::Reference(Reference::Named(load_var))],
                    },
                });

                Ok(())
            } else {
                Err(Error::GenerationError(
                    "Invalid prefix predicate structure".to_string(),
                ))
            }
        }

        fn process_regular_predicate(
            pred_name: &str,
            arguments: &[String],
            rhs_nodes: &mut Vec<RHSVal>,
        ) -> Result<()> {
            // Skip built-in predicates
            if !is_builtin_predicate(pred_name) {
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
                        .collect(),
                };
                rhs_nodes.push(RHSVal::RHSNode(rhs_node));
            }
            Ok(())
        }

        fn is_builtin_predicate(pred_name: &str) -> bool {
            matches!(
                pred_name,
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
            )
        }

        fn process_computation(
            comp: &Lval,
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
        ) -> Result<()> {
            if let Lval::Computation(_var_name, qexpr) = comp {
                if let Lval::Qexpr(exprs) = &**qexpr {
                    for expr in exprs {
                        if let Lval::Sexpr(cells) = &**expr {
                            process_computation_expression(cells, context, instructions)?;
                        }
                    }
                }
            }
            Ok(())
        }

        fn process_computation_expression(
            cells: &[Box<Lval>],
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
        ) -> Result<()> {
            if let Some(Lval::Sym(operation)) = cells.first().map(|v| &**v) {
                let operands: Vec<String> = cells
                    .iter()
                    .skip(1)
                    .map(|cell| match &**cell {
                        Lval::Sym(s) => s.clone(),
                        Lval::Capture(c, _) => format!("rhs_{}", &c[1..]),
                        _ => String::new(),
                    })
                    .collect();

                let temp_var = context.generate_temp_var();
                instructions.push(SSAInstruction::Assignment {
                    variable: temp_var,
                    operation: SSAOperation {
                        op_type: OperationType::from(operation.as_str()),
                        operands: operands
                            .iter()
                            .map(|op| Operand::Reference(Reference::Named(op.clone())))
                            .collect(),
                    },
                });
            }
            Ok(())
        }

        fn create_inference_rule(
            relation_name: &str,
            params: Option<&Vec<String>>,
            rhs_nodes: Vec<RHSVal>,
            instructions: Vec<SSAInstruction>,
        ) -> IRRule {
            IRRule {
                lhs: LHSNode {
                    relation_name: to_pascal_case(relation_name),
                    output_attributes: params
                        .map(|parms| parms.iter().map(|p| p[1..].to_string().clone()).collect())
                        .unwrap_or_default(),
                },
                rhs: RHSVal::NestedRHS(rhs_nodes),
                ssa_block: if !instructions.is_empty() {
                    Some(SSAInstructionBlock { instructions })
                } else {
                    None
                },
            }
        }

        fn process_do_block(
            do_block: &Lval,
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
            symbol_table: &SymbolTable,
        ) -> Result<()> {
            if let Lval::DoForm(actions) = &do_block {
                for action in actions {
                    if let Lval::Qexpr(cells) = &**action {
                        for cell in cells {
                            if let Lval::Sexpr(lvals) = &**cell {
                                if let Some(Lval::Sym(operation)) = lvals.first().map(|v| &**v) {
                                    match operation.as_str() {
                                        "format" => {
                                            process_format_operation(
                                                lvals,
                                                context,
                                                instructions,
                                                symbol_table,
                                            )?;
                                        }
                                        "concat" => {
                                            process_concat_operation(
                                                lvals,
                                                context,
                                                instructions,
                                                symbol_table,
                                            )?;
                                        }
                                        "replace" => {
                                            process_replace_operation(
                                                lvals,
                                                context,
                                                instructions,
                                                symbol_table,
                                            )?;
                                        }
                                        "lowercase" | "uppercase" => {
                                            process_case_operation(
                                                operation.as_str(),
                                                lvals,
                                                context,
                                                instructions,
                                                symbol_table,
                                            )?;
                                        }
                                        "trim" => {
                                            process_trim_operation(
                                                lvals,
                                                context,
                                                instructions,
                                                symbol_table,
                                            )?;
                                        }
                                        _ => {
                                            return Err(Error::GenerationError(format!(
                                                "Unsupported operation: {}",
                                                operation
                                            )));
                                        }
                                    }
                                }
                            } else {
                                return Err(Error::GenerationError(format!(
                                    "Invalid do-block cell type: {:?}",
                                    cell
                                )));
                            }
                        }
                    }
                }
            }
            Ok(())
        }

        fn process_format_operation(
            lvals: &[Box<Lval>],
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
            symbol_table: &SymbolTable,
        ) -> Result<()> {
            let operands = parse_string_operands(&lvals[1..], context, symbol_table)?;
            generate_string_concat_instructions(operands, context, instructions);
            Ok(())
        }

        fn process_concat_operation(
            lvals: &[Box<Lval>],
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
            symbol_table: &SymbolTable,
        ) -> Result<()> {
            if lvals.len() < 3 {
                return Err(Error::GenerationError(
                    "concat requires at least 2 operands".to_string(),
                ));
            }
            let operands = parse_string_operands(&lvals[1..], context, symbol_table)?;
            generate_string_concat_instructions(operands, context, instructions);
            Ok(())
        }

        fn process_replace_operation(
            lvals: &[Box<Lval>],
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
            symbol_table: &SymbolTable,
        ) -> Result<()> {
            if lvals.len() != 4 {
                return Err(Error::GenerationError(
                    "replace requires 3 operands".to_string(),
                ));
            }

            let target = parse_single_operand(&lvals[1], context, symbol_table)?;
            let pattern = parse_single_operand(&lvals[2], context, symbol_table)?;
            let replacement = parse_single_operand(&lvals[3], context, symbol_table)?;

            let result_var = context.generate_temp_var();
            instructions.push(SSAInstruction::Assignment {
                variable: result_var,
                operation: SSAOperation {
                    op_type: OperationType::Replace,
                    operands: vec![
                        operand_to_reference(target),
                        operand_to_reference(pattern),
                        operand_to_reference(replacement),
                    ],
                },
            });
            Ok(())
        }

        fn process_case_operation(
            operation: &str,
            lvals: &[Box<Lval>],
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
            symbol_table: &SymbolTable,
        ) -> Result<()> {
            if lvals.len() != 2 {
                return Err(Error::GenerationError(format!(
                    "{} requires 1 operand",
                    operation
                )));
            }

            let input = parse_single_operand(&lvals[1], context, symbol_table)?;
            let result_var = context.generate_temp_var();

            instructions.push(SSAInstruction::Assignment {
                variable: result_var,
                operation: SSAOperation {
                    op_type: if operation == "lowercase" {
                        OperationType::ToLower
                    } else {
                        OperationType::ToUpper
                    },
                    operands: vec![operand_to_reference(input)],
                },
            });
            Ok(())
        }

        fn process_trim_operation(
            lvals: &[Box<Lval>],
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
            symbol_table: &SymbolTable,
        ) -> Result<()> {
            if lvals.len() != 2 {
                return Err(Error::GenerationError(
                    "trim requires 1 operand".to_string(),
                ));
            }

            let input = parse_single_operand(&lvals[1], context, symbol_table)?;
            let result_var = context.generate_temp_var();

            instructions.push(SSAInstruction::Assignment {
                variable: result_var,
                operation: SSAOperation {
                    op_type: OperationType::Trim,
                    operands: vec![operand_to_reference(input)],
                },
            });
            Ok(())
        }

        fn parse_string_operands(
            lvals: &[Box<Lval>],
            context: &SSAContext,
            symbol_table: &SymbolTable,
        ) -> Result<Vec<StringPart>> {
            lvals
                .iter()
                .map(|lval| parse_single_operand(&**lval, context, symbol_table))
                .collect()
        }

        fn parse_single_operand(
            lval: &Lval,
            context: &SSAContext,
            symbol_table: &SymbolTable,
        ) -> Result<StringPart> {
            match lval {
                Lval::Sym(operand) => {
                    if let Some('$') = operand.chars().next() {
                        Ok(StringPart::Dynamic(operand.clone()))
                    } else {
                        Ok(StringPart::Static(operand.clone()))
                    }
                }
                Lval::Capture(capture_name, _provenance) => {
                    // Validate that the capture exists in the symbol table
                    let _ = symbol_table.lookup_capture(capture_name).ok_or_else(|| {
                        Error::ResolveError(format!(
                            "Referenced capture '{}' not found in do block",
                            capture_name
                        ))
                    })?;

                    Ok(StringPart::Dynamic(format!(
                        "rhs_{}",
                        normalize_string(capture_name)
                    )))
                }
                Lval::String(s) => Ok(StringPart::Static(s[1..s.len() - 1].to_string())),
                _ => Err(Error::GenerationError(format!(
                    "Unsupported operand type: {:?}",
                    lval
                ))),
            }
        }

        fn generate_string_concat_instructions(
            operands: Vec<StringPart>,
            context: &mut SSAContext,
            instructions: &mut Vec<SSAInstruction>,
        ) {
            let mut last_var = String::new();

            // Handle first operand specially
            if let Some(first_operand) = operands.first() {
                let variable = context.generate_temp_var();
                let operation = SSAOperation {
                    op_type: OperationType::Load,
                    operands: vec![operand_to_reference(first_operand.clone())],
                };

                instructions.push(SSAInstruction::Assignment {
                    variable: variable.clone(),
                    operation,
                });

                last_var = variable;
            }

            // Handle remaining operands with concat
            for operand in operands.into_iter().skip(1) {
                let variable = context.generate_temp_var();
                let operation = SSAOperation {
                    op_type: OperationType::Concat,
                    operands: vec![
                        Operand::Reference(Reference::Named(last_var.clone())),
                        operand_to_reference(operand),
                    ],
                };

                instructions.push(SSAInstruction::Assignment {
                    variable: variable.clone(),
                    operation,
                });

                last_var = variable;
            }
        }

        fn operand_to_reference(part: StringPart) -> Operand {
            match part {
                StringPart::Static(text) => {
                    Operand::StringLiteral(text) // Use the static text as-is
                }
                StringPart::Dynamic(var) => {
                    Operand::Reference(Reference::Named(normalize_string(&var)))
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
                                operands: vec![Operand::Identifier(sym.clone())],
                            },
                        });
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![Operand::Reference(Reference::Named(format!(
                                    "rhs_{}",
                                    &sym[1..]
                                )))],
                            },
                        });
                    }
                    Lval::Capture(capture, _provenance) => {
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![Operand::Reference(Reference::Named(format!(
                                    "rhs_{}",
                                    &capture
                                )))],
                            },
                        });
                    }
                    Lval::Num(num) => {
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![Operand::NumberLiteral(*num)],
                            },
                        });
                    }
                    Lval::String(s) => {
                        instructions.push(SSAInstruction::Assignment {
                            variable: temp_var.clone(),
                            operation: SSAOperation {
                                op_type: OperationType::Load,
                                operands: vec![Operand::StringLiteral(s.replace("\"", "").clone())],
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

                operand_vars.push(Operand::Reference(Reference::Named(temp_var.clone())));
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
    use super::*;
    use indexmap::IndexMap;

    #[test]
    fn test_symbol_table() {
        let mut table = SymbolTable::new();

        // Test successful insertions
        assert!(table
            .insert_capture(
                "var1",
                RelationRef::new("relation1".to_string(), IRRelationRole::Input)
            )
            .is_ok());
        assert!(table
            .insert_capture(
                "var2",
                RelationRef::new("relation1".to_string(), IRRelationRole::Input)
            )
            .is_ok());
        assert!(table
            .insert_capture(
                "var3",
                RelationRef::new("relation2".to_string(), IRRelationRole::Input)
            )
            .is_ok());

        // Test duplicate capture name
        assert!(table
            .insert_capture(
                "var1",
                RelationRef::new("relation2".to_string(), IRRelationRole::Input)
            )
            .is_err());

        // Test lookups
        let symbol = table.lookup_capture("var1").unwrap();
        assert_eq!(symbol.name, "var1");
        assert_eq!(symbol.relationref.name, "relation1");

        // Test relation-specific lookups
        let rel1_captures = table.get_relation_captures("relation1");
        assert_eq!(rel1_captures.len(), 2);
        assert!(rel1_captures.iter().any(|s| s.name == "var1"));
        assert!(rel1_captures.iter().any(|s| s.name == "var2"));

        // Test all captures
        let all_captures = table.get_all_captures();
        assert_eq!(all_captures.len(), 3);
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

    #[test]
    fn test_single_capture_form() {
        // Define a simple CaptureForm Lval
        let mut attrs_map = IndexMap::new();
        attrs_map.insert("attr1".to_string(), Lval::num(42));
        attrs_map.insert("attr2".to_string(), Lval::sym("value"));

        let lval = Lval::CaptureForm(
            "TestNode".to_string(),
            attrs_map,
            vec![], // Empty capture_refs
            None,   // No nested captures
            None,   // No when block
            None,   // No do block
        );

        // Create the IrGenerator and generate IR
        let mut generator = IrGenerator::new();

        // Add a mock relation to the IR program
        let mut ir_program = IRProgram {
            rules: Vec::new(),
            relations: vec![IRRelationType {
                name: "TestNode".to_string(),
                attributes: vec![
                    Attribute {
                        name: "attr1".to_string(),
                        attr_type: AttributeType::Number,
                    },
                    Attribute {
                        name: "attr2".to_string(),
                        attr_type: AttributeType::String,
                    },
                ],
                role: IRRelationRole::Input,
                category: Some(ir::RelationCategory::Internal),
            }],
        };

        // Process the Lval
        let result = generator.process_lval(&lval, &mut ir_program, &mut SSAContext::new());

        // Verify that processing succeeded
        assert!(result.is_ok());

        // Verify that rules were added
        assert!(!ir_program.rules.is_empty());
    }

    #[test]
    fn test_nested_capture_forms() {
        // Define a CaptureForm with a nested CaptureForm
        let mut parent_attrs = IndexMap::new();
        parent_attrs.insert("parent_attr1".to_string(), Lval::num(10));
        parent_attrs.insert("parent_attr2".to_string(), Lval::sym("parent_value"));

        let mut child_attrs = IndexMap::new();
        child_attrs.insert("child_attr1".to_string(), Lval::num(20));
        child_attrs.insert("child_attr2".to_string(), Lval::sym("child_value"));

        let child_capture = Lval::CaptureForm(
            "ChildNode".to_string(),
            child_attrs,
            vec![], // Empty capture_refs
            None,   // No nested captures
            None,   // No when block
            None,   // No do block
        );

        let lval = Lval::CaptureForm(
            "ParentNode".to_string(),
            parent_attrs,
            vec![], // Empty capture_refs
            Some(Box::new(child_capture)),
            None, // No when block
            None, // No do block
        );

        // Create the IrGenerator and generate IR
        let mut generator = IrGenerator::new();

        // Create a mock IR program with the necessary relations
        let mut ir_program = IRProgram {
            rules: Vec::new(),
            relations: vec![
                IRRelationType {
                    name: "ParentNode".to_string(),
                    attributes: vec![
                        Attribute {
                            name: "parent_attr1".to_string(),
                            attr_type: AttributeType::Number,
                        },
                        Attribute {
                            name: "parent_attr2".to_string(),
                            attr_type: AttributeType::String,
                        },
                    ],
                    role: IRRelationRole::Input,
                    category: Some(ir::RelationCategory::Internal),
                },
                IRRelationType {
                    name: "ChildNode".to_string(),
                    attributes: vec![
                        Attribute {
                            name: "child_attr1".to_string(),
                            attr_type: AttributeType::Number,
                        },
                        Attribute {
                            name: "child_attr2".to_string(),
                            attr_type: AttributeType::String,
                        },
                    ],
                    role: IRRelationRole::Input,
                    category: Some(ir::RelationCategory::Internal),
                },
            ],
        };

        // Process the Lval
        let result = generator.process_lval(&lval, &mut ir_program, &mut SSAContext::new());

        // Verify that processing succeeded
        assert!(result.is_ok());

        // Verify that rules were added
        assert!(!ir_program.rules.is_empty());
    }
}
