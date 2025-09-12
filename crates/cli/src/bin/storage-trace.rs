use anyhow::{bail, Context, Result};
use clap::Parser;
use tracing::warn;
use graph::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorPipeline, Node,
};
use graph::interface_resolver::BindingRegistry;
use graph::manifest::{find_solidity_files_for_manifest, Manifest, ManifestEntry};
use graph::natspec::extract::extract_source_comments;
use graph::parser::parse_solidity;
use graph::reachability::NodeId; 
use graph::steps::{CallsHandling, ContractHandling};
use graph::parser::get_solidity_language;
use tree_sitter::Node as TsNode; 
use std::collections::HashMap;
use std::fs;
use std::io::{stdout, Write};
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(author, version, about = "Analyzes and compares storage access traces for two Solidity functions.", long_about = None)]
struct Cli {
    #[arg(required = true, num_args = 1..)]
    input_paths: Vec<PathBuf>,

    #[arg(short, long)]
    output_file: Option<PathBuf>,

    #[arg(long)]
    bindings: Option<PathBuf>,

    #[arg(long)]
    manifest_file: Option<PathBuf>,

    #[arg(long, required = true)]
    func1: String,

    #[arg(long, required = true)]
    func2: String,
}

#[derive(Debug, Clone)]
struct OrderedStorageAccessInfo {
    access_type: graph::cg::EdgeType, 
    variable_node_id: NodeId,
    operation_text: String, 
    _operation_span: (usize, usize), 
}

fn find_ancestor_of_kind<'a>(
    mut node: TsNode<'a>,
    kind: &str,
    max_depth: usize,
) -> Option<TsNode<'a>> {
    for _ in 0..max_depth {
        if let Some(parent) = node.parent() {
            if parent.kind() == kind {
                return Some(parent);
            }
            match parent.kind() {
                "expression_statement" | "block" | "function_definition"
                | "modifier_definition" | "contract_definition" | "source_file"
                | "variable_declaration_statement" | "return_statement" => return None,
                _ => node = parent,
            }
        } else {
            return None; 
        }
    }
    None
}

fn is_node_or_descendant(node_to_check: TsNode, potential_container: TsNode) -> bool {
    if node_to_check.id() == potential_container.id() {
        return true;
    }
    node_to_check.start_byte() >= potential_container.start_byte()
        && node_to_check.end_byte() <= potential_container.end_byte()
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    logging::init_subscriber(false);

    let sol_files = find_sol_files(&cli.input_paths)?;
    if sol_files.is_empty() {
        bail!("No valid .sol files found in the provided paths.");
    }

    let mut combined_source = String::new();
    for sol_file in &sol_files {
        let source = fs::read_to_string(sol_file)
            .with_context(|| format!("Failed to read file {}", sol_file.display()))?;
        combined_source.push_str(&source);
        combined_source.push('\n');
    }

    if combined_source.is_empty() {
        bail!("No Solidity source code was successfully read.");
    }

    let combined_ast =
        parse_solidity(&combined_source).context("Failed to parse combined Solidity source")?;
    let solidity_lang = get_solidity_language();

    let input = CallGraphGeneratorInput {
        source: combined_source.to_string(),
        tree: combined_ast.tree,
        solidity_lang,
    };
    let mut ctx = CallGraphGeneratorContext::default();

    let project_root = fs::canonicalize(
        sol_files
            .first()
            .map(|p| p.parent().unwrap_or_else(|| Path::new(".")))
            .unwrap_or_else(|| Path::new(".")),
    )
    .context("Failed to determine project root for manifest/bindings")?;

    warn!(
        "[storage-trace] Using project root for manifest/bindings: {}",
        project_root.display()
    );

    setup_manifest_and_bindings(
        &mut ctx,
        &cli.manifest_file,
        &cli.bindings,
        &cli.input_paths,
        &project_root,
    )?;

    let mut graph = CallGraph::new();
    let mut pipeline = CallGraphGeneratorPipeline::new();
    pipeline.add_step(Box::new(ContractHandling::default()));
    pipeline.add_step(Box::new(CallsHandling::default()));
    pipeline
        .run(input.clone(), &mut ctx, &mut graph, &HashMap::new())
        .context("Failed to generate call graph with pipeline")?;
    graph
        .add_explicit_return_edges(&input, &ctx)
        .context("Failed to add explicit return edges")?;

    let func1_id = find_function_node_id(&graph, &cli.func1)?;
    let func2_id = find_function_node_id(&graph, &cli.func2)?;

    warn!(
        "[storage-trace] Analyzing ordered storage access for: {}",
        cli.func1
    );
    let func1_accesses =
        analyze_ordered_storage_access_for_entry_point(&graph, func1_id, &ctx, &input)?;
    warn!(
        "[storage-trace] Analyzing ordered storage access for: {}",
        cli.func2
    );
    let func2_accesses =
        analyze_ordered_storage_access_for_entry_point(&graph, func2_id, &ctx, &input)?;

    warn!("[storage-trace] Formatting comparison table...");
    let markdown_output = format_comparison_table_to_markdown(
        &graph,
        &cli.func1,
        &func1_accesses,
        &cli.func2,
        &func2_accesses,
    );

    match cli.output_file {
        Some(ref path) => {
            let mut file = fs::File::create(path)
                .with_context(|| format!("Failed to create output file: {}", path.display()))?;
            file.write_all(markdown_output.as_bytes())
                .with_context(|| format!("Failed to write to output file: {}", path.display()))?;
            warn!("Storage trace comparison written to: {}", path.display());
        }
        None => {
            let mut handle = stdout().lock();
            handle
                .write_all(markdown_output.as_bytes())
                .context("Failed to write output to stdout")?;
            handle.flush().context("Failed to flush stdout")?;
        }
    }

    Ok(())
}

fn find_sol_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>> {
    let mut sol_files = Vec::new();
    for path in paths {
        if path.is_dir() {
            for entry in walkdir::WalkDir::new(path)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                if entry.file_type().is_file()
                    && entry.path().extension().is_some_and(|ext| ext == "sol")
                {
                    sol_files.push(entry.path().to_path_buf());
                }
            }
        } else if path.is_file() && path.extension().is_some_and(|ext| ext == "sol") {
            sol_files.push(path.clone());
        } else if path.is_file() {
            // Silently ignore non-Solidity files
        } else {
            bail!("Path not found or invalid: {}", path.display());
        }
    }
    sol_files.sort();
    Ok(sol_files)
}

fn setup_manifest_and_bindings(
    ctx: &mut CallGraphGeneratorContext,
    manifest_file_arg: &Option<PathBuf>,
    bindings_arg: &Option<PathBuf>,
    input_paths: &[PathBuf],
    project_root: &Path,
) -> Result<()> {
    let mut manifest_loaded_from_file = false;
    if let Some(manifest_path_arg) = manifest_file_arg {
        let absolute_manifest_path = if manifest_path_arg.is_absolute() {
            manifest_path_arg.clone()
        } else {
            project_root.join(manifest_path_arg)
        };
        warn!(
            "[storage-trace] Attempting to load manifest from: {}",
            absolute_manifest_path.display()
        );
        match graph::manifest::load_manifest(&absolute_manifest_path) {
            Ok(loaded_manifest) => {
                warn!(
                    "[storage-trace] Manifest loaded successfully from file with {} entries.",
                    loaded_manifest.entries.len()
                );
                ctx.manifest = Some(loaded_manifest);
                manifest_loaded_from_file = true;
            }
            Err(e) => {
                warn!(
                    "Warning: Failed to load manifest from {}: {}. Will attempt to generate from source files.",
                    absolute_manifest_path.display(),
                    e
                );
            }
        }
    }

    if !manifest_loaded_from_file {
        warn!("[storage-trace] Generating manifest in-memory from source files.");
        let mut manifest_in_memory = Manifest::default();
        let sol_files_relative_for_manifest =
            find_solidity_files_for_manifest(input_paths, project_root).context(
                "Failed to find Solidity files relative to project root for manifest generation",
            )?;

        if sol_files_relative_for_manifest.is_empty() {
            warn!("[storage-trace] No Solidity files found for in-memory manifest generation.");
        } else {
            for relative_file_path in &sol_files_relative_for_manifest {
                let full_file_path = project_root.join(relative_file_path);
                match fs::read_to_string(&full_file_path) {
                    Ok(source_content) => match extract_source_comments(&source_content) {
                        Ok(source_comments) => {
                            let entries: Vec<ManifestEntry> = source_comments
                                .into_iter()
                                .map(|sc| ManifestEntry::from((sc, relative_file_path.clone())))
                                .collect();
                            manifest_in_memory.extend_entries(entries);
                        }
                        Err(e) => {
                            warn!(
                                "Warning: Failed to extract comments from {}: {}. Skipping file for manifest.",
                                relative_file_path.display(),
                                e
                            );
                        }
                    },
                    Err(e) => {
                        warn!(
                            "Warning: Failed to read file {} for manifest generation: {}",
                            full_file_path.display(),
                            e
                        );
                    }
                }
            }
            warn!(
                "[storage-trace] In-memory manifest generated with {} entries.",
                manifest_in_memory.entries.len()
            );
            if !manifest_in_memory.entries.is_empty() {
                ctx.manifest = Some(manifest_in_memory);
            }
        }
    }

    let mut binding_registry_option: Option<BindingRegistry> = None;
    if let Some(bindings_path) = bindings_arg {
        let absolute_bindings_path = if bindings_path.is_absolute() {
            bindings_path.clone()
        } else {
            project_root.join(bindings_path)
        };
        warn!(
            "[storage-trace] Attempting to load bindings from file: {}",
            absolute_bindings_path.display()
        );
        match BindingRegistry::load(&absolute_bindings_path) {
            Ok(registry) => {
                warn!(
                    "[storage-trace] BindingRegistry loaded successfully from file with {} keys.",
                    registry.bindings.len()
                );
                binding_registry_option = Some(registry);
            }
            Err(e) => {
                warn!(
                    "Warning: Failed to load bindings from file {}: {}. Proceeding with an empty or Natspec-derived registry.",
                    absolute_bindings_path.display(),
                    e
                );
            }
        }
    }
    if binding_registry_option.is_none() {
        warn!("[storage-trace] No bindings file loaded or specified. Initializing a default BindingRegistry.");
        binding_registry_option = Some(BindingRegistry::default());
    }
    if let Some(registry) = binding_registry_option.as_mut() {
        if let Some(ref manifest_content) = ctx.manifest {
            warn!("[storage-trace] Populating BindingRegistry from manifest Natspec...");
            registry.populate_from_manifest(manifest_content);
        }
    }
    ctx.binding_registry = binding_registry_option;
    Ok(())
}

fn find_function_node_id(graph: &CallGraph, full_name: &str) -> Result<NodeId> {
    let parts: Vec<&str> = full_name.split('.').collect();
    if parts.len() != 2 {
        bail!(
            "Invalid function name format: '{}'. Expected ContractName.FunctionName",
            full_name
        );
    }
    let contract_name = parts[0];
    let function_name = parts[1];

    graph
        .iter_nodes()
        .find(|node| {
            node.name == function_name
                && node.contract_name.as_deref() == Some(contract_name)
                && node.node_type == graph::cg::NodeType::Function
        })
        .map(|node| node.id)
        .ok_or_else(|| anyhow::anyhow!("Function '{}' not found in the call graph.", full_name))
}

fn analyze_ordered_storage_access_for_entry_point(
    graph: &CallGraph,
    entry_point_node_id: NodeId,
    _ctx: &CallGraphGeneratorContext,
    _input: &CallGraphGeneratorInput,
) -> Result<Vec<OrderedStorageAccessInfo>> {
    let analyzer = graph::reachability::ReachabilityAnalyzer::new();

    let is_function_like_node = |node: &Node| -> bool {
        matches!(
            node.node_type,
            graph::cg::NodeType::Function
                | graph::cg::NodeType::Modifier
                | graph::cg::NodeType::Constructor
        )
    };

    let process_node_for_ordered_storage =
        |func_node: &Node, state: &mut Vec<OrderedStorageAccessInfo>, current_graph: &CallGraph| {
            let mut direct_accesses: Vec<OrderedStorageAccessInfo> = Vec::new();
            for edge in &current_graph.edges {
                if edge.source_node_id == func_node.id {
                    if let Some(target_node) = current_graph.nodes.get(edge.target_node_id) {
                        if target_node.node_type == graph::cg::NodeType::StorageVariable {
                            match edge.edge_type {
                                graph::cg::EdgeType::StorageWrite => {
                                    let operation_text = _input.source
                                        [edge.call_site_span.0..edge.call_site_span.1]
                                        .to_string();
                                    direct_accesses.push(OrderedStorageAccessInfo {
                                        access_type: edge.edge_type.clone(),
                                        variable_node_id: target_node.id,
                                        operation_text,
                                        _operation_span: edge.call_site_span,
                                    });
                                }
                                graph::cg::EdgeType::StorageRead => {
                                    let var_node = _input.tree.root_node()
                                        .descendant_for_byte_range(edge.call_site_span.0, edge.call_site_span.1)
                                        .ok_or_else(|| anyhow::anyhow!("Failed to find AST node for variable read at span {:?}", edge.call_site_span)).unwrap();

                                    let mut display_span = edge.call_site_span;
                                    let mut context_found = false;
                                    let max_depth = 7; 

                                    // 1. Assignment (RHS)
                                    if !context_found {
                                        if let Some(ancestor) = find_ancestor_of_kind(var_node, "assignment_expression", max_depth) {
                                            if let Some(rhs_node) = ancestor.child_by_field_name("right") {
                                                if is_node_or_descendant(var_node, rhs_node) {
                                                    display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                    context_found = true;
                                                }
                                            }
                                        }
                                    }

                                    // 2. Variable Declaration (Initializer)
                                    if !context_found {
                                        if let Some(ancestor) = find_ancestor_of_kind(var_node, "variable_declaration_statement", max_depth) {
                                            let var_decl_node_opt = if ancestor.kind() == "variable_declaration" { // ancestor could be var_decl itself
                                                Some(ancestor)
                                            } else { // or var_decl_statement containing var_decl
                                                ancestor.named_child(0).filter(|c| c.kind() == "variable_declaration")
                                            };
                                            if let Some(var_decl_node) = var_decl_node_opt {
                                                if let Some(initializer_node) = var_decl_node.child_by_field_name("value") {
                                                    if is_node_or_descendant(var_node, initializer_node) {
                                                        display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                        context_found = true;
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    // 3. Return Statement
                                    if !context_found {
                                        if let Some(ancestor) = find_ancestor_of_kind(var_node, "return_statement", max_depth) {
                                            if is_node_or_descendant(var_node, ancestor) {
                                                display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                context_found = true;
                                            }
                                        }
                                    }

                                    // 4. Function Call / Emit / Require / Assert Argument
                                    if !context_found {
                                        if let Some(ancestor) = find_ancestor_of_kind(var_node, "call_expression", max_depth) {
                                            if let Some(args_list_node) = ancestor.child_by_field_name("arguments") {
                                                if is_node_or_descendant(var_node, args_list_node) {
                                                    display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                    context_found = true;
                                                }
                                            }
                                        } else if let Some(ancestor) = find_ancestor_of_kind(var_node, "emit_statement", max_depth) {
                                            if let Some(args_list_node) = ancestor.child_by_field_name("arguments") {
                                                if is_node_or_descendant(var_node, args_list_node) {
                                                    display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                    context_found = true;
                                                }
                                            }
                                        }
                                    }

                                    // 5. Binary Operation Operand
                                    if !context_found {
                                        if let Some(ancestor) = find_ancestor_of_kind(var_node, "binary_expression", max_depth) {
                                            if let (Some(left_node), Some(right_node)) = (ancestor.child_by_field_name("left"), ancestor.child_by_field_name("right")) {
                                                if is_node_or_descendant(var_node, left_node) || is_node_or_descendant(var_node, right_node) {
                                                    display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                    context_found = true;
                                                }
                                            }
                                        }
                                    }

                                    // 6. Unary Operation Operand
                                    if !context_found {
                                        if let Some(ancestor) = find_ancestor_of_kind(var_node, "unary_expression", max_depth) {
                                            if let Some(arg_node) = ancestor.child_by_field_name("argument") {
                                                if is_node_or_descendant(var_node, arg_node) {
                                                    display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                    context_found = true;
                                                }
                                            }
                                        }
                                    }

                                    // 7. If/While/For Condition
                                    if !context_found {
                                        for kind in ["if_statement", "while_statement", "for_statement"] {
                                            if let Some(ancestor) = find_ancestor_of_kind(var_node, kind, max_depth) {
                                                if let Some(condition_node) = ancestor.child_by_field_name("condition") {
                                                    if is_node_or_descendant(var_node, condition_node) {
                                                        display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                        context_found = true;
                                                        break;
                                                    }
                                                }
                                                // For 'for_statement', could also check 'initialization' and 'update' if relevant
                                                if kind == "for_statement" {
                                                    if let Some(init_node) = ancestor.child_by_field_name("initialization") {
                                                        if is_node_or_descendant(var_node, init_node) {
                                                            display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                            context_found = true;
                                                            break;
                                                        }
                                                    }
                                                    if let Some(update_node) = ancestor.child_by_field_name("update") {
                                                        if is_node_or_descendant(var_node, update_node) {
                                                            display_span = (ancestor.start_byte(), ancestor.end_byte());
                                                            context_found = true;
                                                            break;
                                                        }
                                                    }
                                                }
                                            }
                                            if context_found { break; }
                                        }
                                    }
                                    
                                    // Fallback: Try to find the closest useful parent expression or statement.
                                    if !context_found {
                                        let mut best_fallback_span = display_span; 
                                        let mut temp_node = var_node;
                                        
                                        for i in 0..5 { 
                                            if let Some(parent) = temp_node.parent() {
                                                let parent_span = (parent.start_byte(), parent.end_byte());
                                                if parent_span == (var_node.byte_range().start, var_node.byte_range().end) { // Parent is same span as var_node, ascend further
                                                    temp_node = parent;
                                                    continue;
                                                }

                                                match parent.kind() {
                                                    "expression_statement" | 
                                                    "variable_declaration_statement" | 
                                                    "return_statement" |
                                                    "if_statement" | 
                                                    "for_statement" | 
                                                    "while_statement" => {
                                                        best_fallback_span = parent_span;
                                                        break; 
                                                    }
                                                    "expression" | 
                                                    "binary_expression" | "unary_expression" | 
                                                    "call_expression" | "member_expression" => {
                                                        best_fallback_span = parent_span;
                                                        // Don't break immediately, a statement parent might be one level higher
                                                        // and preferred if the loop continues.
                                                        // However, for simplicity, let's break if we find a decent expression.
                                                        // If a statement is found in a subsequent iteration, it will overwrite this.
                                                        if i > 0 { // If this expression is not the immediate parent, it's likely good enough
                                                            break;
                                                        }
                                                    }
                                                    // Stop at structural boundaries without taking them as context
                                                    "block" | "function_definition" | "modifier_definition" |
                                                    "contract_definition" | "source_file" => {
                                                        break; 
                                                    }
                                                    _ => {} // Other kinds, continue ascending
                                                }
                                                temp_node = parent;
                                            } else {
                                                break; // Reached root
                                            }
                                        }
                                        display_span = best_fallback_span;
                                    }

                                    let operation_text = _input.source[display_span.0..display_span.1].to_string();
                                    direct_accesses.push(OrderedStorageAccessInfo {
                                        access_type: edge.edge_type.clone(),
                                        variable_node_id: target_node.id,
                                        operation_text,
                                        _operation_span: display_span,
                                    });
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            direct_accesses.sort_by_key(|access_info| {
                // The sorting key MUST use the original variable access span to maintain correct order.
                // We find the original edge that corresponds to this access_info.
                // The access_info.operation_span might be wider now, but the fundamental access point is the key.
                graph
                    .edges
                    .iter()
                    .find(|e| {
                        e.source_node_id == func_node.id
                            && e.target_node_id == access_info.variable_node_id
                            && e.edge_type == access_info.access_type
                    })
                    .map_or(0, |e| e.call_site_span.0)
            });
            state.extend(direct_accesses);
        };

    let mut single_entry_map: HashMap<NodeId, Vec<OrderedStorageAccessInfo>> = HashMap::new();
    let mut initial_state = Vec::new();
    let mut visited_functions = std::collections::HashSet::new();

    analyzer.dfs_traverse(
        entry_point_node_id,
        graph,
        &is_function_like_node,
        &process_node_for_ordered_storage,
        &mut initial_state,
        &mut visited_functions,
    );
    single_entry_map.insert(entry_point_node_id, initial_state);

    Ok(single_entry_map
        .remove(&entry_point_node_id)
        .unwrap_or_default())
}

fn format_comparison_table_to_markdown(
    graph: &CallGraph,
    func1_name: &str,
    func1_accesses: &[OrderedStorageAccessInfo],
    func2_name: &str,
    func2_accesses: &[OrderedStorageAccessInfo],
) -> String {
    let mut md = String::new();
    md.push_str(&format!(
        "| Storage Variable | {} | {} |\n",
        func1_name, func2_name
    ));
    md.push_str("|------------------|--------------------------|--------------------------|\n");

    let mut all_vars: std::collections::HashMap<NodeId, String> = std::collections::HashMap::new();
    for acc in func1_accesses.iter().chain(func2_accesses.iter()) {
        all_vars.entry(acc.variable_node_id).or_insert_with(|| {
            let var_node = graph.nodes.get(acc.variable_node_id).unwrap();
            let var_full_name = format!(
                "{}.{}",
                var_node.contract_name.as_deref().unwrap_or("Global"),
                var_node.name
            );
            var_full_name
        });
    }

    let mut sorted_vars: Vec<(NodeId, String)> = all_vars.into_iter().collect();
    sorted_vars.sort_by(|a, b| a.1.cmp(&b.1));

    for (var_id, var_full_name) in sorted_vars {
        let mut func1_ops_str = String::new();
        for acc in func1_accesses.iter() { 
            if acc.variable_node_id == var_id {
                if !func1_ops_str.is_empty() {
                    func1_ops_str.push_str(", ");
                }
                let op_char = if acc.access_type == graph::cg::EdgeType::StorageRead {
                    "R"
                } else {
                    "W"
                };
                let cleaned_op_text = acc.operation_text.trim().replace('\n', " ");
                func1_ops_str.push_str(&format!("**{}**: `{}`", op_char, cleaned_op_text));
            }
        }
        if func1_ops_str.is_empty() {
            func1_ops_str.push('-');
        }

        let mut func2_ops_str = String::new();
        for acc in func2_accesses.iter() { 
            if acc.variable_node_id == var_id {
                if !func2_ops_str.is_empty() {
                    func2_ops_str.push_str(", ");
                }
                let op_char = if acc.access_type == graph::cg::EdgeType::StorageRead {
                    "R"
                } else {
                    "W"
                };
                let cleaned_op_text = acc.operation_text.trim().replace('\n', " ");
                func2_ops_str.push_str(&format!("**{}**: `{}`", op_char, cleaned_op_text));
            }
        }
        if func2_ops_str.is_empty() {
            func2_ops_str.push('-');
        }

        md.push_str(&format!(
            "| {} | {} | {} |\n",
            var_full_name, func1_ops_str, func2_ops_str
        ));
    }

    md
}
