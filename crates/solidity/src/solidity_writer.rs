//! Writes a Solidity AST to textual Solidity source code format.

use crate::ast::*;
use std::fmt::Write;

const INDENT_STEP: &str = "    ";

pub fn write_source_unit(source_unit: &SourceUnit) -> String {
    let mut output = String::new();
    let mut indent = String::new(); // Start with no indentation

    for item in &source_unit.items {
        write_source_unit_item(&mut output, item, &mut indent);
        writeln!(output).unwrap(); 
    }

    output.trim_end().to_string() 
}

fn write_source_unit_item(output: &mut String, item: &SourceUnitItem, indent: &mut String) {
    match item {
        SourceUnitItem::Pragma(pragma) => write_pragma(output, pragma, indent),
        SourceUnitItem::Import(import) => write_import(output, import, indent),
        SourceUnitItem::Contract(contract) => write_contract(output, contract, indent),
        SourceUnitItem::Interface(interface) => write_interface(output, interface, indent),
        SourceUnitItem::Library(library) => write_library(output, library, indent),
        SourceUnitItem::Function(function) => write_function(output, function, indent),
        SourceUnitItem::ConstantVariable(var) => write_constant_variable(output, var, indent),
        SourceUnitItem::Struct(struct_def) => write_struct(output, struct_def, indent),
        SourceUnitItem::Enum(enum_def) => write_enum(output, enum_def, indent),
        SourceUnitItem::Error(error_def) => write_error(output, error_def, indent),
        SourceUnitItem::Event(event_def) => write_event(output, event_def, indent),
        SourceUnitItem::Using(using_def) => write_using(output, using_def, indent),
    }
}

fn write_pragma(output: &mut String, pragma: &PragmaDirective, indent: &str) {
    write!(output, "{}pragma", indent).unwrap();
    for token in &pragma.tokens {
        write!(output, " {}", token).unwrap();
    }
    writeln!(output, ";").unwrap();
}

fn write_import(output: &mut String, import: &ImportDirective, indent: &str) {
    write!(output, "{}import", indent).unwrap();

    if let Some(symbols) = &import.symbols {
        write!(output, " {{").unwrap();
        for (i, symbol) in symbols.iter().enumerate() {
            if i > 0 {
                write!(output, ", ").unwrap();
            }
            write!(output, "{}", symbol.name).unwrap();
            if let Some(alias) = &symbol.alias {
                write!(output, " as {}", alias).unwrap();
            }
        }
        write!(output, "}}").unwrap();
    } else if let Some(alias) = &import.alias {
        write!(output, " * as {}", alias).unwrap();
    }

    write!(output, " from \"{}\"", import.path).unwrap();
    writeln!(output, ";").unwrap();
}

fn write_contract(output: &mut String, contract: &ContractDefinition, indent: &mut String) {
    if contract.is_abstract {
        write!(output, "{}abstract ", indent).unwrap();
    }
    write!(output, "contract {}", contract.name).unwrap();

    if !contract.inheritance.is_empty() {
        write!(output, " is ").unwrap();
        for (i, inheritance) in contract.inheritance.iter().enumerate() {
            if i > 0 {
                write!(output, ", ").unwrap();
            }
            write_identifier_path(output, &inheritance.name);
            if let Some(args) = &inheritance.arguments {
                write!(output, "(").unwrap();
                write_expression_list(output, args);
                write!(output, ")").unwrap();
            }
        }
    }

    writeln!(output, " {{").unwrap();

    indent.push_str(INDENT_STEP);
    for element in &contract.body {
        write_contract_body_element(output, element, indent);
        writeln!(output).unwrap();
    }
    indent.truncate(indent.len() - INDENT_STEP.len());

    writeln!(output, "{}}}", indent).unwrap();
}

fn write_interface(output: &mut String, interface: &InterfaceDefinition, indent: &mut String) {
    write!(output, "{}interface {}", indent, interface.name).unwrap();

    if !interface.inheritance.is_empty() {
        write!(output, " is ").unwrap();
        for (i, inheritance) in interface.inheritance.iter().enumerate() {
            if i > 0 {
                write!(output, ", ").unwrap();
            }
            write_identifier_path(output, &inheritance.name);
            if let Some(args) = &inheritance.arguments {
                write!(output, "(").unwrap();
                write_expression_list(output, args);
                write!(output, ")").unwrap();
            }
        }
    }

    writeln!(output, " {{").unwrap();

    indent.push_str(INDENT_STEP);
    for element in &interface.body {
        write_contract_body_element(output, element, indent);
        writeln!(output).unwrap();
    }
    indent.truncate(indent.len() - INDENT_STEP.len());

    writeln!(output, "{}}}", indent).unwrap();
}

fn write_library(output: &mut String, library: &LibraryDefinition, indent: &mut String) {
    write!(output, "{}library {}", indent, library.name).unwrap();
    writeln!(output, " {{").unwrap();

    indent.push_str(INDENT_STEP);
    for element in &library.body {
        write_contract_body_element(output, element, indent);
        writeln!(output).unwrap();
    }
    indent.truncate(indent.len() - INDENT_STEP.len());

    writeln!(output, "{}}}", indent).unwrap();
}

fn write_contract_body_element(
    output: &mut String,
    element: &ContractBodyElement,
    indent: &mut String,
) {
    match element {
        ContractBodyElement::Constructor(constructor) => {
            write_constructor(output, constructor, indent)
        }
        ContractBodyElement::Function(function) => write_function(output, function, indent),
        ContractBodyElement::Modifier(modifier) => write_modifier(output, modifier, indent),
        ContractBodyElement::StateVariable(var) => write_state_variable(output, var, indent),
        ContractBodyElement::Event(event) => write_event(output, event, indent),
        ContractBodyElement::Error(error) => write_error(output, error, indent),
        ContractBodyElement::Struct(struct_def) => write_struct(output, struct_def, indent),
        ContractBodyElement::Enum(enum_def) => write_enum(output, enum_def, indent),
        ContractBodyElement::Using(using_def) => write_using(output, using_def, indent),
    }
}

fn write_function(output: &mut String, function: &FunctionDefinition, indent: &str) {
    write!(output, "{}function", indent).unwrap();

    if let Some(name) = &function.name {
        write!(output, " {}", name).unwrap();
    }

    write!(output, "(").unwrap();
    write_parameter_list(output, &function.parameters);
    write!(output, ")").unwrap();

    if let Some(visibility) = &function.visibility {
        write!(output, " {}", visibility.to_string()).unwrap();
    }

    if let Some(state_mutability) = &function.state_mutability {
        write!(output, " {}", state_mutability.to_string()).unwrap();
    }

    if function.is_virtual {
        write!(output, " virtual").unwrap();
    }

    if let Some(override_spec) = &function.override_specifier {
        write!(output, " override").unwrap();
        if !override_spec.overrides.is_empty() {
            write!(output, "(").unwrap();
            for (i, path) in override_spec.overrides.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                write_identifier_path(output, path);
            }
            write!(output, ")").unwrap();
        }
    }

    for modifier in &function.modifiers {
        write!(output, " ").unwrap();
        write_identifier_path(output, &modifier.name);
        if let Some(args) = &modifier.arguments {
            write!(output, "(").unwrap();
            write_expression_list(output, args);
            write!(output, ")").unwrap();
        }
    }

    if let Some(returns) = &function.returns {
        write!(output, " returns (").unwrap();
        write_parameter_list(output, returns);
        write!(output, ")").unwrap();
    }

    if let Some(body) = &function.body {
        write!(output, " ").unwrap();
        write_block(output, body, &mut indent.to_string());
    } else {
        writeln!(output, ";").unwrap();
    }
}

fn write_constructor(output: &mut String, constructor: &ConstructorDefinition, indent: &str) {
    write!(output, "{}constructor(", indent).unwrap();
    write_parameter_list(output, &constructor.parameters);
    write!(output, ")").unwrap();

    if let Some(visibility) = &constructor.visibility {
        write!(output, " {}", visibility.to_string()).unwrap();
    }

    if constructor.is_payable {
        write!(output, " payable").unwrap();
    }

    for modifier in &constructor.modifiers {
        write!(output, " ").unwrap();
        write_identifier_path(output, &modifier.name);
        if let Some(args) = &modifier.arguments {
            write!(output, "(").unwrap();
            write_expression_list(output, args);
            write!(output, ")").unwrap();
        }
    }

    write!(output, " ").unwrap();
    write_block(output, &constructor.body, &mut indent.to_string());
}

fn write_modifier(output: &mut String, modifier: &ModifierDefinition, indent: &str) {
    write!(output, "{}modifier {}(", indent, modifier.name).unwrap();
    write_parameter_list(output, &modifier.parameters);
    write!(output, ")").unwrap();

    if modifier.is_virtual {
        write!(output, " virtual").unwrap();
    }

    if let Some(override_spec) = &modifier.override_specifier {
        write!(output, " override").unwrap();
        if !override_spec.overrides.is_empty() {
            write!(output, "(").unwrap();
            for (i, path) in override_spec.overrides.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                write_identifier_path(output, path);
            }
            write!(output, ")").unwrap();
        }
    }

    if let Some(body) = &modifier.body {
        write!(output, " ").unwrap();
        write_block(output, body, &mut indent.to_string());
    } else {
        writeln!(output, ";").unwrap();
    }
}

fn write_state_variable(output: &mut String, var: &StateVariableDeclaration, indent: &str) {
    write!(output, "{}", indent).unwrap();
    write_type_name(output, &var.type_name);

    if let Some(visibility) = &var.visibility {
        write!(output, " {}", visibility.to_string()).unwrap();
    }

    if var.is_constant {
        write!(output, " constant").unwrap();
    }

    if var.is_immutable {
        write!(output, " immutable").unwrap();
    }

    if var.is_transient {
        write!(output, " transient").unwrap();
    }

    if let Some(override_spec) = &var.override_specifier {
        write!(output, " override").unwrap();
        if !override_spec.overrides.is_empty() {
            write!(output, "(").unwrap();
            for (i, path) in override_spec.overrides.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                write_identifier_path(output, path);
            }
            write!(output, ")").unwrap();
        }
    }

    write!(output, " {}", var.name).unwrap();

    if let Some(initial_value) = &var.initial_value {
        write!(output, " = ").unwrap();
        write_expression(output, initial_value);
    }

    writeln!(output, ";").unwrap();
}

fn write_constant_variable(output: &mut String, var: &ConstantVariableDeclaration, indent: &str) {
    write!(output, "{}", indent).unwrap();
    write_type_name(output, &var.type_name);
    write!(output, " constant {} = ", var.name).unwrap();
    write_expression(output, &var.initial_value);
    writeln!(output, ";").unwrap();
}

fn write_struct(output: &mut String, struct_def: &StructDefinition, indent: &str) {
    writeln!(output, "{}struct {} {{", indent, struct_def.name).unwrap();

    let mut inner_indent = indent.to_string();
    inner_indent.push_str(INDENT_STEP);

    for member in &struct_def.members {
        write!(output, "{}", inner_indent).unwrap();
        write_type_name(output, &member.type_name);
        writeln!(output, " {};", member.name).unwrap();
    }

    writeln!(output, "{}}}", indent).unwrap();
}

fn write_enum(output: &mut String, enum_def: &EnumDefinition, indent: &str) {
    write!(output, "{}enum {} {{ ", indent, enum_def.name).unwrap();
    for (i, value) in enum_def.values.iter().enumerate() {
        if i > 0 {
            write!(output, ", ").unwrap();
        }
        write!(output, "{}", value).unwrap();
    }
    writeln!(output, " }}").unwrap();
}

/// Writes an event definition.
fn write_event(output: &mut String, event: &EventDefinition, indent: &str) {
    write!(output, "{}event {}(", indent, event.name).unwrap();

    for (i, param) in event.parameters.iter().enumerate() {
        if i > 0 {
            write!(output, ", ").unwrap();
        }
        write_type_name(output, &param.type_name);
        if param.is_indexed {
            write!(output, " indexed").unwrap();
        }
        if let Some(name) = &param.name {
            write!(output, " {}", name).unwrap();
        }
    }

    write!(output, ")").unwrap();

    if event.is_anonymous {
        write!(output, " anonymous").unwrap();
    }

    writeln!(output, ";").unwrap();
}

fn write_error(output: &mut String, error: &ErrorDefinition, indent: &str) {
    write!(output, "{}error {}(", indent, error.name).unwrap();

    for (i, param) in error.parameters.iter().enumerate() {
        if i > 0 {
            write!(output, ", ").unwrap();
        }
        write_type_name(output, &param.type_name);
        if let Some(name) = &param.name {
            write!(output, " {}", name).unwrap();
        }
    }

    writeln!(output, ");").unwrap();
}

fn write_using(output: &mut String, using: &UsingDirective, indent: &str) {
    write!(output, "{}using ", indent).unwrap();
    write_identifier_path(output, &using.library);
    write!(output, " for").unwrap();

    if let Some(type_name) = &using.type_name {
        write!(output, " ").unwrap();
        write_type_name(output, type_name);
    } else {
        write!(output, " *").unwrap();
    }

    if using.is_global {
        write!(output, " global").unwrap();
    }

    writeln!(output, ";").unwrap();
}

fn write_parameter_list(output: &mut String, parameters: &[Parameter]) {
    for (i, param) in parameters.iter().enumerate() {
        if i > 0 {
            write!(output, ", ").unwrap();
        }
        write_type_name(output, &param.type_name);
        if let Some(data_location) = &param.data_location {
            write!(output, " {}", data_location.to_string()).unwrap();
        }
        if let Some(name) = &param.name {
            write!(output, " {}", name).unwrap();
        }
    }
}

fn write_type_name(output: &mut String, type_name: &TypeName) {
    match type_name {
        TypeName::Elementary(elem) => write_elementary_type(output, elem),
        TypeName::UserDefined(path) => write_identifier_path(output, path),
        TypeName::Array(base_type, size) => {
            write_type_name(output, base_type);
            write!(output, "[").unwrap();
            if let Some(size_expr) = size {
                write_expression(output, size_expr);
            }
            write!(output, "]").unwrap();
        }
        TypeName::Mapping(mapping) => {
            write!(output, "mapping(").unwrap();
            write_type_name(output, &mapping.key_type);
            if let Some(key_name) = &mapping.key_name {
                write!(output, " {}", key_name).unwrap();
            }
            write!(output, " => ").unwrap();
            write_type_name(output, &mapping.value_type);
            if let Some(value_name) = &mapping.value_name {
                write!(output, " {}", value_name).unwrap();
            }
            write!(output, ")").unwrap();
        }
        TypeName::Function(func_type) => {
            write!(output, "function(").unwrap();
            write_parameter_list(output, &func_type.parameters);
            write!(output, ")").unwrap();

            if let Some(visibility) = &func_type.visibility {
                write!(output, " {}", visibility.to_string()).unwrap();
            }

            if let Some(state_mutability) = &func_type.state_mutability {
                write!(output, " {}", state_mutability.to_string()).unwrap();
            }

            if let Some(returns) = &func_type.returns {
                write!(output, " returns (").unwrap();
                write_parameter_list(output, returns);
                write!(output, ")").unwrap();
            }
        }
    }
}

fn write_elementary_type(output: &mut String, elem_type: &ElementaryTypeName) {
    match elem_type {
        ElementaryTypeName::Address => write!(output, "address").unwrap(),
        ElementaryTypeName::AddressPayable => write!(output, "address payable").unwrap(),
        ElementaryTypeName::Bool => write!(output, "bool").unwrap(),
        ElementaryTypeName::String => write!(output, "string").unwrap(),
        ElementaryTypeName::Bytes => write!(output, "bytes").unwrap(),
        ElementaryTypeName::SignedInteger(size) => {
            write!(output, "int").unwrap();
            if let Some(bits) = size {
                write!(output, "{}", bits).unwrap();
            }
        }
        ElementaryTypeName::UnsignedInteger(size) => {
            write!(output, "uint").unwrap();
            if let Some(bits) = size {
                write!(output, "{}", bits).unwrap();
            }
        }
        ElementaryTypeName::FixedBytes(size) => {
            write!(output, "bytes").unwrap();
            if let Some(bytes) = size {
                write!(output, "{}", bytes).unwrap();
            }
        }
        ElementaryTypeName::Fixed => write!(output, "fixed").unwrap(),
        ElementaryTypeName::Ufixed => write!(output, "ufixed").unwrap(),
    }
}

fn write_identifier_path(output: &mut String, path: &IdentifierPath) {
    for (i, part) in path.parts.iter().enumerate() {
        if i > 0 {
            write!(output, ".").unwrap();
        }
        write!(output, "{}", part).unwrap();
    }
}

fn write_expression_list(output: &mut String, expressions: &[Expression]) {
    for (i, expr) in expressions.iter().enumerate() {
        if i > 0 {
            write!(output, ", ").unwrap();
        }
        write_expression(output, expr);
    }
}

fn write_expression(output: &mut String, expression: &Expression) {
    match expression {
        Expression::Identifier(name) => write!(output, "{}", name).unwrap(),
        Expression::Literal(literal) => write_literal(output, literal),
        Expression::Binary(binary) => {
            write_expression(output, &binary.left);
            write!(output, " {} ", binary.operator.to_string()).unwrap();
            write_expression(output, &binary.right);
        }
        Expression::Unary(unary) => {
            if unary.is_prefix {
                write!(output, "{}", unary.operator.to_string()).unwrap();
                write_expression(output, &unary.operand);
            } else {
                write_expression(output, &unary.operand);
                write!(output, "{}", unary.operator.to_string()).unwrap();
            }
        }
        Expression::FunctionCall(call) => {
            write_expression(output, &call.function);
            write!(output, "(").unwrap();
            write_expression_list(output, &call.arguments);
            write!(output, ")").unwrap();
        }
        Expression::MemberAccess(access) => {
            write_expression(output, &access.object);
            write!(output, ".{}", access.member).unwrap();
        }
        Expression::IndexAccess(access) => {
            write_expression(output, &access.object);
            write!(output, "[").unwrap();
            if let Some(index) = &access.index {
                write_expression(output, index);
            }
            write!(output, "]").unwrap();
        }
        Expression::Conditional(conditional) => {
            write_expression(output, &conditional.condition);
            write!(output, " ? ").unwrap();
            write_expression(output, &conditional.true_expr);
            write!(output, " : ").unwrap();
            write_expression(output, &conditional.false_expr);
        }
        Expression::Assignment(assignment) => {
            write_expression(output, &assignment.left);
            write!(output, " {} ", assignment.operator.to_string()).unwrap();
            write_expression(output, &assignment.right);
        }
        Expression::Tuple(tuple) => {
            write!(output, "(").unwrap();
            for (i, element) in tuple.elements.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                if let Some(expr) = element {
                    write_expression(output, expr);
                }
            }
            write!(output, ")").unwrap();
        }
        Expression::Array(array) => {
            write!(output, "[").unwrap();
            write_expression_list(output, &array.elements);
            write!(output, "]").unwrap();
        }
        Expression::TypeConversion(conversion) => {
            write_type_name(output, &conversion.type_name);
            write!(output, "(").unwrap();
            write_expression(output, &conversion.expression);
            write!(output, ")").unwrap();
        }
        Expression::New(new_expr) => {
            write!(output, "new ").unwrap();
            write_type_name(output, &new_expr.type_name);
        }
    }
}

fn write_literal(output: &mut String, literal: &Literal) {
    match literal {
        Literal::Boolean(value) => write!(output, "{}", value).unwrap(),
        Literal::Number(number) => {
            write!(output, "{}", number.value).unwrap();
            if let Some(sub_denom) = &number.sub_denomination {
                write!(output, " {}", sub_denom).unwrap();
            }
        }
        Literal::String(string) => write!(output, "\"{}\"", string.value).unwrap(),
        Literal::HexString(hex_string) => write!(output, "hex\"{}\"", hex_string.value).unwrap(),
        Literal::UnicodeString(unicode_string) => {
            write!(output, "unicode\"{}\"", unicode_string.value).unwrap()
        }
    }
}

fn write_block(output: &mut String, block: &Block, indent: &mut String) {
    writeln!(output, "{{").unwrap();

    indent.push_str(INDENT_STEP);
    for statement in &block.statements {
        write_statement(output, statement, indent);
    }
    indent.truncate(indent.len() - INDENT_STEP.len());

    writeln!(output, "{}}}", indent).unwrap();
}

/// Writes a statement.
fn write_statement(output: &mut String, statement: &Statement, indent: &str) {
    match statement {
        Statement::Block(block) => {
            write!(output, "{}", indent).unwrap();
            write_block(output, block, &mut indent.to_string());
        }
        Statement::Expression(expr_stmt) => {
            write!(output, "{}", indent).unwrap();
            write_expression(output, &expr_stmt.expression);
            writeln!(output, ";").unwrap();
        }
        Statement::Variable(var_stmt) => {
            write!(output, "{}", indent).unwrap();
            write_type_name(output, &var_stmt.declaration.type_name);
            if let Some(data_location) = &var_stmt.declaration.data_location {
                write!(output, " {}", data_location.to_string()).unwrap();
            }
            write!(output, " {}", var_stmt.declaration.name).unwrap();
            if let Some(initial_value) = &var_stmt.initial_value {
                write!(output, " = ").unwrap();
                write_expression(output, initial_value);
            }
            writeln!(output, ";").unwrap();
        }
        Statement::If(if_stmt) => {
            write!(output, "{}if (", indent).unwrap();
            write_expression(output, &if_stmt.condition);
            write!(output, ") ").unwrap();
            write_statement(output, &if_stmt.then_statement, "");
            if let Some(else_stmt) = &if_stmt.else_statement {
                write!(output, " else ").unwrap();
                write_statement(output, else_stmt, "");
            }
        }
        Statement::For(for_stmt) => {
            write!(output, "{}for (", indent).unwrap();
            if let Some(init) = &for_stmt.init {
                write_statement(output, init, "");
            } else {
                write!(output, ";").unwrap();
            }
            write!(output, " ").unwrap();
            if let Some(condition) = &for_stmt.condition {
                write_expression(output, condition);
            }
            write!(output, "; ").unwrap();
            if let Some(update) = &for_stmt.update {
                write_expression(output, update);
            }
            write!(output, ") ").unwrap();
            write_statement(output, &for_stmt.body, "");
        }
        Statement::While(while_stmt) => {
            write!(output, "{}while (", indent).unwrap();
            write_expression(output, &while_stmt.condition);
            write!(output, ") ").unwrap();
            write_statement(output, &while_stmt.body, "");
        }
        Statement::DoWhile(do_while_stmt) => {
            write!(output, "{}do ", indent).unwrap();
            write_statement(output, &do_while_stmt.body, "");
            write!(output, " while (").unwrap();
            write_expression(output, &do_while_stmt.condition);
            writeln!(output, ");").unwrap();
        }
        Statement::Continue(_) => writeln!(output, "{}continue;", indent).unwrap(),
        Statement::Break(_) => writeln!(output, "{}break;", indent).unwrap(),
        Statement::Return(return_stmt) => {
            write!(output, "{}return", indent).unwrap();
            if let Some(expr) = &return_stmt.expression {
                write!(output, " ").unwrap();
                write_expression(output, expr);
            }
            writeln!(output, ";").unwrap();
        }
        Statement::Emit(emit_stmt) => {
            write!(output, "{}emit ", indent).unwrap();
            write_expression(output, &emit_stmt.event_call);
            writeln!(output, ";").unwrap();
        }
        Statement::Revert(revert_stmt) => {
            write!(output, "{}revert ", indent).unwrap();
            write_expression(output, &revert_stmt.error_call);
            writeln!(output, ";").unwrap();
        }
        Statement::Try(try_stmt) => {
            write!(output, "{}try ", indent).unwrap();
            write_expression(output, &try_stmt.expression);
            if let Some(returns) = &try_stmt.returns {
                write!(output, " returns (").unwrap();
                write_parameter_list(output, returns);
                write!(output, ")").unwrap();
            }
            write!(output, " ").unwrap();
            write_block(output, &try_stmt.body, &mut indent.to_string());
            for catch_clause in &try_stmt.catch_clauses {
                write!(output, " catch").unwrap();
                if let Some(identifier) = &catch_clause.identifier {
                    write!(output, " {}", identifier).unwrap();
                }
                if let Some(parameters) = &catch_clause.parameters {
                    write!(output, "(").unwrap();
                    write_parameter_list(output, parameters);
                    write!(output, ")").unwrap();
                }
                write!(output, " ").unwrap();
                write_block(output, &catch_clause.body, &mut indent.to_string());
            }
        }
        Statement::Assembly(assembly_stmt) => {
            write!(output, "{}assembly", indent).unwrap();
            if let Some(dialect) = &assembly_stmt.dialect {
                write!(output, " \"{}\"", dialect).unwrap();
            }
            for flag in &assembly_stmt.flags {
                write!(output, " (\"{}\"))", flag).unwrap();
            }
            write!(output, " ").unwrap();
            write_yul_block(output, &assembly_stmt.body, &mut indent.to_string());
        }
        Statement::Unchecked(unchecked_stmt) => {
            write!(output, "{}unchecked ", indent).unwrap();
            write_block(output, &unchecked_stmt.body, &mut indent.to_string());
        }
    }
}

fn write_yul_block(output: &mut String, block: &YulBlock, indent: &mut String) {
    writeln!(output, "{{").unwrap();

    indent.push_str(INDENT_STEP);
    for statement in &block.statements {
        write_yul_statement(output, statement, indent);
    }
    indent.truncate(indent.len() - INDENT_STEP.len());

    writeln!(output, "{}}}", indent).unwrap();
}

fn write_yul_statement(output: &mut String, statement: &YulStatement, indent: &str) {
    match statement {
        YulStatement::Block(block) => {
            write!(output, "{}", indent).unwrap();
            write_yul_block(output, block, &mut indent.to_string());
        }
        YulStatement::VariableDeclaration(var_decl) => {
            write!(output, "{}let ", indent).unwrap();
            for (i, var) in var_decl.variables.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                write!(output, "{}", var).unwrap();
            }
            if let Some(expr) = &var_decl.expression {
                write!(output, " := ").unwrap();
                write_yul_expression(output, expr);
            }
            writeln!(output).unwrap();
        }
        YulStatement::Assignment(assignment) => {
            write!(output, "{}", indent).unwrap();
            for (i, var) in assignment.variables.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                write_yul_path(output, var);
            }
            write!(output, " := ").unwrap();
            write_yul_expression(output, &assignment.expression);
            writeln!(output).unwrap();
        }
        YulStatement::FunctionCall(call) => {
            write!(output, "{}", indent).unwrap();
            write_yul_function_call(output, call);
            writeln!(output).unwrap();
        }
        YulStatement::If(if_stmt) => {
            write!(output, "{}if ", indent).unwrap();
            write_yul_expression(output, &if_stmt.condition);
            write!(output, " ").unwrap();
            write_yul_block(output, &if_stmt.body, &mut indent.to_string());
        }
        YulStatement::For(for_stmt) => {
            write!(output, "{}for ", indent).unwrap();
            write_yul_block(output, &for_stmt.init, &mut indent.to_string());
            write!(output, " ").unwrap();
            write_yul_expression(output, &for_stmt.condition);
            write!(output, " ").unwrap();
            write_yul_block(output, &for_stmt.post, &mut indent.to_string());
            write!(output, " ").unwrap();
            write_yul_block(output, &for_stmt.body, &mut indent.to_string());
        }
        YulStatement::Switch(switch_stmt) => {
            write!(output, "{}switch ", indent).unwrap();
            write_yul_expression(output, &switch_stmt.expression);
            writeln!(output).unwrap();
            for case in &switch_stmt.cases {
                write!(output, "{}case ", indent).unwrap();
                write_yul_literal(output, &case.value);
                write!(output, " ").unwrap();
                write_yul_block(output, &case.body, &mut indent.to_string());
            }
            if let Some(default) = &switch_stmt.default {
                write!(output, "{}default ", indent).unwrap();
                write_yul_block(output, default, &mut indent.to_string());
            }
        }
        YulStatement::FunctionDefinition(func_def) => {
            write!(output, "{}function {}(", indent, func_def.name).unwrap();
            for (i, param) in func_def.parameters.iter().enumerate() {
                if i > 0 {
                    write!(output, ", ").unwrap();
                }
                write!(output, "{}", param).unwrap();
            }
            write!(output, ")").unwrap();
            if !func_def.returns.is_empty() {
                write!(output, " -> ").unwrap();
                for (i, ret) in func_def.returns.iter().enumerate() {
                    if i > 0 {
                        write!(output, ", ").unwrap();
                    }
                    write!(output, "{}", ret).unwrap();
                }
            }
            write!(output, " ").unwrap();
            write_yul_block(output, &func_def.body, &mut indent.to_string());
        }
        YulStatement::Leave => writeln!(output, "{}leave", indent).unwrap(),
        YulStatement::Break => writeln!(output, "{}break", indent).unwrap(),
        YulStatement::Continue => writeln!(output, "{}continue", indent).unwrap(),
    }
}

fn write_yul_expression(output: &mut String, expression: &YulExpression) {
    match expression {
        YulExpression::Path(path) => write_yul_path(output, path),
        YulExpression::FunctionCall(call) => write_yul_function_call(output, call),
        YulExpression::Literal(literal) => write_yul_literal(output, literal),
    }
}

fn write_yul_path(output: &mut String, path: &YulPath) {
    for (i, part) in path.parts.iter().enumerate() {
        if i > 0 {
            write!(output, ".").unwrap();
        }
        write!(output, "{}", part).unwrap();
    }
}

fn write_yul_function_call(output: &mut String, call: &YulFunctionCall) {
    write!(output, "{}(", call.function).unwrap();
    for (i, arg) in call.arguments.iter().enumerate() {
        if i > 0 {
            write!(output, ", ").unwrap();
        }
        write_yul_expression(output, arg);
    }
    write!(output, ")").unwrap();
}

fn write_yul_literal(output: &mut String, literal: &YulLiteral) {
    match literal {
        YulLiteral::Number(value) => write!(output, "{}", value).unwrap(),
        YulLiteral::String(value) => write!(output, "\"{}\"", value).unwrap(),
        YulLiteral::HexNumber(value) => write!(output, "{}", value).unwrap(),
        YulLiteral::Boolean(value) => write!(output, "{}", value).unwrap(),
        YulLiteral::HexString(value) => write!(output, "hex\"{}\"", value).unwrap(),
    }
}

impl Visibility {
    fn to_string(&self) -> &'static str {
        match self {
            Visibility::Public => "public",
            Visibility::Private => "private",
            Visibility::Internal => "internal",
            Visibility::External => "external",
        }
    }
}

impl StateMutability {
    fn to_string(&self) -> &'static str {
        match self {
            StateMutability::Pure => "pure",
            StateMutability::View => "view",
            StateMutability::Payable => "payable",
        }
    }
}

impl DataLocation {
    fn to_string(&self) -> &'static str {
        match self {
            DataLocation::Memory => "memory",
            DataLocation::Storage => "storage",
            DataLocation::Calldata => "calldata",
        }
    }
}

impl BinaryOperator {
    fn to_string(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Mod => "%",
            BinaryOperator::Exp => "**",
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanOrEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanOrEqual => ">=",
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
            BinaryOperator::BitAnd => "&",
            BinaryOperator::BitOr => "|",
            BinaryOperator::BitXor => "^",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",
            BinaryOperator::ShiftRightArithmetic => ">>>",
        }
    }
}

impl UnaryOperator {
    fn to_string(&self) -> &'static str {
        match self {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::Not => "!",
            UnaryOperator::BitNot => "~",
            UnaryOperator::Increment => "++",
            UnaryOperator::Decrement => "--",
            UnaryOperator::Delete => "delete ",
        }
    }
}

impl AssignmentOperator {
    fn to_string(&self) -> &'static str {
        match self {
            AssignmentOperator::Assign => "=",
            AssignmentOperator::AddAssign => "+=",
            AssignmentOperator::SubAssign => "-=",
            AssignmentOperator::MulAssign => "*=",
            AssignmentOperator::DivAssign => "/=",
            AssignmentOperator::ModAssign => "%=",
            AssignmentOperator::BitAndAssign => "&=",
            AssignmentOperator::BitOrAssign => "|=",
            AssignmentOperator::BitXorAssign => "^=",
            AssignmentOperator::ShiftLeftAssign => "<<=",
            AssignmentOperator::ShiftRightAssign => ">>=",
            AssignmentOperator::ShiftRightArithmeticAssign => ">>>=",
        }
    }
}
