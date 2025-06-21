//! Builder for Solidity AST

use crate::ast::*;
use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum BuilderError {
    BuildError(String),
    Other(String),
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuilderError::BuildError(msg) => write!(f, "Build error: {}", msg),
            BuilderError::Other(msg) => write!(f, "Builder error: {}", msg),
        }
    }
}

impl Error for BuilderError {}

#[derive(Debug, Default, Clone)]
pub struct SolidityBuilder {
    items: Vec<SourceUnitItem>,
}

impl SolidityBuilder {
    pub fn new() -> Self {
        Default::default()
    }

    fn add_item(&mut self, item: SourceUnitItem) -> &mut Self {
        self.items.push(item);
        self
    }

    pub fn build(self) -> SourceUnit {
        SourceUnit { items: self.items }
    }

    pub fn item_count(&self) -> usize {
        self.items.len()
    }

    pub fn pragma(&mut self, name: impl Into<String>, value: impl Into<String>) -> &mut Self {
        let tokens = vec![name.into(), value.into()];
        self.add_item(SourceUnitItem::Pragma(PragmaDirective { tokens }))
    }

    pub fn import(&mut self, path: impl Into<String>) -> &mut Self {
        self.add_item(SourceUnitItem::Import(ImportDirective {
            path: path.into(),
            alias: None,
            symbols: None,
        }))
    }

    pub fn import_as(&mut self, path: impl Into<String>, alias: impl Into<String>) -> &mut Self {
        self.add_item(SourceUnitItem::Import(ImportDirective {
            path: path.into(),
            alias: Some(alias.into()),
            symbols: None,
        }))
    }

    pub fn import_symbols(
        &mut self,
        path: impl Into<String>,
        symbols: Vec<(String, Option<String>)>,
    ) -> &mut Self {
        let import_symbols = symbols
            .into_iter()
            .map(|(name, alias)| ImportSymbol { name, alias })
            .collect();

        self.add_item(SourceUnitItem::Import(ImportDirective {
            path: path.into(),
            alias: None,
            symbols: Some(import_symbols),
        }))
    }

    pub fn contract<F>(&mut self, name: impl Into<String>, build_contract: F) -> &mut Self
    where
        F: FnOnce(&mut ContractBuilder),
    {
        let mut contract_builder = ContractBuilder::new(name.into(), false);
        build_contract(&mut contract_builder);

        self.add_item(SourceUnitItem::Contract(contract_builder.build()))
    }

    pub fn abstract_contract<F>(&mut self, name: impl Into<String>, build_contract: F) -> &mut Self
    where
        F: FnOnce(&mut ContractBuilder),
    {
        let mut contract_builder = ContractBuilder::new(name.into(), true);
        build_contract(&mut contract_builder);

        self.add_item(SourceUnitItem::Contract(contract_builder.build()))
    }

    pub fn interface<F>(&mut self, name: impl Into<String>, build_interface: F) -> &mut Self
    where
        F: FnOnce(&mut ContractBuilder),
    {
        let mut interface_builder = ContractBuilder::new(name.into(), false);
        build_interface(&mut interface_builder);

        let contract = interface_builder.build();
        self.add_item(SourceUnitItem::Interface(InterfaceDefinition {
            name: contract.name,
            inheritance: contract.inheritance,
            body: contract.body,
        }))
    }

    pub fn library<F>(&mut self, name: impl Into<String>, build_library: F) -> &mut Self
    where
        F: FnOnce(&mut ContractBuilder),
    {
        let mut library_builder = ContractBuilder::new(name.into(), false);
        build_library(&mut library_builder);

        let contract = library_builder.build();
        self.add_item(SourceUnitItem::Library(LibraryDefinition {
            name: contract.name,
            body: contract.body,
        }))
    }

    pub fn struct_def<F>(&mut self, name: impl Into<String>, build_struct: F) -> &mut Self
    where
        F: FnOnce(&mut StructBuilder),
    {
        let mut struct_builder = StructBuilder::new(name.into());
        build_struct(&mut struct_builder);

        self.add_item(SourceUnitItem::Struct(struct_builder.build()))
    }

    pub fn enum_def(
        &mut self,
        name: impl Into<String>,
        values: Vec<impl Into<String>>,
    ) -> &mut Self {
        let enum_values = values.into_iter().map(Into::into).collect();

        self.add_item(SourceUnitItem::Enum(EnumDefinition {
            name: name.into(),
            values: enum_values,
        }))
    }

    pub fn error_def<F>(&mut self, name: impl Into<String>, build_error: F) -> &mut Self
    where
        F: FnOnce(&mut ErrorBuilder),
    {
        let mut error_builder = ErrorBuilder::new(name.into());
        build_error(&mut error_builder);

        self.add_item(SourceUnitItem::Error(error_builder.build()))
    }

    pub fn event_def<F>(&mut self, name: impl Into<String>, build_event: F) -> &mut Self
    where
        F: FnOnce(&mut EventBuilder),
    {
        let mut event_builder = EventBuilder::new(name.into());
        build_event(&mut event_builder);

        self.add_item(SourceUnitItem::Event(event_builder.build()))
    }
}

#[derive(Debug, Clone)]
pub struct ContractBuilder {
    name: String,
    is_abstract: bool,
    inheritance: Vec<InheritanceSpecifier>,
    body: Vec<ContractBodyElement>,
}

impl ContractBuilder {
    pub fn new(name: String, is_abstract: bool) -> Self {
        Self {
            name,
            is_abstract,
            inheritance: Vec::new(),
            body: Vec::new(),
        }
    }

    pub fn build(self) -> ContractDefinition {
        ContractDefinition {
            is_abstract: self.is_abstract,
            name: self.name,
            inheritance: self.inheritance,
            body: self.body,
        }
    }

    pub fn inherits(&mut self, name: impl Into<String>) -> &mut Self {
        self.inheritance.push(InheritanceSpecifier {
            name: IdentifierPath::single(name.into()),
            arguments: None,
        });
        self
    }

    pub fn inherits_with_args(
        &mut self,
        name: impl Into<String>,
        args: Vec<Expression>,
    ) -> &mut Self {
        self.inheritance.push(InheritanceSpecifier {
            name: IdentifierPath::single(name.into()),
            arguments: Some(args),
        });
        self
    }

    pub fn state_variable(
        &mut self,
        type_name: TypeName,
        name: impl Into<String>,
        visibility: Option<Visibility>,
        initial_value: Option<Expression>,
    ) -> &mut Self {
        self.body.push(ContractBodyElement::StateVariable(
            StateVariableDeclaration {
                type_name,
                visibility,
                is_constant: false,
                is_immutable: false,
                is_transient: false,
                override_specifier: None,
                name: name.into(),
                initial_value,
            },
        ));
        self
    }

    pub fn constant_variable(
        &mut self,
        type_name: TypeName,
        name: impl Into<String>,
        initial_value: Expression,
    ) -> &mut Self {
        self.body.push(ContractBodyElement::StateVariable(
            StateVariableDeclaration {
                type_name,
                visibility: None,
                is_constant: true,
                is_immutable: false,
                is_transient: false,
                override_specifier: None,
                name: name.into(),
                initial_value: Some(initial_value),
            },
        ));
        self
    }

    pub fn function<F>(&mut self, name: impl Into<String>, build_function: F) -> &mut Self
    where
        F: FnOnce(&mut FunctionBuilder),
    {
        let mut function_builder = FunctionBuilder::new(Some(name.into()));
        build_function(&mut function_builder);

        self.body
            .push(ContractBodyElement::Function(function_builder.build()));
        self
    }

    pub fn constructor<F>(&mut self, build_constructor: F) -> &mut Self
    where
        F: FnOnce(&mut ConstructorBuilder),
    {
        let mut constructor_builder = ConstructorBuilder::new();
        build_constructor(&mut constructor_builder);

        self.body.push(ContractBodyElement::Constructor(
            constructor_builder.build(),
        ));
        self
    }

    pub fn modifier<F>(&mut self, name: impl Into<String>, build_modifier: F) -> &mut Self
    where
        F: FnOnce(&mut ModifierBuilder),
    {
        let mut modifier_builder = ModifierBuilder::new(name.into());
        build_modifier(&mut modifier_builder);

        self.body
            .push(ContractBodyElement::Modifier(modifier_builder.build()));
        self
    }

    pub fn event<F>(&mut self, name: impl Into<String>, build_event: F) -> &mut Self
    where
        F: FnOnce(&mut EventBuilder),
    {
        let mut event_builder = EventBuilder::new(name.into());
        build_event(&mut event_builder);

        self.body
            .push(ContractBodyElement::Event(event_builder.build()));
        self
    }

    pub fn error<F>(&mut self, name: impl Into<String>, build_error: F) -> &mut Self
    where
        F: FnOnce(&mut ErrorBuilder),
    {
        let mut error_builder = ErrorBuilder::new(name.into());
        build_error(&mut error_builder);

        self.body
            .push(ContractBodyElement::Error(error_builder.build()));
        self
    }

    pub fn struct_def<F>(&mut self, name: impl Into<String>, build_struct: F) -> &mut Self
    where
        F: FnOnce(&mut StructBuilder),
    {
        let mut struct_builder = StructBuilder::new(name.into());
        build_struct(&mut struct_builder);

        self.body
            .push(ContractBodyElement::Struct(struct_builder.build()));
        self
    }

    pub fn enum_def(
        &mut self,
        name: impl Into<String>,
        values: Vec<impl Into<String>>,
    ) -> &mut Self {
        let enum_values = values.into_iter().map(Into::into).collect();

        self.body.push(ContractBodyElement::Enum(EnumDefinition {
            name: name.into(),
            values: enum_values,
        }));
        self
    }
}

#[derive(Debug, Clone)]
pub struct FunctionBuilder {
    name: Option<String>,
    parameters: Vec<Parameter>,
    visibility: Option<Visibility>,
    state_mutability: Option<StateMutability>,
    modifiers: Vec<ModifierInvocation>,
    is_virtual: bool,
    override_specifier: Option<OverrideSpecifier>,
    returns: Option<Vec<Parameter>>,
    body: Option<Block>,
}

impl FunctionBuilder {
    pub fn new(name: Option<String>) -> Self {
        Self {
            name,
            parameters: Vec::new(),
            visibility: None,
            state_mutability: None,
            modifiers: Vec::new(),
            is_virtual: false,
            override_specifier: None,
            returns: None,
            body: None,
        }
    }

    pub fn build(self) -> FunctionDefinition {
        FunctionDefinition {
            name: self.name,
            parameters: self.parameters,
            visibility: self.visibility,
            state_mutability: self.state_mutability,
            modifiers: self.modifiers,
            is_virtual: self.is_virtual,
            override_specifier: self.override_specifier,
            returns: self.returns,
            body: self.body,
        }
    }

    pub fn parameter(&mut self, type_name: TypeName, name: impl Into<String>) -> &mut Self {
        self.parameters.push(Parameter {
            type_name,
            data_location: None,
            name: Some(name.into()),
        });
        self
    }

    pub fn parameter_with_location(
        &mut self,
        type_name: TypeName,
        name: impl Into<String>,
        location: DataLocation,
    ) -> &mut Self {
        self.parameters.push(Parameter {
            type_name,
            data_location: Some(location),
            name: Some(name.into()),
        });
        self
    }

    pub fn visibility(&mut self, visibility: Visibility) -> &mut Self {
        self.visibility = Some(visibility);
        self
    }

    pub fn state_mutability(&mut self, mutability: StateMutability) -> &mut Self {
        self.state_mutability = Some(mutability);
        self
    }

    pub fn modifier(&mut self, name: impl Into<String>) -> &mut Self {
        self.modifiers.push(ModifierInvocation {
            name: IdentifierPath::single(name.into()),
            arguments: None,
        });
        self
    }

    pub fn modifier_with_args(
        &mut self,
        name: impl Into<String>,
        args: Vec<Expression>,
    ) -> &mut Self {
        self.modifiers.push(ModifierInvocation {
            name: IdentifierPath::single(name.into()),
            arguments: Some(args),
        });
        self
    }

    pub fn virtual_fn(&mut self) -> &mut Self {
        self.is_virtual = true;
        self
    }

    pub fn override_fn(&mut self, overrides: Vec<String>) -> &mut Self {
        self.override_specifier = Some(OverrideSpecifier {
            overrides: overrides.into_iter().map(IdentifierPath::single).collect(),
        });
        self
    }

    pub fn returns(&mut self, parameters: Vec<Parameter>) -> &mut Self {
        self.returns = Some(parameters);
        self
    }

    pub fn body<F>(&mut self, build_body: F) -> &mut Self
    where
        F: FnOnce(&mut BlockBuilder),
    {
        let mut block_builder = BlockBuilder::new();
        build_body(&mut block_builder);

        self.body = Some(block_builder.build());
        self
    }
}

#[derive(Debug, Clone)]
pub struct ConstructorBuilder {
    parameters: Vec<Parameter>,
    modifiers: Vec<ModifierInvocation>,
    is_payable: bool,
    visibility: Option<Visibility>,
    body: Option<Block>,
}

impl ConstructorBuilder {
    pub fn new() -> Self {
        Self {
            parameters: Vec::new(),
            modifiers: Vec::new(),
            is_payable: false,
            visibility: None,
            body: None,
        }
    }

    pub fn build(self) -> ConstructorDefinition {
        ConstructorDefinition {
            parameters: self.parameters,
            modifiers: self.modifiers,
            is_payable: self.is_payable,
            visibility: self.visibility,
            body: self.body.unwrap_or(Block {
                statements: Vec::new(),
            }),
        }
    }

    pub fn parameter(&mut self, type_name: TypeName, name: impl Into<String>) -> &mut Self {
        self.parameters.push(Parameter {
            type_name,
            data_location: None,
            name: Some(name.into()),
        });
        self
    }

    /// Adds a modifier.
    pub fn modifier(&mut self, name: impl Into<String>) -> &mut Self {
        self.modifiers.push(ModifierInvocation {
            name: IdentifierPath::single(name.into()),
            arguments: None,
        });
        self
    }

    pub fn payable(&mut self) -> &mut Self {
        self.is_payable = true;
        self
    }

    pub fn visibility(&mut self, visibility: Visibility) -> &mut Self {
        self.visibility = Some(visibility);
        self
    }

    pub fn body<F>(&mut self, build_body: F) -> &mut Self
    where
        F: FnOnce(&mut BlockBuilder),
    {
        let mut block_builder = BlockBuilder::new();
        build_body(&mut block_builder);

        self.body = Some(block_builder.build());
        self
    }
}

impl Default for ConstructorBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct ModifierBuilder {
    name: String,
    parameters: Vec<Parameter>,
    is_virtual: bool,
    override_specifier: Option<OverrideSpecifier>,
    body: Option<Block>,
}

impl ModifierBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            parameters: Vec::new(),
            is_virtual: false,
            override_specifier: None,
            body: None,
        }
    }

    pub fn build(self) -> ModifierDefinition {
        ModifierDefinition {
            name: self.name,
            parameters: self.parameters,
            is_virtual: self.is_virtual,
            override_specifier: self.override_specifier,
            body: self.body,
        }
    }

    pub fn parameter(&mut self, type_name: TypeName, name: impl Into<String>) -> &mut Self {
        self.parameters.push(Parameter {
            type_name,
            data_location: None,
            name: Some(name.into()),
        });
        self
    }

    pub fn virtual_modifier(&mut self) -> &mut Self {
        self.is_virtual = true;
        self
    }

    pub fn override_modifier(&mut self, overrides: Vec<String>) -> &mut Self {
        self.override_specifier = Some(OverrideSpecifier {
            overrides: overrides.into_iter().map(IdentifierPath::single).collect(),
        });
        self
    }

    pub fn body<F>(&mut self, build_body: F) -> &mut Self
    where
        F: FnOnce(&mut BlockBuilder),
    {
        let mut block_builder = BlockBuilder::new();
        build_body(&mut block_builder);

        self.body = Some(block_builder.build());
        self
    }
}

#[derive(Debug, Clone)]
pub struct StructBuilder {
    name: String,
    members: Vec<StructMember>,
}

impl StructBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            members: Vec::new(),
        }
    }

    pub fn build(self) -> StructDefinition {
        StructDefinition {
            name: self.name,
            members: self.members,
        }
    }

    /// Adds a member.
    pub fn member(&mut self, type_name: TypeName, name: impl Into<String>) -> &mut Self {
        self.members.push(StructMember {
            type_name,
            name: name.into(),
        });
        self
    }
}

#[derive(Debug, Clone)]
pub struct EventBuilder {
    name: String,
    parameters: Vec<EventParameter>,
    is_anonymous: bool,
}

impl EventBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            parameters: Vec::new(),
            is_anonymous: false,
        }
    }

    pub fn build(self) -> EventDefinition {
        EventDefinition {
            name: self.name,
            parameters: self.parameters,
            is_anonymous: self.is_anonymous,
        }
    }

    pub fn parameter(&mut self, type_name: TypeName, name: Option<String>) -> &mut Self {
        self.parameters.push(EventParameter {
            type_name,
            is_indexed: false,
            name,
        });
        self
    }

    pub fn indexed_parameter(&mut self, type_name: TypeName, name: Option<String>) -> &mut Self {
        self.parameters.push(EventParameter {
            type_name,
            is_indexed: true,
            name,
        });
        self
    }

    pub fn anonymous(&mut self) -> &mut Self {
        self.is_anonymous = true;
        self
    }
}

#[derive(Debug, Clone)]
pub struct ErrorBuilder {
    name: String,
    parameters: Vec<ErrorParameter>,
}

impl ErrorBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            parameters: Vec::new(),
        }
    }

    pub fn build(self) -> ErrorDefinition {
        ErrorDefinition {
            name: self.name,
            parameters: self.parameters,
        }
    }

    pub fn parameter(&mut self, type_name: TypeName, name: Option<String>) -> &mut Self {
        self.parameters.push(ErrorParameter { type_name, name });
        self
    }
}

#[derive(Debug, Clone)]
pub struct BlockBuilder {
    statements: Vec<Statement>,
}

impl BlockBuilder {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn build(self) -> Block {
        Block {
            statements: self.statements,
        }
    }

    pub fn statement(&mut self, statement: Statement) -> &mut Self {
        self.statements.push(statement);
        self
    }

    /// Adds an expression statement.
    pub fn expression(&mut self, expression: Expression) -> &mut Self {
        self.statements
            .push(Statement::Expression(ExpressionStatement { expression }));
        self
    }

    pub fn variable_declaration(
        &mut self,
        type_name: TypeName,
        name: impl Into<String>,
        initial_value: Option<Expression>,
    ) -> &mut Self {
        self.variable_declaration_with_location(type_name, name, None, initial_value)
    }

    pub fn variable_declaration_with_location(
        &mut self,
        type_name: TypeName,
        name: impl Into<String>,
        data_location: Option<DataLocation>,
        initial_value: Option<Expression>,
    ) -> &mut Self {
        let declaration = VariableDeclaration {
            type_name,
            data_location,
            name: name.into(),
        };

        self.statements
            .push(Statement::Variable(VariableDeclarationStatement {
                declaration,
                initial_value,
            }));
        self
    }

    pub fn assignment(&mut self, left: impl Into<String>, right: impl Into<String>) -> &mut Self {
        let assignment = Expression::Assignment(AssignmentExpression {
            left: Box::new(Expression::Identifier(left.into())),
            operator: AssignmentOperator::Assign,
            right: Box::new(Expression::Identifier(right.into())),
        });

        self.expression(assignment)
    }

    pub fn return_statement(&mut self, expression: Option<Expression>) -> &mut Self {
        self.statements
            .push(Statement::Return(ReturnStatement { expression }));
        self
    }

    pub fn if_statement<F>(&mut self, condition: Expression, build_then: F) -> &mut Self
    where
        F: FnOnce(&mut BlockBuilder),
    {
        let mut then_builder = BlockBuilder::new();
        build_then(&mut then_builder);

        self.statements.push(Statement::If(IfStatement {
            condition,
            then_statement: Box::new(Statement::Block(then_builder.build())),
            else_statement: None,
        }));
        self
    }

    pub fn if_else_statement<F, G>(
        &mut self,
        condition: Expression,
        build_then: F,
        build_else: G,
    ) -> &mut Self
    where
        F: FnOnce(&mut BlockBuilder),
        G: FnOnce(&mut BlockBuilder),
    {
        let mut then_builder = BlockBuilder::new();
        build_then(&mut then_builder);

        let mut else_builder = BlockBuilder::new();
        build_else(&mut else_builder);

        self.statements.push(Statement::If(IfStatement {
            condition,
            then_statement: Box::new(Statement::Block(then_builder.build())),
            else_statement: Some(Box::new(Statement::Block(else_builder.build()))),
        }));
        self
    }
}

impl Default for BlockBuilder {
    fn default() -> Self {
        Self::new()
    }
}

pub fn uint256() -> TypeName {
    TypeName::Elementary(ElementaryTypeName::UnsignedInteger(Some(256)))
}

pub fn uint(bits: u16) -> TypeName {
    TypeName::Elementary(ElementaryTypeName::UnsignedInteger(Some(bits)))
}

pub fn int256() -> TypeName {
    TypeName::Elementary(ElementaryTypeName::SignedInteger(Some(256)))
}

pub fn int(bits: u16) -> TypeName {
    TypeName::Elementary(ElementaryTypeName::SignedInteger(Some(bits)))
}

pub fn address() -> TypeName {
    TypeName::Elementary(ElementaryTypeName::Address)
}

pub fn bool() -> TypeName {
    TypeName::Elementary(ElementaryTypeName::Bool)
}

pub fn string() -> TypeName {
    TypeName::Elementary(ElementaryTypeName::String)
}

pub fn bytes() -> TypeName {
    TypeName::Elementary(ElementaryTypeName::Bytes)
}

/// Helper function to determine if a type requires a data location specifier
pub fn requires_data_location(type_name: &TypeName) -> bool {
    match type_name {
        TypeName::Elementary(elem) => match elem {
            ElementaryTypeName::String | ElementaryTypeName::Bytes => true,
            _ => false,
        },
        TypeName::Array(_, _) => true,
        TypeName::UserDefined(_) => true, // Structs require data location
        TypeName::Mapping(_) => false, // Mappings are always storage
        TypeName::Function(_) => false,
    }
}

/// Helper function to get the appropriate data location for a type in local variable context
pub fn get_default_data_location(type_name: &TypeName) -> Option<DataLocation> {
    if requires_data_location(type_name) {
        Some(DataLocation::Memory)
    } else {
        None
    }
}

pub fn bytes_fixed(size: u8) -> TypeName {
    TypeName::Elementary(ElementaryTypeName::FixedBytes(Some(size)))
}

pub fn array(element_type: TypeName, size: Option<Expression>) -> TypeName {
    TypeName::Array(Box::new(element_type), size.map(Box::new))
}

pub fn mapping(key_type: TypeName, value_type: TypeName) -> TypeName {
    TypeName::Mapping(MappingType {
        key_type: Box::new(key_type),
        key_name: None,
        value_type: Box::new(value_type),
        value_name: None,
    })
}

pub fn user_type(name: impl Into<String>) -> TypeName {
    TypeName::UserDefined(IdentifierPath::single(name.into()))
}

pub fn number(value: impl Into<String>) -> Expression {
    Expression::Literal(Literal::Number(NumberLiteral {
        value: value.into(),
        sub_denomination: None,
    }))
}

pub fn string_literal(value: impl Into<String>) -> Expression {
    Expression::Literal(Literal::String(StringLiteral {
        value: value.into(),
    }))
}

pub fn boolean(value: bool) -> Expression {
    Expression::Literal(Literal::Boolean(value))
}

pub fn identifier(name: impl Into<String>) -> Expression {
    Expression::Identifier(name.into())
}

pub fn binary(left: Expression, operator: BinaryOperator, right: Expression) -> Expression {
    Expression::Binary(BinaryExpression {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    })
}

pub fn add(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::Add, right)
}

pub fn sub(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::Sub, right)
}

pub fn mul(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::Mul, right)
}

pub fn div(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::Div, right)
}

pub fn eq(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::Equal, right)
}

pub fn gt(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::GreaterThan, right)
}

pub fn lt(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::LessThan, right)
}

pub fn and(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::And, right)
}

pub fn or(left: Expression, right: Expression) -> Expression {
    binary(left, BinaryOperator::Or, right)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_contract_builder() {
        let mut builder = SolidityBuilder::new();
        builder
            .pragma("solidity", "^0.8.0")
            .contract("MyContract", |contract| {
                contract
                    .state_variable(uint256(), "value", Some(Visibility::Public), None)
                    .function("setValue", |func| {
                        func.parameter(uint256(), "_value")
                            .visibility(Visibility::Public)
                            .body(|body| {
                                body.assignment("value", "_value");
                            });
                    });
            });

        let source_unit = builder.build();
        assert_eq!(source_unit.items.len(), 2);

        if let SourceUnitItem::Pragma(pragma) = &source_unit.items[0] {
            assert_eq!(pragma.tokens, vec!["solidity", "^0.8.0"]);
        } else {
            panic!("Expected pragma directive");
        }

        if let SourceUnitItem::Contract(contract) = &source_unit.items[1] {
            assert_eq!(contract.name, "MyContract");
            assert!(!contract.is_abstract);
            assert_eq!(contract.body.len(), 2);
        } else {
            panic!("Expected contract definition");
        }
    }

    #[test]
    fn test_struct_builder() {
        let mut builder = SolidityBuilder::new();
        builder.struct_def("Person", |s| {
            s.member(string(), "name")
                .member(uint256(), "age")
                .member(address(), "wallet");
        });

        let source_unit = builder.build();
        assert_eq!(source_unit.items.len(), 1);

        if let SourceUnitItem::Struct(struct_def) = &source_unit.items[0] {
            assert_eq!(struct_def.name, "Person");
            assert_eq!(struct_def.members.len(), 3);
            assert_eq!(struct_def.members[0].name, "name");
            assert_eq!(struct_def.members[1].name, "age");
            assert_eq!(struct_def.members[2].name, "wallet");
        } else {
            panic!("Expected struct definition");
        }
    }

    #[test]
    fn test_event_builder() {
        let mut builder = SolidityBuilder::new();
        builder.event_def("Transfer", |event| {
            event
                .indexed_parameter(address(), Some("from".to_string()))
                .indexed_parameter(address(), Some("to".to_string()))
                .parameter(uint256(), Some("value".to_string()));
        });

        let source_unit = builder.build();
        assert_eq!(source_unit.items.len(), 1);

        if let SourceUnitItem::Event(event_def) = &source_unit.items[0] {
            assert_eq!(event_def.name, "Transfer");
            assert_eq!(event_def.parameters.len(), 3);
            assert!(event_def.parameters[0].is_indexed);
            assert!(event_def.parameters[1].is_indexed);
            assert!(!event_def.parameters[2].is_indexed);
        } else {
            panic!("Expected event definition");
        }
    }

    #[test]
    fn test_expression_helpers() {
        let expr = add(identifier("a"), mul(identifier("b"), number("10")));

        if let Expression::Binary(binary) = expr {
            assert!(matches!(binary.operator, BinaryOperator::Add));
            if let Expression::Binary(right_binary) = *binary.right {
                assert!(matches!(right_binary.operator, BinaryOperator::Mul));
            } else {
                panic!("Expected multiplication on the right side");
            }
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_type_helpers() {
        assert!(matches!(
            uint256(),
            TypeName::Elementary(ElementaryTypeName::UnsignedInteger(Some(256)))
        ));
        assert!(matches!(
            address(),
            TypeName::Elementary(ElementaryTypeName::Address)
        ));
        assert!(matches!(
            bool(),
            TypeName::Elementary(ElementaryTypeName::Bool)
        ));

        let arr_type = array(uint256(), Some(number("10")));
        if let TypeName::Array(element_type, size) = arr_type {
            assert!(matches!(
                *element_type,
                TypeName::Elementary(ElementaryTypeName::UnsignedInteger(Some(256)))
            ));
            assert!(size.is_some());
        } else {
            panic!("Expected array type");
        }

        let map_type = mapping(address(), uint256());
        if let TypeName::Mapping(mapping_type) = map_type {
            assert!(matches!(
                *mapping_type.key_type,
                TypeName::Elementary(ElementaryTypeName::Address)
            ));
            assert!(matches!(
                *mapping_type.value_type,
                TypeName::Elementary(ElementaryTypeName::UnsignedInteger(Some(256)))
            ));
        } else {
            panic!("Expected mapping type");
        }
    }

    #[test]
    fn test_complex_contract() {
        let mut builder = SolidityBuilder::new();
        builder
            .pragma("solidity", "^0.8.0")
            .import("./IERC20.sol")
            .contract("Token", |contract| {
                contract
                    .inherits("IERC20")
                    .state_variable(
                        mapping(address(), uint256()),
                        "balances",
                        Some(Visibility::Private),
                        None,
                    )
                    .state_variable(uint256(), "totalSupply", Some(Visibility::Public), None)
                    .constructor(|constructor| {
                        constructor
                            .parameter(uint256(), "initialSupply")
                            .body(|body| {
                                body.assignment("totalSupply", "initialSupply");
                            });
                    })
                    .function("transfer", |func| {
                        func.parameter(address(), "to")
                            .parameter(uint256(), "amount")
                            .visibility(Visibility::Public)
                            .returns(vec![Parameter {
                                type_name: bool(),
                                data_location: None,
                                name: None,
                            }])
                            .body(|body| {
                                body.if_statement(
                                    gt(identifier("balances[msg.sender]"), identifier("amount")),
                                    |then_block| {
                                        then_block
                                            .assignment(
                                                "balances[msg.sender]",
                                                "balances[msg.sender] - amount",
                                            )
                                            .assignment("balances[to]", "balances[to] + amount")
                                            .return_statement(Some(boolean(true)));
                                    },
                                );
                            });
                    })
                    .event("Transfer", |event| {
                        event
                            .indexed_parameter(address(), Some("from".to_string()))
                            .indexed_parameter(address(), Some("to".to_string()))
                            .parameter(uint256(), Some("value".to_string()));
                    });
            });

        let source_unit = builder.build();
        assert_eq!(source_unit.items.len(), 3); // pragma, import, contract

        if let SourceUnitItem::Contract(contract) = &source_unit.items[2] {
            assert_eq!(contract.name, "Token");
            assert_eq!(contract.inheritance.len(), 1);
            assert!(!contract.body.is_empty());
        } else {
            panic!("Expected contract definition");
        }
    }
}
