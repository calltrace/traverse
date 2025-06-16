//! AST types for Solidity

#[derive(Debug, Clone, PartialEq, Default)]
pub struct SourceUnit {
    pub items: Vec<SourceUnitItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SourceUnitItem {
    Pragma(PragmaDirective),
    Import(ImportDirective),
    Contract(ContractDefinition),
    Interface(InterfaceDefinition),
    Library(LibraryDefinition),
    Function(FunctionDefinition),
    ConstantVariable(ConstantVariableDeclaration),
    Struct(StructDefinition),
    Enum(EnumDefinition),
    Error(ErrorDefinition),
    Event(EventDefinition),
    Using(UsingDirective),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PragmaDirective {
    pub tokens: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportDirective {
    pub path: String,
    pub alias: Option<String>,
    pub symbols: Option<Vec<ImportSymbol>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportSymbol {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContractDefinition {
    pub is_abstract: bool,
    pub name: String,
    pub inheritance: Vec<InheritanceSpecifier>,
    pub body: Vec<ContractBodyElement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDefinition {
    pub name: String,
    pub inheritance: Vec<InheritanceSpecifier>,
    pub body: Vec<ContractBodyElement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LibraryDefinition {
    pub name: String,
    pub body: Vec<ContractBodyElement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InheritanceSpecifier {
    pub name: IdentifierPath,
    pub arguments: Option<Vec<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContractBodyElement {
    Constructor(ConstructorDefinition),
    Function(FunctionDefinition),
    Modifier(ModifierDefinition),
    StateVariable(StateVariableDeclaration),
    Event(EventDefinition),
    Error(ErrorDefinition),
    Struct(StructDefinition),
    Enum(EnumDefinition),
    Using(UsingDirective),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub name: Option<String>,
    pub parameters: Vec<Parameter>,
    pub visibility: Option<Visibility>,
    pub state_mutability: Option<StateMutability>,
    pub modifiers: Vec<ModifierInvocation>,
    pub is_virtual: bool,
    pub override_specifier: Option<OverrideSpecifier>,
    pub returns: Option<Vec<Parameter>>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDefinition {
    pub parameters: Vec<Parameter>,
    pub modifiers: Vec<ModifierInvocation>,
    pub is_payable: bool,
    pub visibility: Option<Visibility>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModifierDefinition {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub is_virtual: bool,
    pub override_specifier: Option<OverrideSpecifier>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateVariableDeclaration {
    pub type_name: TypeName,
    pub visibility: Option<Visibility>,
    pub is_constant: bool,
    pub is_immutable: bool,
    pub is_transient: bool,
    pub override_specifier: Option<OverrideSpecifier>,
    pub name: String,
    pub initial_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantVariableDeclaration {
    pub type_name: TypeName,
    pub name: String,
    pub initial_value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub name: String,
    pub members: Vec<StructMember>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    pub type_name: TypeName,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDefinition {
    pub name: String,
    pub values: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EventDefinition {
    pub name: String,
    pub parameters: Vec<EventParameter>,
    pub is_anonymous: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EventParameter {
    pub type_name: TypeName,
    pub is_indexed: bool,
    pub name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorDefinition {
    pub name: String,
    pub parameters: Vec<ErrorParameter>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ErrorParameter {
    pub type_name: TypeName,
    pub name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UsingDirective {
    pub library: IdentifierPath,
    pub type_name: Option<TypeName>,
    pub is_global: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub type_name: TypeName,
    pub data_location: Option<DataLocation>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModifierInvocation {
    pub name: IdentifierPath,
    pub arguments: Option<Vec<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OverrideSpecifier {
    pub overrides: Vec<IdentifierPath>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Internal,
    External,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StateMutability {
    Pure,
    View,
    Payable,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataLocation {
    Memory,
    Storage,
    Calldata,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeName {
    Elementary(ElementaryTypeName),
    UserDefined(IdentifierPath),
    Array(Box<TypeName>, Option<Box<Expression>>), // Fixed recursion with Box
    Mapping(MappingType),
    Function(FunctionTypeName),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElementaryTypeName {
    Address,
    AddressPayable,
    Bool,
    String,
    Bytes,
    SignedInteger(Option<u16>), // bit size
    UnsignedInteger(Option<u16>), // bit size
    FixedBytes(Option<u8>), // byte size
    Fixed,
    Ufixed,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MappingType {
    pub key_type: Box<TypeName>,
    pub key_name: Option<String>,
    pub value_type: Box<TypeName>,
    pub value_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionTypeName {
    pub parameters: Vec<Parameter>,
    pub visibility: Option<Visibility>,
    pub state_mutability: Option<StateMutability>,
    pub returns: Option<Vec<Parameter>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierPath {
    pub parts: Vec<String>,
}

impl IdentifierPath {
    pub fn new(parts: Vec<String>) -> Self {
        Self { parts }
    }
    
    pub fn single(name: String) -> Self {
        Self { parts: vec![name] }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(String),
    Literal(Literal),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    FunctionCall(FunctionCallExpression),
    MemberAccess(MemberAccessExpression),
    IndexAccess(IndexAccessExpression),
    Conditional(ConditionalExpression),
    Assignment(AssignmentExpression),
    Tuple(TupleExpression),
    Array(ArrayExpression),
    TypeConversion(Box<TypeConversionExpression>), // Fixed recursion with Box
    New(NewExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    ShiftRightArithmetic,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
    pub is_prefix: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
    BitNot,
    Increment,
    Decrement,
    Delete,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCallExpression {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccessExpression {
    pub object: Box<Expression>,
    pub member: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexAccessExpression {
    pub object: Box<Expression>,
    pub index: Option<Box<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalExpression {
    pub condition: Box<Expression>,
    pub true_expr: Box<Expression>,
    pub false_expr: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignmentExpression {
    pub left: Box<Expression>,
    pub operator: AssignmentOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShiftLeftAssign,
    ShiftRightAssign,
    ShiftRightArithmeticAssign,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpression {
    pub elements: Vec<Option<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpression {
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConversionExpression {
    pub type_name: Box<TypeName>, // Fixed recursion with Box
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewExpression {
    pub type_name: TypeName,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Number(NumberLiteral),
    String(StringLiteral),
    HexString(HexStringLiteral),
    UnicodeString(UnicodeStringLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {
    pub value: String,
    pub sub_denomination: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HexStringLiteral {
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnicodeStringLiteral {
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Block),
    Expression(ExpressionStatement),
    Variable(VariableDeclarationStatement),
    If(IfStatement),
    For(ForStatement),
    While(WhileStatement),
    DoWhile(DoWhileStatement),
    Continue(ContinueStatement),
    Break(BreakStatement),
    Return(ReturnStatement),
    Emit(EmitStatement),
    Revert(RevertStatement),
    Try(TryStatement),
    Assembly(AssemblyStatement),
    Unchecked(UncheckedBlock),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclarationStatement {
    pub declaration: VariableDeclaration,
    pub initial_value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub type_name: TypeName,
    pub data_location: Option<DataLocation>,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_statement: Box<Statement>,
    pub else_statement: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub init: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoWhileStatement {
    pub body: Box<Statement>,
    pub condition: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStatement;

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement;

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    pub expression: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EmitStatement {
    pub event_call: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RevertStatement {
    pub error_call: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryStatement {
    pub expression: Expression,
    pub returns: Option<Vec<Parameter>>,
    pub body: Block,
    pub catch_clauses: Vec<CatchClause>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CatchClause {
    pub identifier: Option<String>,
    pub parameters: Option<Vec<Parameter>>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssemblyStatement {
    pub dialect: Option<String>,
    pub flags: Vec<String>,
    pub body: YulBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UncheckedBlock {
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulBlock {
    pub statements: Vec<YulStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum YulStatement {
    Block(YulBlock),
    VariableDeclaration(YulVariableDeclaration),
    Assignment(YulAssignment),
    FunctionCall(YulFunctionCall),
    If(YulIfStatement),
    For(YulForStatement),
    Switch(YulSwitchStatement),
    FunctionDefinition(YulFunctionDefinition),
    Leave,
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulVariableDeclaration {
    pub variables: Vec<String>,
    pub expression: Option<YulExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulAssignment {
    pub variables: Vec<YulPath>,
    pub expression: YulExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulFunctionCall {
    pub function: String,
    pub arguments: Vec<YulExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulIfStatement {
    pub condition: YulExpression,
    pub body: YulBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulForStatement {
    pub init: YulBlock,
    pub condition: YulExpression,
    pub post: YulBlock,
    pub body: YulBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulSwitchStatement {
    pub expression: YulExpression,
    pub cases: Vec<YulSwitchCase>,
    pub default: Option<YulBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulSwitchCase {
    pub value: YulLiteral,
    pub body: YulBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulFunctionDefinition {
    pub name: String,
    pub parameters: Vec<String>,
    pub returns: Vec<String>,
    pub body: YulBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct YulPath {
    pub parts: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum YulExpression {
    Path(YulPath),
    FunctionCall(YulFunctionCall),
    Literal(YulLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub enum YulLiteral {
    Number(String),
    String(String),
    HexNumber(String),
    Boolean(bool),
    HexString(String),
}
