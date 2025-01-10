use std::collections::BTreeMap;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pos {
    pub line: usize,
    pub column: usize,
}

impl Pos {
    pub fn nopos() -> Self {
        Self { line: 0, column: 0 }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprNode {
    EVar {
        pos: Pos,
        name: String,
    },
    EInt {
        pos: Pos,
        value: i64,
    },
    EString {
        pos: Pos,
        value: String,
    },
    EInterpolated {
        pos: Pos,
        value: String,
    },
    EBool {
        pos: Pos,
        value: bool,
    },
    EApply {
        pos: Pos,
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    EBinOp {
        pos: Pos,
        op: BOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    EUnOp {
        pos: Pos,
        op: UOp,
        expr: Box<Expr>,
    },
    EStruct {
        pos: Pos,
        constructor: String,
        fields: Vec<(String, Expr)>,
    },
    ETuple {
        pos: Pos,
        items: Vec<Expr>,
    },
    EField {
        pos: Pos,
        base: Box<Expr>,
        field: String,
    },
    EITE {
        pos: Pos,
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    EMatch {
        pos: Pos,
        match_expr: Box<Expr>,
        cases: Vec<(Expr, Expr)>,
    },
    ESeq {
        pos: Pos,
        first: Box<Expr>,
        second: Box<Expr>,
    },
    ESet {
        pos: Pos,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    EVarDecl {
        pos: Pos,
        name: String,
    },
    EPHolder {
        pos: Pos,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr(pub Box<ExprNode>);

impl Expr {
    pub fn new(node: ExprNode) -> Self {
        Self(Box::new(node))
    }

    pub fn var(name: &str) -> Self {
        Self::new(ExprNode::EVar {
            pos: Pos::nopos(),
            name: name.into(),
        })
    }

    pub fn int(value: i64) -> Self {
        Self::new(ExprNode::EInt {
            pos: Pos::nopos(),
            value,
        })
    }

    pub fn string_lit(s: &str) -> Self {
        Self::new(ExprNode::EString {
            pos: Pos::nopos(),
            value: s.into(),
        })
    }

    pub fn interpolated(s: &str) -> Self {
        Self::new(ExprNode::EInterpolated {
            pos: Pos::nopos(),
            value: s.into(),
        })
    }

    pub fn bool_lit(b: bool) -> Self {
        Self::new(ExprNode::EBool {
            pos: Pos::nopos(),
            value: b,
        })
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &*self.0 {
            ExprNode::EVar { name, .. } => write!(f, "{}", name),
            ExprNode::EInt { value, .. } => write!(f, "{}", value),
            ExprNode::EString { value, .. } => write!(f, "\"{}\"", value),
            ExprNode::EInterpolated { value, .. } => write!(f, "[|{}|]", value),
            ExprNode::EBool { value, .. } => {
                if *value {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            ExprNode::EApply { func, args, .. } => {
                let args_str = args
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({})", func, args_str)
            }
            ExprNode::EBinOp { op, left, right, .. } => {
                let op_str = match op {
                    BOp::Eq => "==",
                    BOp::Ne => "!=",
                    BOp::Lt => "<",
                    BOp::Le => "<=",
                    BOp::Gt => ">",
                    BOp::Ge => ">=",
                    BOp::And => "&&",
                    BOp::Or => "||",
                    BOp::Add => "+",
                    BOp::Sub => "-",
                    BOp::Mul => "*",
                    BOp::Div => "/",
                    BOp::Mod => "%",
                };
                write!(f, "({} {} {})", left, op_str, right)
            }
            ExprNode::EUnOp { op, expr, .. } => {
                let op_str = match op {
                    UOp::Not => "!",
                    UOp::Neg => "-",
                };
                write!(f, "({} {})", op_str, expr)
            }
            ExprNode::EStruct {
                constructor,
                fields,
                ..
            } => {
                let mut field_strs = Vec::new();
                for (n, e) in fields {
                    if n.is_empty() {
                        field_strs.push(e.to_string());
                    } else {
                        field_strs.push(format!(".{}={}", n, e));
                    }
                }
                let joined = field_strs.join(", ");
                write!(f, "{}{{{}}}", constructor, joined)
            }
            ExprNode::ETuple { items, .. } => {
                if items.len() == 1 {
                    write!(f, "{}", items[0])
                } else {
                    let joined = items
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "({})", joined)
                }
            }
            ExprNode::EField { base, field, .. } => {
                write!(f, "({}).{}", base, field)
            }
            ExprNode::EITE {
                cond,
                then_expr,
                else_expr,
                ..
            } => {
                write!(f, "if ({}) {{ {} }} else {{ {} }}", cond, then_expr, else_expr)
            }
            ExprNode::EMatch {
                match_expr,
                cases,
                ..
            } => {
                let mut case_parts = Vec::new();
                for (cpat, cval) in cases {
                    case_parts.push(format!("{} -> {}", cpat, cval));
                }
                let joined = case_parts.join(", ");
                write!(f, "match({}) {{ {} }}", match_expr, joined)
            }
            ExprNode::ESeq { first, second, .. } => {
                write!(f, "{{ {}; {} }}", first, second)
            }
            ExprNode::ESet { lhs, rhs, .. } => {
                write!(f, "({} = {})", lhs, rhs)
            }
            ExprNode::EVarDecl { name, .. } => write!(f, "var {}", name),
            ExprNode::EPHolder { .. } => write!(f, "_"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgType {
    pub pos: Pos,
    pub is_mut: bool,
    pub arg_type: DType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureExprArg {
    pub name: String,
    pub maybe_type: Option<ArgType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DType {
    TBool {
        pos: Pos,
    },
    TInt {
        pos: Pos,
    },
    TString {
        pos: Pos,
    },
    TBit {
        pos: Pos,
        width: u32,
    },
    TSigned {
        pos: Pos,
        width: u32,
    },
    TDouble {
        pos: Pos,
    },
    TFloat {
        pos: Pos,
    },
    TStruct {
        pos: Pos,
        constructors: Vec<Constructor>,
    },
    TTuple {
        pos: Pos,
        tup_args: Vec<DType>,
    },
    TUser {
        pos: Pos,
        name: String,
        type_args: Vec<DType>,
    },
    TVar {
        pos: Pos,
        var_name: String,
    },
    TOpaque {
        pos: Pos,
        name: String,
        type_args: Vec<DType>,
    },
    TFunction {
        pos: Pos,
        func_args: Vec<ArgType>,
        ret_type: Box<DType>,
    },
}

impl Display for DType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DType::TBool { .. } => write!(f, "bool"),
            DType::TInt { .. } => write!(f, "bigint"),
            DType::TString { .. } => write!(f, "string"),
            DType::TBit { width, .. } => write!(f, "bit<{}>", width),
            DType::TSigned { width, .. } => write!(f, "signed<{}>", width),
            DType::TDouble { .. } => write!(f, "double"),
            DType::TFloat { .. } => write!(f, "float"),
            DType::TStruct { constructors, .. } => {
                let mut cstrs = Vec::new();
                for c in constructors {
                    cstrs.push(c.to_string());
                }
                write!(f, "{}", cstrs.join(" | "))
            }
            DType::TTuple { tup_args, .. } => {
                if tup_args.len() == 1 {
                    write!(f, "{}", tup_args[0])
                } else {
                    let joined = tup_args
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "({})", joined)
                }
            }
            DType::TUser {
                name,
                type_args,
                ..
            } => {
                if type_args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let joined = type_args
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}<{}>", name, joined)
                }
            }
            DType::TVar { var_name, .. } => write!(f, "'{}", var_name),
            DType::TOpaque {
                name,
                type_args,
                ..
            } => {
                if type_args.is_empty() {
                    write!(f, "{}", name)
                } else {
                    let joined = type_args
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "{}<{}>", name, joined)
                }
            }
            DType::TFunction {
                func_args,
                ret_type,
                ..
            } => {
                let args_joined = func_args
                    .iter()
                    .map(|a| a.arg_type.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "function({}): {}", args_joined, ret_type)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub pos: Pos,
    pub name: String,
    pub ftype: DType,
}

impl Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ftype)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constructor {
    pub pos: Pos,
    pub name: String,
    pub args: Vec<Field>,
}

impl Display for Constructor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();
        for (i, fld) in self.args.iter().enumerate() {
            if i > 0 {
                parts.push(", ".to_string());
            }
            parts.push(format!("{}: {}", fld.name, fld.ftype));
        }
        let joined = parts.join("");
        write!(f, "{}{{{}}}", self.name, joined)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KeyExpr {
    pub pos: Pos,
    pub var: String,
    pub expr: Expr,
}

impl Display for KeyExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}) {}", self.var, self.expr)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelationRole {
    RelInput,
    RelOutput,
    RelInternal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RelationSemantics {
    RelSet,
    RelStream,
    RelMultiset,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Relation {
    pub pos: Pos,
    pub role: RelationRole,
    pub semantics: RelationSemantics,
    pub name: String,
    pub rtype: DType,
    pub primary_key: Option<KeyExpr>,
}

impl Display for Relation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let role_str = match self.role {
            RelationRole::RelInput => "input ",
            RelationRole::RelOutput => "output ",
            RelationRole::RelInternal => "",
        };
        let sem_str = match self.semantics {
            RelationSemantics::RelSet => "relation",
            RelationSemantics::RelStream => "stream",
            RelationSemantics::RelMultiset => "multiset",
        };
        write!(f, "{}{} {}[{}]", role_str, sem_str, self.name, self.rtype)?;
        if let Some(pk) = &self.primary_key {
            write!(f, " primary key {};", pk)
        } else {
            write!(f, ";")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Index {
    pub pos: Pos,
    pub name: String,
    pub vars: Vec<Field>,
    pub atom: Atom,
}

impl Display for Index {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let vars_str = self
            .vars
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "index {}({}) on {}", self.name, vars_str, self.atom)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Delay {
    pub pos: Pos,
    pub delay: u32,
}

impl Delay {
    pub fn zero() -> Self {
        Self {
            pos: Pos::nopos(),
            delay: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Atom {
    pub pos: Pos,
    pub relation: String,
    pub delay: Delay,
    pub diff: bool,
    pub value: Expr,
}

impl Atom {
    pub fn is_delayed(&self) -> bool {
        self.delay.delay != 0
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let diff_str = if self.diff { "'" } else { "" };
        let delay_str = if self.is_delayed() {
            format!("-{}", self.delay.delay)
        } else {
            "".to_string()
        };
        write!(f, "{}{}{}({})", self.relation, diff_str, delay_str, self.value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuleLHS {
    pub pos: Pos,
    pub atom: Atom,
    pub location: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuleRHS {
    RHSLiteral {
        pos: Pos,
        polarity: bool,
        atom: Atom,
    },
    RHSCondition {
        pos: Pos,
        expr: Expr,
    },
    RHSGroupBy {
        pos: Pos,
        var: String,
        project: Expr,
        group_by: Expr,
    },
    RHSFlatMap {
        pos: Pos,
        vars: Expr,
        map_expr: Expr,
    },
    RHSInspect {
        pos: Pos,
        inspect_expr: Expr,
    },
}

impl Display for RuleRHS {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            RuleRHS::RHSLiteral { polarity, atom, .. } => {
                if *polarity {
                    write!(f, "{}", atom)
                } else {
                    write!(f, "not {}", atom)
                }
            }
            RuleRHS::RHSCondition { expr, .. } => write!(f, "{}", expr),
            RuleRHS::RHSGroupBy {
                var,
                project,
                group_by,
                ..
            } => {
                write!(f, "var {} = {}.group_by({})", var, project, group_by)
            }
            RuleRHS::RHSFlatMap { vars, map_expr, .. } => {
                write!(f, "{} = FlatMap({})", vars, map_expr)
            }
            RuleRHS::RHSInspect { inspect_expr, .. } => {
                write!(f, "Inspect {}", inspect_expr)
            }
        }
    }
}

impl RuleRHS {
    pub fn is_literal(&self) -> bool {
        matches!(self, RuleRHS::RHSLiteral { .. })
    }
    pub fn is_positive_literal(&self) -> bool {
        matches!(self, RuleRHS::RHSLiteral { polarity: true, .. })
    }
    pub fn is_negative_literal(&self) -> bool {
        matches!(self, RuleRHS::RHSLiteral { polarity: false, .. })
    }
    pub fn is_group_by(&self) -> bool {
        matches!(self, RuleRHS::RHSGroupBy { .. })
    }
    pub fn is_condition(&self) -> bool {
        matches!(self, RuleRHS::RHSCondition { .. })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rule {
    pub pos: Pos,
    pub module: ModuleName,
    pub lhs: Vec<RuleLHS>,
    pub rhs: Vec<RuleRHS>,
}

impl Display for Rule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let lhs_str = self
            .lhs
            .iter()
            .map(|l| {
                let base = format!("{}", l.atom);
                if let Some(loc) = &l.location {
                    format!("{} @{}", base, loc)
                } else {
                    base
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        if self.rhs.is_empty() {
            write!(f, "{}.", lhs_str)
        } else {
            let rhs_str = self
                .rhs
                .iter()
                .map(|rc| rc.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            write!(f, "{} :- {}.", lhs_str, rhs_str)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleName {
    pub path: Vec<String>,
}

impl Display for ModuleName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.path.is_empty() {
            write!(f, "")
        } else {
            let joined = self.path.join("::");
            write!(f, "{}", joined)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub pos: Pos,
    pub module: ModuleName,
    pub alias: ModuleName,
}

impl Display for Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.alias.path.is_empty() {
            write!(f, "import {}", self.module)
        } else {
            write!(f, "import {} as {}", self.module, self.alias)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HOType {
    HOTypeRelation {
        pos: Pos,
        hotype: DType,
    },
    HOTypeFunction {
        pos: Pos,
        args: Vec<FuncArg>,
        hotype: DType,
    },
}

impl Display for HOType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HOType::HOTypeRelation { hotype, .. } => {
                write!(f, "relation[{}]", hotype)
            }
            HOType::HOTypeFunction { args, hotype, .. } => {
                let arg_str = args
                    .iter()
                    .map(|a| {
                        let is_mut = if a.arg_type.is_mut { "mut " } else { "" };
                        format!("{}: {}", is_mut.to_string() + &a.name, a.arg_type.arg_type)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "function({}): {}", arg_str, hotype)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FuncArg {
    pub pos: Pos,
    pub name: String,
    pub arg_type: ArgType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub pos: Pos,
    pub name: String,
    pub args: Vec<FuncArg>,
    pub ftype: DType,
    pub def: Option<Expr>,
}

impl Function {
    pub fn is_extern(&self) -> bool {
        self.def.is_none()
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args_str = self
            .args
            .iter()
            .map(|fa| {
                let is_mut = if fa.arg_type.is_mut { "mut " } else { "" };
                format!("{}: {}", is_mut.to_owned() + &fa.name, fa.arg_type.arg_type)
            })
            .collect::<Vec<_>>()
            .join(", ");
        if let Some(body) = &self.def {
            write!(
                f,
                "function {}({}): {} {{ {} }}",
                self.name, args_str, self.ftype, body
            )
        } else {
            write!(
                f,
                "extern function {}({}): {};",
                self.name, args_str, self.ftype
            )
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Transformer {
    pub pos: Pos,
    pub is_extern: bool,
    pub name: String,
    pub inputs: Vec<HOField>,
    pub outputs: Vec<HOField>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HOField {
    pub pos: Pos,
    pub name: String,
    pub hof_type: HOType,
}

impl Display for HOField {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: ", self.name)?;
        match &self.hof_type {
            HOType::HOTypeRelation { hotype, .. } => {
                write!(f, "relation[{}]", hotype)
            }
            HOType::HOTypeFunction { args, hotype, .. } => {
                let arg_str = args
                    .iter()
                    .map(|a| {
                        let is_mut = if a.arg_type.is_mut { "mut " } else { "" };
                        format!("{}: {}", is_mut.to_owned() + &a.name, a.arg_type.arg_type)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "function({}): {}", arg_str, hotype)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Apply {
    pub pos: Pos,
    pub module: ModuleName,
    pub transformer: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDef {
    pub pos: Pos,
    pub name: String,
    pub type_args: Vec<String>,
    pub alias: Option<DType>,
    pub is_extern: bool,
}

impl Display for TypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_extern {
            if self.type_args.is_empty() {
                write!(f, "extern type {};", self.name)
            } else {
                let joined = self
                    .type_args
                    .iter()
                    .map(|a| format!("'{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "extern type {}<{}>;", self.name, joined)
            }
        } else if let Some(alias) = &self.alias {
            if self.type_args.is_empty() {
                write!(f, "typedef {} = {};", self.name, alias)
            } else {
                let joined = self
                    .type_args
                    .iter()
                    .map(|a| format!("'{}", a))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "typedef {}<{}> = {};", self.name, joined, alias)
            }
        } else {
            write!(f, "extern type {};", self.name)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DatalogProgram {
    pub imports: Vec<Import>,
    pub typedefs: BTreeMap<String, TypeDef>,
    pub functions: BTreeMap<String, Vec<Function>>,
    pub transformers: BTreeMap<String, Transformer>,
    pub relations: BTreeMap<String, Relation>,
    pub indexes: BTreeMap<String, Index>,
    pub rules: Vec<Rule>,
    pub applys: Vec<Apply>,
    pub sources: BTreeMap<String, String>,
}

impl DatalogProgram {
    pub fn new() -> Self {
        Self {
            imports: vec![],
            typedefs: BTreeMap::new(),
            functions: BTreeMap::new(),
            transformers: BTreeMap::new(),
            relations: BTreeMap::new(),
            indexes: BTreeMap::new(),
            rules: vec![],
            applys: vec![],
            sources: BTreeMap::new(),
        }
    }

    pub fn add_import(mut self, imp: Import) -> Self {
        self.imports.push(imp);
        self
    }

    pub fn add_typedef(mut self, tdef: TypeDef) -> Self {
        self.typedefs.insert(tdef.name.clone(), tdef);
        self
    }

    pub fn add_function(mut self, func: Function) -> Self {
        self.functions
            .entry(func.name.clone())
            .or_insert_with(Vec::new)
            .push(func);
        self
    }

    pub fn add_transformer(mut self, tr: Transformer) -> Self {
        self.transformers.insert(tr.name.clone(), tr);
        self
    }

    pub fn add_relation(mut self, rel: Relation) -> Self {
        self.relations.insert(rel.name.clone(), rel);
        self
    }

    pub fn add_index(mut self, idx: Index) -> Self {
        self.indexes.insert(idx.name.clone(), idx);
        self
    }

    pub fn add_rule(mut self, rule: Rule) -> Self {
        self.rules.push(rule);
        self
    }

    pub fn add_apply(mut self, app: Apply) -> Self {
        self.applys.push(app);
        self
    }

    pub fn set_source(mut self, module: &str, code: &str) -> Self {
        self.sources.insert(module.into(), code.into());
        self
    }
}

impl Display for DatalogProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for i in &self.imports {
            writeln!(f, "{}", i)?;
        }
        for td in self.typedefs.values() {
            writeln!(f, "{}", td)?;
        }
        for rel in self.relations.values() {
            writeln!(f, "{}", rel)?;
        }
        for (_, idx) in &self.indexes {
            writeln!(f, "{}", idx)?;
        }
        for rule in &self.rules {
            writeln!(f, "{}", rule)?;
        }
        for (_, funs) in &self.functions {
            for fun in funs {
                writeln!(f, "{}", fun)?;
            }
        }
        for tr in self.transformers.values() {
            if tr.is_extern {
                write!(f, "extern ")?;
            }
            let inputs_str = tr
                .inputs
                .iter()
                .map(|h| h.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            let outputs_str = tr
                .outputs
                .iter()
                .map(|h| h.to_string())
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(f, "transformer {}({}) -> ({})", tr.name, inputs_str, outputs_str)?;
        }
        for app in &self.applys {
            let in_str = app.inputs.join(", ");
            let out_str = app.outputs.join(", ");
            writeln!(f, "apply {}({}) -> ({}).", app.transformer, in_str, out_str)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eq_operators() {
        let e1 = Expr::var("x");
        let e2 = Expr::var("x");
        assert_eq!(e1, e2);
    }

    #[test]
    fn test_float_type_eq() {
        let t1 = DType::TFloat { pos: Pos::nopos() };
        let t2 = DType::TFloat { pos: Pos::nopos() };
        assert_eq!(t1, t2);
    }

    #[test]
    fn test_struct_display() {
        let c = Constructor {
            pos: Pos::nopos(),
            name: "MyCons".to_string(),
            args: vec![
                Field {
                    pos: Pos::nopos(),
                    name: "x".to_string(),
                    ftype: DType::TInt { pos: Pos::nopos() },
                },
                Field {
                    pos: Pos::nopos(),
                    name: "y".to_string(),
                    ftype: DType::TString { pos: Pos::nopos() },
                },
            ],
        };
        let dt = DType::TStruct {
            pos: Pos::nopos(),
            constructors: vec![c],
        };
        assert!(format!("{}", dt).contains("MyCons{x: bigint, y: string}"));
    }

    #[test]
    fn test_rule_eq() {
        let a1 = Atom {
            pos: Pos::nopos(),
            relation: "Foo".to_string(),
            delay: Delay::zero(),
            diff: false,
            value: Expr::int(10),
        };
        let a2 = a1.clone();
        let lhs1 = RuleLHS {
            pos: Pos::nopos(),
            atom: a1,
            location: None,
        };
        let lhs2 = RuleLHS {
            pos: Pos::nopos(),
            atom: a2,
            location: None,
        };
        let r1 = Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![lhs1],
            rhs: vec![],
        };
        let r2 = Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![lhs2],
            rhs: vec![],
        };
        assert_eq!(r1, r2);
    }
}

fn main() {
    let prog = DatalogProgram::new()
        .add_relation(Relation {
            pos: Pos::nopos(),
            role: RelationRole::RelInternal,
            semantics: RelationSemantics::RelSet,
            name: "Example".to_string(),
            rtype: DType::TBit { pos: Pos::nopos(), width: 8 },
            primary_key: None,
        })
        .add_rule(Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "Example".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::var("x"),
                },
                location: None,
            }],
            rhs: vec![RuleRHS::RHSLiteral {
                pos: Pos::nopos(),
                polarity: true,
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "Other".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::var("x"),
                },
            }],
        });
    println!("{}", prog);
}
