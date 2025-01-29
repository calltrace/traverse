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

    pub fn tuple(items: Vec<Expr>) -> Self {
        Self::new(ExprNode::ETuple {
            pos: Pos::nopos(),
            items,
        })
    }

    pub fn apply(func: Expr, args: Vec<Expr>) -> Self {
        Self::new(ExprNode::EApply {
            pos: Pos::nopos(),
            func: Box::new(func),
            args,
        })
    }

    pub fn binop(op: BOp, left: Expr, right: Expr) -> Self {
        Self::new(ExprNode::EBinOp {
            pos: Pos::nopos(),
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    pub fn unop(op: UOp, expr: Expr) -> Self {
        Self::new(ExprNode::EUnOp {
            pos: Pos::nopos(),
            op,
            expr: Box::new(expr),
        })
    }

    pub fn struct_lit(constructor: &str, fields: Vec<(String, Expr)>) -> Self {
        Self::new(ExprNode::EStruct {
            pos: Pos::nopos(),
            constructor: constructor.into(),
            fields,
        })
    }

    pub fn field(base: Expr, field: &str) -> Self {
        Self::new(ExprNode::EField {
            pos: Pos::nopos(),
            base: Box::new(base),
            field: field.into(),
        })
    }

    pub fn ite(cond: Expr, then_expr: Expr, else_expr: Expr) -> Self {
        Self::new(ExprNode::EITE {
            pos: Pos::nopos(),
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        })
    }

    pub fn match_expr(match_expr: Expr, cases: Vec<(Expr, Expr)>) -> Self {
        Self::new(ExprNode::EMatch {
            pos: Pos::nopos(),
            match_expr: Box::new(match_expr),
            cases,
        })
    }

    pub fn seq(first: Expr, second: Expr) -> Self {
        Self::new(ExprNode::ESeq {
            pos: Pos::nopos(),
            first: Box::new(first),
            second: Box::new(second),
        })
    }

    pub fn set(lhs: Expr, rhs: Expr) -> Self {
        Self::new(ExprNode::ESet {
            pos: Pos::nopos(),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }

    pub fn var_decl(name: &str) -> Self {
        Self::new(ExprNode::EVarDecl {
            pos: Pos::nopos(),
            name: name.into(),
        })
    }

    pub fn placeholder() -> Self {
        Self::new(ExprNode::EPHolder { pos: Pos::nopos() })
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
            ExprNode::EBinOp {
                op, left, right, ..
            } => {
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
                    write!(f, "{}", joined)
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
                write!(
                    f,
                    "if ({}) {{ {} }} else {{ {} }}",
                    cond, then_expr, else_expr
                )
            }
            ExprNode::EMatch {
                match_expr, cases, ..
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
pub struct Field {
    pub pos: Pos,
    pub name: String,
    pub ftype: DType,
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
        name: String,
        fields: Vec<Field>,
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

impl DType {
    fn to_ddlog_relation_fields(&self) -> Vec<(String, DType)> {
        match self {
            DType::TStruct { fields, .. } => {
                if fields.is_empty() {
                    vec![(
                        "val".to_string(),
                        DType::TStruct {
                            pos: Pos::nopos(),
                            name: "".to_string(),
                            fields: vec![],
                        },
                    )]
                } else {
                    fields
                        .iter()
                        .map(|fld| (fld.name.clone(), fld.ftype.clone()))
                        .collect()
                }
            }
            // For all other DTypes, treat them as a single field named "val".
            _ => vec![("val".to_string(), self.clone())],
        }
    }
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
            DType::TStruct { name, fields, .. } => {
                let mut parts = vec![];
                for (i, fld) in fields.iter().enumerate() {
                    if i > 0 {
                        parts.push(", ".to_string());
                    }
                    parts.push(format!("{}: {}", fld.name, fld.ftype));
                }
                let joined = parts.join("");
                write!(f, "{}{{{}}}", name, joined)
            }
            DType::TTuple { tup_args, .. } => {
                if tup_args.len() == 1 {
                    write!(f, "{}", tup_args[0])
                } else {
                    let joined = tup_args
                        .iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "({})", joined)
                }
            }
            DType::TUser {
                name, type_args, ..
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
                name, type_args, ..
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

impl Display for Field {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ftype)
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

impl Relation {
    fn ddlog_fields(&self) -> String {
        let fields = self.rtype.to_ddlog_relation_fields();
        let mut rendered = Vec::with_capacity(fields.len());
        for (name, ty) in fields {
            rendered.push(format!("{}: {}", name, ty));
        }
        rendered.join(", ")
    }
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
        write!(
            f,
            "{}{} {}({})",
            role_str,
            sem_str,
            self.name,
            self.ddlog_fields()
        )?;
        if let Some(pk) = &self.primary_key {
            write!(f, " primary key {}", pk)
        } else {
            write!(f, "")
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
        write!(
            f,
            "{}{}{}({})",
            self.relation, diff_str, delay_str, self.value
        )
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
            // treat it as a fact
            write!(f, "{}.", lhs_str)
        } else {
            let rhs_str = self
                .rhs
                .iter()
                .map(|rc| rc.to_string())
                .collect::<Vec<_>>()
                .join(",\n        ");
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
            write!(f, "{}", self.path.join("::"))
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
    pub fn add_relation(&mut self, rel: Relation) -> &mut Self {
        self.relations.insert(rel.name.clone(), rel);
        self
    }
    pub fn add_rule(&mut self, rule: Rule) -> &mut Self {
        self.rules.push(rule);
        self
    }
    pub fn add_import(&mut self, imp: Import) -> &mut Self {
        self.imports.push(imp);
        self
    }
    pub fn add_typedef(&mut self, tdef: TypeDef) -> &mut Self {
        self.typedefs.insert(tdef.name.clone(), tdef);
        self
    }
    pub fn add_function(&mut self, func: Function) -> &mut Self {
        self.functions
            .entry(func.name.clone())
            .or_default()
            .push(func);
        self
    }
    pub fn add_transformer(&mut self, tr: Transformer) -> &mut Self {
        self.transformers.insert(tr.name.clone(), tr);
        self
    }
    pub fn add_index(&mut self, idx: Index) -> &mut Self {
        self.indexes.insert(idx.name.clone(), idx);
        self
    }
    pub fn add_apply(&mut self, app: Apply) -> &mut Self {
        self.applys.push(app);
        self
    }
    pub fn set_source(&mut self, module: &str, code: &str) -> &mut Self {
        self.sources.insert(module.into(), code.into());
        self
    }

    fn dump_relations_ordered(&self) -> String {
        let mut result = String::new();

        // Dump input relations first
        for rel in self.relations.values() {
            if matches!(rel.role, RelationRole::RelInput) {
                result.push_str(&format!("{}\n", rel));
            }
        }

        // Then dump output relations
        for rel in self.relations.values() {
            if matches!(rel.role, RelationRole::RelOutput) {
                result.push_str(&format!("{}\n", rel));
            }
        }

        result
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
        writeln!(f, "{}", self.dump_relations_ordered())?;
        for idx in self.indexes.values() {
            writeln!(f, "{}", idx)?;
        }

        writeln!(f)?;

        for rule in &self.rules {
            writeln!(f, "{}", rule)?;
        }
        for funs in self.functions.values() {
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
            writeln!(
                f,
                "transformer {}({}) -> ({})",
                tr.name, inputs_str, outputs_str
            )?;
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
    fn test_basic_exprs_eq_and_display() {
        let e1 = Expr::var("x");
        let e2 = Expr::int(42);
        let e3 = Expr::string_lit("abc");
        let e4 = Expr::interpolated("Hello ${world}");
        assert_eq!(format!("{}", e1), "x");
        assert_eq!(format!("{}", e2), "42");
        assert_eq!(format!("{}", e3), "\"abc\"");
        assert_eq!(format!("{}", e4), "[|Hello ${world}|]");
    }

    #[test]
    fn test_interpolation_in_rule() {
        let rule = Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "Foo".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("Hello ${world}"),
                },
                location: None,
            }],
            rhs: vec![RuleRHS::RHSCondition {
                pos: Pos::nopos(),
                expr: Expr::interpolated("Check ${stuff}"),
            }],
        };
        // Expecting "Foo([|Hello ${world}|]) :- [|Check ${stuff}|]."
        assert_eq!(
            format!("{}", rule),
            "Foo([|Hello ${world}|]) :- [|Check ${stuff}|]."
        );
    }

    #[test]
    fn test_multiple_interpolations() {
        let rule = Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "Bar".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("Greeting ${name}"),
                },
                location: None,
            }],
            rhs: vec![
                RuleRHS::RHSCondition {
                    pos: Pos::nopos(),
                    expr: Expr::interpolated("First ${this}"),
                },
                RuleRHS::RHSCondition {
                    pos: Pos::nopos(),
                    expr: Expr::interpolated("Then ${that}"),
                },
            ],
        };
        let rule_str = format!("{}", rule);
        // Expecting "Bar([|Greeting ${name}|]) :- [|First ${this}|], [|Then ${that}|]."
        assert_eq!(
            rule_str,
            "Bar([|Greeting ${name}|]) :- [|First ${this}|], [|Then ${that}|]."
        );
    }

    #[test]
    fn test_program_with_interpolations() {
        let rel = Relation {
            pos: Pos::nopos(),
            role: RelationRole::RelInternal,
            semantics: RelationSemantics::RelSet,
            name: "TestRel".to_string(),
            rtype: DType::TInt { pos: Pos::nopos() },
            primary_key: None,
        };
        let rule = Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "TestRel".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::int(1),
                },
                location: None,
            }],
            rhs: vec![RuleRHS::RHSCondition {
                pos: Pos::nopos(),
                expr: Expr::interpolated("Check ${val}"),
            }],
        };
        let mut prog = DatalogProgram::new();
        prog.add_relation(rel).add_rule(rule);
        let out = format!("{}", prog);
        // We expect "relation TestRel(bigint);" to appear (the new style).
        assert!(out.contains("TestRel(val: bigint);"));
        // We expect "TestRel(1) :- [|Check ${val}|]."
        assert!(out.contains("TestRel(1) :- [|Check ${val}|]."));
    }

    #[test]
    fn test_output_relation_with_interpolation() {
        let output_relation = Relation {
            pos: Pos::nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "OutputRel".to_string(),
            rtype: DType::TString { pos: Pos::nopos() },
            primary_key: None,
        };
        let rule = Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "OutputRel".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("Result ${x}"),
                },
                location: None,
            }],
            rhs: vec![RuleRHS::RHSCondition {
                pos: Pos::nopos(),
                expr: Expr::interpolated("Compute ${x} from something"),
            }],
        };
        let mut program = DatalogProgram::new();
        program.add_relation(output_relation).add_rule(rule);
        let out = format!("{}", program);
        // e.g. "output relation OutputRel(val: string);"
        assert!(out.contains("output relation OutputRel(val: string);"));
        assert!(out.contains("OutputRel([|Result ${x}|]) :- [|Compute ${x} from something|]."));
    }

    #[test]
    fn test_output_relation_with_literal_and_interpolation() {
        let output_relation = Relation {
            pos: Pos::nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "OutputRel2".to_string(),
            rtype: DType::TString { pos: Pos::nopos() },
            primary_key: None,
        };
        let rule = Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "OutputRel2".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("String ${some_val}"),
                },
                location: None,
            }],
            rhs: vec![
                RuleRHS::RHSLiteral {
                    pos: Pos::nopos(),
                    polarity: true,
                    atom: Atom {
                        pos: Pos::nopos(),
                        relation: "CheckRelation".to_string(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::string_lit("foo"),
                    },
                },
                RuleRHS::RHSCondition {
                    pos: Pos::nopos(),
                    expr: Expr::interpolated("Also ${other_val} needed"),
                },
            ],
        };
        let mut program = DatalogProgram::new();
        program.add_relation(output_relation).add_rule(rule);
        let out = format!("{}", program);
        // e.g. "output relation OutputRel2(val: string);"
        assert!(out.contains("output relation OutputRel2(val: string);"));
        assert!(out.contains("OutputRel2([|String ${some_val}|]) :- CheckRelation(\"foo\"), [|Also ${other_val} needed|]."));
    }

    #[test]
    fn test_output_relation_standalone() {
        let output_relation = Relation {
            pos: Pos::nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "Alone".to_string(),
            rtype: DType::TInt { pos: Pos::nopos() },
            primary_key: None,
        };
        let mut program = DatalogProgram::new();
        program.add_relation(output_relation);
        let out = format!("{}", program);
        // e.g. "output relation Alone(val: bigint);"
        assert!(out.contains("output relation Alone(val: bigint);"));
    }

    #[test]
    fn test_output_relation_in_code() {
        let mut program = DatalogProgram::new();
        program
            .add_relation(Relation {
                pos: Pos::nopos(),
                role: RelationRole::RelOutput,
                semantics: RelationSemantics::RelSet,
                name: "SampleOut".to_string(),
                rtype: DType::TString { pos: Pos::nopos() },
                primary_key: None,
            })
            .add_rule(Rule {
                pos: Pos::nopos(),
                module: ModuleName { path: vec![] },
                lhs: vec![RuleLHS {
                    pos: Pos::nopos(),
                    atom: Atom {
                        pos: Pos::nopos(),
                        relation: "SampleOut".to_string(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::interpolated("FinalValue ${result}"),
                    },
                    location: None,
                }],
                rhs: vec![RuleRHS::RHSCondition {
                    pos: Pos::nopos(),
                    expr: Expr::interpolated("Compute ${result} from data"),
                }],
            });
        let output = format!("{}", program);
        // e.g. "output relation SampleOut(val: string);"
        assert!(output.contains("output relation SampleOut(val: string);"));
        // "SampleOut([|FinalValue ${result}|]) :- [|Compute ${result} from data|]."
        assert!(output
            .contains("SampleOut([|FinalValue ${result}|]) :- [|Compute ${result} from data|]."));
    }

    #[test]
    fn test_langx_transformation() {
        #[derive(Debug, Clone, PartialEq, Eq)]
        enum LangXNode {
            Emit(String),
            Sequence(Vec<LangXNode>),
            Check(String),
        }

        fn transform_langx_ast(ast: &LangXNode, prog: &mut DatalogProgram) {
            match ast {
                LangXNode::Emit(val) => {
                    let out_rel = Relation {
                        pos: Pos::nopos(),
                        role: RelationRole::RelOutput,
                        semantics: RelationSemantics::RelSet,
                        name: "LangXOut".to_string(),
                        rtype: DType::TString { pos: Pos::nopos() },
                        primary_key: None,
                    };
                    prog.add_relation(out_rel);

                    let rule = Rule {
                        pos: Pos::nopos(),
                        module: ModuleName { path: vec![] },
                        lhs: vec![RuleLHS {
                            pos: Pos::nopos(),
                            atom: Atom {
                                pos: Pos::nopos(),
                                relation: "LangXOut".to_string(),
                                delay: Delay::zero(),
                                diff: false,
                                value: Expr::interpolated(&format!("Value from X: {}", val)),
                            },
                            location: None,
                        }],
                        rhs: vec![RuleRHS::RHSCondition {
                            pos: Pos::nopos(),
                            expr: Expr::interpolated("Some check from DSL X"),
                        }],
                    };

                    prog.add_rule(rule);
                }
                LangXNode::Sequence(nodes) => {
                    for subnode in nodes {
                        transform_langx_ast(subnode, prog);
                    }
                }
                LangXNode::Check(msg) => {
                    let check_rel = Relation {
                        pos: Pos::nopos(),
                        role: RelationRole::RelInternal,
                        semantics: RelationSemantics::RelSet,
                        name: "LangXCheck".to_string(),
                        rtype: DType::TString { pos: Pos::nopos() },
                        primary_key: None,
                    };
                    prog.add_relation(check_rel);

                    let rule = Rule {
                        pos: Pos::nopos(),
                        module: ModuleName { path: vec![] },
                        lhs: vec![RuleLHS {
                            pos: Pos::nopos(),
                            atom: Atom {
                                pos: Pos::nopos(),
                                relation: "LangXCheck".to_string(),
                                delay: Delay::zero(),
                                diff: false,
                                value: Expr::interpolated(&format!("Check: {}", msg)),
                            },
                            location: None,
                        }],
                        rhs: vec![RuleRHS::RHSCondition {
                            pos: Pos::nopos(),
                            expr: Expr::interpolated("Some check condition"),
                        }],
                    };

                    prog.add_rule(rule);
                }
            }
        }

        let ast = LangXNode::Sequence(vec![
            LangXNode::Check("alpha".to_string()),
            LangXNode::Emit("beta".to_string()),
            LangXNode::Sequence(vec![
                LangXNode::Check("deep1".to_string()),
                LangXNode::Emit("deep2".to_string()),
            ]),
        ]);

        let mut ddlog_prog = DatalogProgram::new();
        transform_langx_ast(&ast, &mut ddlog_prog);
        let text = format!("{}", ddlog_prog);

        println!("{}", text);
        // Basic checks
        assert!(text.contains("LangXCheck(val: string);"));
        assert!(text.contains("LangXOut(val: string);"));

        // Check capture strings
        assert!(text.contains("Check: alpha"));
        assert!(text.contains("Value from X: beta"));
        assert!(text.contains("Check: deep1"));
        assert!(text.contains("Value from X: deep2"));
    }

    #[test]
    fn test_mut_api_usage() {
        let mut prog = DatalogProgram::new();
        prog.add_relation(Relation {
            pos: Pos::nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "TestRel".to_string(),
            rtype: DType::TInt { pos: Pos::nopos() },
            primary_key: None,
        })
        .add_rule(Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: "TestRel".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::int(42),
                },
                location: None,
            }],
            rhs: vec![],
        });
        let output = format!("{}", prog);
        // e.g. "output relation TestRel(val: bigint);"
        assert!(output.contains("output relation TestRel(val: bigint);"));
        // "TestRel(42)."
        assert!(output.contains("TestRel(42)."));
    }
}
