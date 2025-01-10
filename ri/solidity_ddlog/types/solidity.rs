#![allow(
    path_statements,
    unused_imports,
    non_snake_case,
    non_camel_case_types,
    non_upper_case_globals,
    unused_parens,
    non_shorthand_field_patterns,
    dead_code,
    overflowing_literals,
    unreachable_patterns,
    unused_variables,
    clippy::missing_safety_doc,
    clippy::match_single_binding,
    clippy::ptr_arg,
    clippy::redundant_closure,
    clippy::needless_lifetimes,
    clippy::borrowed_box,
    clippy::map_clone,
    clippy::toplevel_ref_arg,
    clippy::double_parens,
    clippy::collapsible_if,
    clippy::clone_on_copy,
    clippy::unused_unit,
    clippy::deref_addrof,
    clippy::clone_on_copy,
    clippy::needless_return,
    clippy::op_ref,
    clippy::match_like_matches_macro,
    clippy::comparison_chain,
    clippy::len_zero,
    clippy::extra_unused_lifetimes
)]

use ::num::One;
use ::std::ops::Deref;

use ::differential_dataflow::collection;
use ::timely::communication;
use ::timely::dataflow::scopes;
use ::timely::worker;

use ::ddlog_derive::{FromRecord, IntoRecord, Mutator};
use ::differential_datalog::ddval::DDValConvert;
use ::differential_datalog::program;
use ::differential_datalog::program::TupleTS;
use ::differential_datalog::program::XFormArrangement;
use ::differential_datalog::program::XFormCollection;
use ::differential_datalog::program::Weight;
use ::differential_datalog::record::FromRecord;
use ::differential_datalog::record::FromRecordInner;
use ::differential_datalog::record::IntoRecord;
use ::differential_datalog::record::Mutator;
use ::differential_datalog::record::MutatorInner;
use ::serde::Deserialize;
use ::serde::Serialize;


// `usize` and `isize` are builtin Rust types; we therefore declare an alias to DDlog's `usize` and
// `isize`.
pub type std_usize = u64;
pub type std_isize = i64;


pub static __STATIC_11: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", Child = "###));
pub static __STATIC_13: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", Expression = "###));
pub static __STATIC_1: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", Name = "###));
pub static __STATIC_15: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", Operand = "###));
pub static __STATIC_16: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", OperandType = "###));
pub static __STATIC_8: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", Operator = "###));
pub static __STATIC_3: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", StateMutability = "###));
pub static __STATIC_5: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", Type = "###));
pub static __STATIC_2: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###", Visibility = "###));
pub static __STATIC_7: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"BinaryExpressionNode: ID = "###));
pub static __STATIC_10: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"ChildEdge: Parent = "###));
pub static __STATIC_12: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"ExpressionEdge: Statement = "###));
pub static __STATIC_0: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"FunctionDefinitionNode: ID = "###));
pub static __STATIC_9: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"IdentifierNode: ID = "###));
pub static __STATIC_14: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"OperandEdge: Expression = "###));
pub static __STATIC_4: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"ParameterNode: ID = "###));
pub static __STATIC_6: ::once_cell::sync::Lazy<String> = ::once_cell::sync::Lazy::new(|| String::from(r###"ReturnStatementNode: ID = "###));
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "BinaryExpressionNode")]
pub struct BinaryExpressionNode {
    pub id: NodeId,
    pub operator: String
}
impl abomonation::Abomonation for BinaryExpressionNode{}
impl ::std::fmt::Display for BinaryExpressionNode {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            BinaryExpressionNode{id,operator} => {
                __formatter.write_str("BinaryExpressionNode{")?;
                differential_datalog::record::format_ddlog_str(id, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(operator, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for BinaryExpressionNode {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "ChildEdge")]
pub struct ChildEdge {
    pub parent: NodeId,
    pub child: NodeId
}
impl abomonation::Abomonation for ChildEdge{}
impl ::std::fmt::Display for ChildEdge {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            ChildEdge{parent,child} => {
                __formatter.write_str("ChildEdge{")?;
                differential_datalog::record::format_ddlog_str(parent, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(child, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for ChildEdge {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "DumpTree")]
pub struct DumpTree {
    pub phrase: String
}
impl abomonation::Abomonation for DumpTree{}
impl ::std::fmt::Display for DumpTree {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            DumpTree{phrase} => {
                __formatter.write_str("DumpTree{")?;
                differential_datalog::record::format_ddlog_str(phrase, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for DumpTree {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "ExpressionEdge")]
pub struct ExpressionEdge {
    pub statement: NodeId,
    pub expression: NodeId
}
impl abomonation::Abomonation for ExpressionEdge{}
impl ::std::fmt::Display for ExpressionEdge {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            ExpressionEdge{statement,expression} => {
                __formatter.write_str("ExpressionEdge{")?;
                differential_datalog::record::format_ddlog_str(statement, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(expression, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for ExpressionEdge {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "FunctionDefinitionNode")]
pub struct FunctionDefinitionNode {
    pub id: NodeId,
    pub name: String,
    pub visibility: String,
    pub state_mutability: String
}
impl abomonation::Abomonation for FunctionDefinitionNode{}
impl ::std::fmt::Display for FunctionDefinitionNode {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            FunctionDefinitionNode{id,name,visibility,state_mutability} => {
                __formatter.write_str("FunctionDefinitionNode{")?;
                differential_datalog::record::format_ddlog_str(id, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(name, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(visibility, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(state_mutability, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for FunctionDefinitionNode {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "IdentifierNode")]
pub struct IdentifierNode {
    pub id: NodeId,
    pub name: String
}
impl abomonation::Abomonation for IdentifierNode{}
impl ::std::fmt::Display for IdentifierNode {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            IdentifierNode{id,name} => {
                __formatter.write_str("IdentifierNode{")?;
                differential_datalog::record::format_ddlog_str(id, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(name, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for IdentifierNode {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
pub type NodeId = String;
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "OperandEdge")]
pub struct OperandEdge {
    pub expression: NodeId,
    pub operand: NodeId,
    pub operand_type: String
}
impl abomonation::Abomonation for OperandEdge{}
impl ::std::fmt::Display for OperandEdge {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            OperandEdge{expression,operand,operand_type} => {
                __formatter.write_str("OperandEdge{")?;
                differential_datalog::record::format_ddlog_str(expression, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(operand, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(operand_type, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for OperandEdge {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "ParameterNode")]
pub struct ParameterNode {
    pub id: NodeId,
    pub name: String,
    pub param_type: String
}
impl abomonation::Abomonation for ParameterNode{}
impl ::std::fmt::Display for ParameterNode {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            ParameterNode{id,name,param_type} => {
                __formatter.write_str("ParameterNode{")?;
                differential_datalog::record::format_ddlog_str(id, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(name, __formatter)?;
                __formatter.write_str(",")?;
                differential_datalog::record::format_ddlog_str(param_type, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for ParameterNode {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
#[derive(Eq, Ord, Clone, Hash, PartialEq, PartialOrd, IntoRecord, Mutator, Default, Serialize, Deserialize, FromRecord)]
#[ddlog(rename = "ReturnStatementNode")]
pub struct ReturnStatementNode {
    pub id: NodeId
}
impl abomonation::Abomonation for ReturnStatementNode{}
impl ::std::fmt::Display for ReturnStatementNode {
    fn fmt(&self, __formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
            ReturnStatementNode{id} => {
                __formatter.write_str("ReturnStatementNode{")?;
                differential_datalog::record::format_ddlog_str(id, __formatter)?;
                __formatter.write_str("}")
            }
        }
    }
}
impl ::std::fmt::Debug for ReturnStatementNode {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Display::fmt(&self, f)
    }
}
pub static __Arng___Null_0 : ::once_cell::sync::Lazy<program::Arrangement> = ::once_cell::sync::Lazy::new(|| {
    program::Arrangement::Map{
       debug_info:  Default::default(),
        afun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<(::differential_datalog::ddval::DDValue,::differential_datalog::ddval::DDValue)>
        {
            let __cloned = __v.clone();
            match unsafe { <()>::from_ddvalue_unchecked(__v) } {
                _ => Some((()).into_ddvalue()),
                _ => None
            }.map(|x|(x,__cloned))
        }
        __f},
        queryable: true
    }
});
pub static __Rule_DumpTree_0 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=((((((("FunctionDefinitionNode: ID = " ++ id) ++ ", Name = ") ++ name) ++ ", Visibility = ") ++ visibility) ++ ", StateMutability = ") ++ state_mutability)}: DumpTree)] :- FunctionDefinitionNode[(FunctionDefinitionNode{.id=(id: string), .name=(name: string), .visibility=(visibility: string), .state_mutability=(state_mutability: string)}: FunctionDefinitionNode)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 52, 1, 57, 1)),
        rel: 4,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 52, 1, 53, 89)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let (ref id, ref name, ref visibility, ref state_mutability) = match *unsafe { <FunctionDefinitionNode>::from_ddvalue_ref_unchecked(&__v) } {
                                                        FunctionDefinitionNode{id: ref id, name: ref name, visibility: ref visibility, state_mutability: ref state_mutability} => ((*id).clone(), (*name).clone(), (*visibility).clone(), (*state_mutability).clone()),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append((*(&*crate::__STATIC_0)).clone(), id), r###", Name = "###), name), r###", Visibility = "###), visibility), r###", StateMutability = "###), state_mutability)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});
pub static __Rule_DumpTree_1 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=((((("ParameterNode: ID = " ++ id) ++ ", Name = ") ++ name) ++ ", Type = ") ++ param_type)}: DumpTree)] :- ParameterNode[(ParameterNode{.id=(id: string), .name=(name: string), .param_type=(param_type: string)}: ParameterNode)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 57, 1, 61, 1)),
        rel: 7,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 57, 1, 57, 92)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let (ref id, ref name, ref param_type) = match *unsafe { <ParameterNode>::from_ddvalue_ref_unchecked(&__v) } {
                                                        ParameterNode{id: ref id, name: ref name, param_type: ref param_type} => ((*id).clone(), (*name).clone(), (*param_type).clone()),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append((*(&*crate::__STATIC_4)).clone(), id), r###", Name = "###), name), r###", Type = "###), param_type)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});
pub static __Rule_DumpTree_2 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=("ReturnStatementNode: ID = " ++ id)}: DumpTree)] :- ReturnStatementNode[(ReturnStatementNode{.id=(id: string)}: ReturnStatementNode)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 61, 1, 65, 1)),
        rel: 8,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 61, 1, 61, 46)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let ref id = match *unsafe { <ReturnStatementNode>::from_ddvalue_ref_unchecked(&__v) } {
                                                        ReturnStatementNode{id: ref id} => (*id).clone(),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append((*(&*crate::__STATIC_6)).clone(), id)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});
pub static __Rule_DumpTree_3 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=((("BinaryExpressionNode: ID = " ++ id) ++ ", Operator = ") ++ operator)}: DumpTree)] :- BinaryExpressionNode[(BinaryExpressionNode{.id=(id: string), .operator=(operator: string)}: BinaryExpressionNode)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 65, 1, 69, 1)),
        rel: 0,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 65, 1, 65, 78)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let (ref id, ref operator) = match *unsafe { <BinaryExpressionNode>::from_ddvalue_ref_unchecked(&__v) } {
                                                        BinaryExpressionNode{id: ref id, operator: ref operator} => ((*id).clone(), (*operator).clone()),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append((*(&*crate::__STATIC_7)).clone(), id), r###", Operator = "###), operator)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});
pub static __Rule_DumpTree_4 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=((("IdentifierNode: ID = " ++ id) ++ ", Name = ") ++ name)}: DumpTree)] :- IdentifierNode[(IdentifierNode{.id=(id: string), .name=(name: string)}: IdentifierNode)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 69, 1, 73, 1)),
        rel: 5,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 69, 1, 69, 64)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let (ref id, ref name) = match *unsafe { <IdentifierNode>::from_ddvalue_ref_unchecked(&__v) } {
                                                        IdentifierNode{id: ref id, name: ref name} => ((*id).clone(), (*name).clone()),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append((*(&*crate::__STATIC_9)).clone(), id), r###", Name = "###), name)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});
pub static __Rule_DumpTree_5 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=((("ChildEdge: Parent = " ++ parent) ++ ", Child = ") ++ child)}: DumpTree)] :- ChildEdge[(ChildEdge{.parent=(parent: string), .child=(child: string)}: ChildEdge)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 73, 1, 77, 1)),
        rel: 1,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 73, 1, 73, 69)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let (ref parent, ref child) = match *unsafe { <ChildEdge>::from_ddvalue_ref_unchecked(&__v) } {
                                                        ChildEdge{parent: ref parent, child: ref child} => ((*parent).clone(), (*child).clone()),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append((*(&*crate::__STATIC_10)).clone(), parent), r###", Child = "###), child)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});
pub static __Rule_DumpTree_6 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=((("ExpressionEdge: Statement = " ++ statement) ++ ", Expression = ") ++ expression)}: DumpTree)] :- ExpressionEdge[(ExpressionEdge{.statement=(statement: string), .expression=(expression: string)}: ExpressionEdge)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 77, 1, 81, 1)),
        rel: 3,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 77, 1, 77, 90)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let (ref statement, ref expression) = match *unsafe { <ExpressionEdge>::from_ddvalue_ref_unchecked(&__v) } {
                                                        ExpressionEdge{statement: ref statement, expression: ref expression} => ((*statement).clone(), (*expression).clone()),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append((*(&*crate::__STATIC_12)).clone(), statement), r###", Expression = "###), expression)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});
pub static __Rule_DumpTree_7 : ::once_cell::sync::Lazy<program::Rule> = ::once_cell::sync::Lazy::new(|| {
    /* DumpTree[(DumpTree{.phrase=((((("OperandEdge: Expression = " ++ expression) ++ ", Operand = ") ++ operand) ++ ", OperandType = ") ++ operand_type)}: DumpTree)] :- OperandEdge[(OperandEdge{.expression=(expression: string), .operand=(operand: string), .operand_type=(operand_type: string)}: OperandEdge)]. */
    ::differential_datalog::program::Rule::CollectionRule {
        debug_info: ::ddlog_profiler::RuleDebugInfo::new(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 81, 1, 84, 1)),
        rel: 6,
        xform: ::core::option::Option::Some(XFormCollection::FilterMap{
                                                debug_info: ::ddlog_profiler::OperatorDebugInfo::head(::ddlog_profiler::SourcePosition::new_range("solidity.dl", 81, 1, 82, 46)),
                                                fmfun: {fn __f(__v: ::differential_datalog::ddval::DDValue) -> ::std::option::Option<::differential_datalog::ddval::DDValue>
                                                {
                                                    let (ref expression, ref operand, ref operand_type) = match *unsafe { <OperandEdge>::from_ddvalue_ref_unchecked(&__v) } {
                                                        OperandEdge{expression: ref expression, operand: ref operand, operand_type: ref operand_type} => ((*expression).clone(), (*operand).clone(), (*operand_type).clone()),
                                                        _ => return ::core::option::Option::None
                                                    };
                                                    Some(((DumpTree{phrase: ::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append(::ddlog_rt::string_append_str(::ddlog_rt::string_append((*(&*crate::__STATIC_14)).clone(), expression), r###", Operand = "###), operand), r###", OperandType = "###), operand_type)})).into_ddvalue())
                                                }
                                                __f},
                                                next: Box::new(None)
                                            }),
    }
});