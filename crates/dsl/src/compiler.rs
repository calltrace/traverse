use crate::ast::Lval;
use crate::ddlog_lang::{
    Atom, DType, DatalogProgram, Delay, Expr, ExprNode, ModuleName, Pos, Relation, RelationRole,
    RelationSemantics, Rule as DdlogRule, RuleLHS, RuleRHS,
};
use crate::error::{BlisprResult, Error};
use crate::parser::{lval_read, Grammar, Rule}; // Ensure Rule is imported
use pest::Parser; // Ensure Lval is imported

fn to_ast(input: &str) -> BlisprResult {
    let pairs = Grammar::parse(Rule::program, input)
        .map_err(|e| Error::Parse(format!("Parse error: {}", e)))?;
    let program_pair = pairs
        .into_iter()
        .next()
        .ok_or_else(|| Error::Parse("No content found in the DSL program.".to_string()))?;
    lval_read(program_pair) // Use the existing lval_read function
}

pub fn compile_dsl_to_ddlog(dsl_ast: &Lval) -> Result<DatalogProgram, Error> {
    let mut program = DatalogProgram::new();
    traverse_dsl_ast(dsl_ast, &mut program)?;
    Ok(program)
}

fn traverse_dsl_ast(node: &Lval, program: &mut DatalogProgram) -> Result<(), Error> {
    match node {
        Lval::Sexpr(children) | Lval::Qexpr(children) => {
            if children.is_empty() {
                return Ok(());
            }
            if let Lval::Sym(ref keyword) = *children[0] {
                if keyword == "emit" {
                    handle_emit_expression(&children[1..], program)?;
                }
            }
            for child in children {
                traverse_dsl_ast(child, program)?;
            }
        }
        _ => {}
    }
    Ok(())
}

fn handle_emit_expression(args: &[Box<Lval>], program: &mut DatalogProgram) -> Result<(), Error> {
    if args.is_empty() {
        return Ok(());
    }
    let node_type = match &*args[0] {
        Lval::Sym(s) => s.clone(),
        _ => return Ok(()),
    };
    let mut captures = Vec::new();
    let mut parent: Option<String> = None;
    for arg in &args[1..] {
        match &**arg {
            Lval::Sexpr(sub_children) | Lval::Qexpr(sub_children) => {
                if sub_children.is_empty() {
                    continue;
                }
                if let Lval::Sym(ref sub_keyword) = *sub_children[0] {
                    if sub_keyword == "parent" && sub_children.len() > 1 {
                        if let Lval::Sym(ref parent_node) = *sub_children[1] {
                            parent = Some(parent_node.clone());
                        }
                    } else if sub_children.len() == 2 {
                        if let Lval::Sym(ref relation) = *sub_children[0] {
                            if let Lval::Capture(ref cap_name) = *sub_children[1] {
                                captures.push((relation.clone(), cap_name.clone()));
                            }
                        }
                    }
                }
            }
            Lval::Capture(cap_name) => {
                captures.push(("Capture".to_string(), cap_name.clone()));
            }
            _ => {}
        }
    }
    for (relation, cap) in captures {
        let insert_expr = Expr::new(ExprNode::EInterpolated {
            pos: Pos::nopos(),
            value: format!("{}: {}", node_type, cap),
        });
        let atom = Atom {
            pos: Pos::nopos(),
            relation: relation.clone(),
            delay: Delay::zero(),
            diff: false,
            value: insert_expr,
        };
        let lhs = RuleLHS {
            pos: Pos::nopos(),
            atom,
            location: None,
        };
        let condition_expr = Expr::new(ExprNode::EInterpolated {
            pos: Pos::nopos(),
            value: format!("Condition for {}", cap),
        });
        let rhs = RuleRHS::RHSCondition {
            pos: Pos::nopos(),
            expr: condition_expr,
        };
        let rule = DdlogRule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![lhs],
            rhs: vec![rhs],
        };
        program.add_rule(rule);
    }
    if let Some(parent_node) = parent {
        let relation_name = format!("{}_children", parent_node);
        let insert_expr = Expr::new(ExprNode::ETuple {
            pos: Pos::nopos(),
            items: vec![Expr::int(1), Expr::int(2)],
        });
        let atom = Atom {
            pos: Pos::nopos(),
            relation: relation_name.clone(),
            delay: Delay::zero(),
            diff: false,
            value: insert_expr,
        };
        let lhs = RuleLHS {
            pos: Pos::nopos(),
            atom,
            location: None,
        };
        let condition_expr = Expr::new(ExprNode::EInterpolated {
            pos: Pos::nopos(),
            value: format!("Parent condition for {}", parent_node),
        });
        let rhs = RuleRHS::RHSCondition {
            pos: Pos::nopos(),
            expr: condition_expr,
        };
        let rule = DdlogRule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![lhs],
            rhs: vec![rhs],
        };
        program.add_rule(rule);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use crate::ddlog_lang::Field;

    fn define_solidity_relations(program: &mut DatalogProgram) {
        let solidity_relations = vec![
            (
                "SolidityNode",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "SolidityNode".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "node_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "attributes".to_string(),
                            ftype: DType::TString { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "Capture",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "Capture".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "node_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "value".to_string(),
                            ftype: DType::TString { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "Contract",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "Contract".to_string(),
                    fields: vec![Field {
                        pos: Pos::nopos(),
                        name: "node_id".to_string(),
                        ftype: DType::TInt { pos: Pos::nopos() },
                    }],
                },
                RelationRole::RelInput,
            ),
            (
                "Contract_children",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "Contract_children".to_string(),
                    fields: vec![Field {
                        pos: Pos::nopos(),
                        name: "parent_id".to_string(),
                        ftype: DType::TInt { pos: Pos::nopos() },
                    }],
                },
                RelationRole::RelInput,
            ),
            (
                "Constructor",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "Constructor".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "node_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "attributes".to_string(),
                            ftype: DType::TString { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "Constructor_children",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "Constructor_children".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "parent_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "child_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "FunctionDefinition",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "FunctionDefinition".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "node_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "attributes".to_string(),
                            ftype: DType::TString { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "FunctionDefinition_children",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "FunctionDefinition_children".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "parent_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "child_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "Modifier",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "Modifier".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "node_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "attributes".to_string(),
                            ftype: DType::TString { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "Modifier_children",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "Modifier_children".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "parent_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "child_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
        ];

        for (name, dtype, role) in solidity_relations {
            let rel = Relation {
                pos: Pos::nopos(),
                role,
                semantics: RelationSemantics::RelSet,
                name: name.to_string(),
                rtype: dtype,
                primary_key: None,
            };
            program.add_relation(rel);
        }
    }

    fn define_mermaid_relations(program: &mut DatalogProgram) {
        let mermaid_relations = vec![
            (
                "SequenceStmt",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "SequenceStmt".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "node_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "description".to_string(),
                            ftype: DType::TString { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
            (
                "DiagramSequence_children",
                DType::TStruct {
                    pos: Pos::nopos(),
                    name: "DiagramSequence_children".to_string(),
                    fields: vec![
                        Field {
                            pos: Pos::nopos(),
                            name: "parent_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                        Field {
                            pos: Pos::nopos(),
                            name: "child_id".to_string(),
                            ftype: DType::TInt { pos: Pos::nopos() },
                        },
                    ],
                },
                RelationRole::RelInput,
            ),
        ];

        for (name, dtype, role) in mermaid_relations {
            let rel = Relation {
                pos: Pos::nopos(),
                role,
                semantics: RelationSemantics::RelSet,
                name: name.to_string(),
                rtype: dtype,
                primary_key: None,
            };
            program.add_relation(rel);
        }
    }
    // Helper function to create a Position without specific line and column
    fn nopos() -> Pos {
        Pos { line: 0, column: 0 }
    }

    // Additional Tests Based on `ddlog_lang` Changes

    // Test: Basic Expressions and Display
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

    // Test: Interpolation in Rules
    #[test]
    fn test_interpolation_in_rule() {
        let rule = DdlogRule {
            pos: nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: nopos(),
                atom: Atom {
                    pos: nopos(),
                    relation: "Foo".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("Hello ${world}"),
                },
                location: None,
            }],
            rhs: vec![RuleRHS::RHSCondition {
                pos: nopos(),
                expr: Expr::interpolated("Check ${stuff}"),
            }],
        };
        assert_eq!(
            format!("{}", rule),
            "Foo([|Hello ${world}|]) :- [|Check ${stuff}|]."
        );
    }

    // Test: Multiple Interpolations in Rules
    #[test]
    fn test_multiple_interpolations() {
        let rule = DdlogRule {
            pos: nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: nopos(),
                atom: Atom {
                    pos: nopos(),
                    relation: "Bar".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("Greeting ${name}"),
                },
                location: None,
            }],
            rhs: vec![
                RuleRHS::RHSCondition {
                    pos: nopos(),
                    expr: Expr::interpolated("First ${this}"),
                },
                RuleRHS::RHSCondition {
                    pos: nopos(),
                    expr: Expr::interpolated("Then ${that}"),
                },
            ],
        };
        let rule_str = format!("{}", rule);
        assert_eq!(
            rule_str,
            "Bar([|Greeting ${name}|]) :- [|First ${this}|], [|Then ${that}|]."
        );
    }

    // Test: Program with Interpolations
    #[test]
    fn test_program_with_interpolations() {
        let rel = Relation {
            pos: nopos(),
            role: RelationRole::RelInput,
            semantics: RelationSemantics::RelSet,
            name: "TestRel".to_string(),
            rtype: DType::TInt { pos: nopos() },
            primary_key: None,
        };
        let rule = DdlogRule {
            pos: nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: nopos(),
                atom: Atom {
                    pos: nopos(),
                    relation: "TestRel".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::int(1),
                },
                location: None,
            }],
            rhs: vec![RuleRHS::RHSCondition {
                pos: nopos(),
                expr: Expr::interpolated("Check ${val}"),
            }],
        };
        let mut prog = DatalogProgram::new();
        prog.add_relation(rel).add_rule(rule);
        let out = format!("{}", prog);
        let expected_ddlog = r#"input relation TestRel(bigint);
TestRel(1) :- [|Check ${val}|].
"#;
        assert_eq!(
            out, expected_ddlog,
            "The emitted DDlog program does not match the expected output."
        );
    }

    // Test: Output Relation with Interpolation
    #[test]
    fn test_output_relation_with_interpolation() {
        let output_relation = Relation {
            pos: nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "OutputRel".to_string(),
            rtype: DType::TString { pos: nopos() },
            primary_key: None,
        };
        let rule = DdlogRule {
            pos: nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: nopos(),
                atom: Atom {
                    pos: nopos(),
                    relation: "OutputRel".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("Result ${x}"),
                },
                location: None,
            }],
            rhs: vec![RuleRHS::RHSCondition {
                pos: nopos(),
                expr: Expr::interpolated("Compute ${x} from something"),
            }],
        };
        let mut program = DatalogProgram::new();
        program.add_relation(output_relation).add_rule(rule);
        let out = format!("{}", program);
        let expected_ddlog = r#"output relation OutputRel(string);
OutputRel([|Result ${x}|]) :- [|Compute ${x} from something|].
"#;
        assert_eq!(
            out, expected_ddlog,
            "The emitted DDlog program does not match the expected output."
        );
    }

    // Test: Output Relation with Literal and Interpolation
    #[test]
    fn test_output_relation_with_literal_and_interpolation() {
        let output_relation = Relation {
            pos: nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "OutputRel2".to_string(),
            rtype: DType::TString { pos: nopos() },
            primary_key: None,
        };
        let rule = DdlogRule {
            pos: nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: nopos(),
                atom: Atom {
                    pos: nopos(),
                    relation: "OutputRel2".to_string(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated("String ${some_val}"),
                },
                location: None,
            }],
            rhs: vec![
                RuleRHS::RHSLiteral {
                    pos: nopos(),
                    polarity: true,
                    atom: Atom {
                        pos: nopos(),
                        relation: "CheckRelation".to_string(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::string_lit("foo"),
                    },
                },
                RuleRHS::RHSCondition {
                    pos: nopos(),
                    expr: Expr::interpolated("Also ${other_val} needed"),
                },
            ],
        };
        let mut program = DatalogProgram::new();
        program.add_relation(output_relation).add_rule(rule);
        let out = format!("{}", program);
        let expected_ddlog = r#"output relation OutputRel2(string);
OutputRel2([|String ${some_val}|]) :- CheckRelation("foo"), [|Also ${other_val} needed|].
"#;
        assert_eq!(
            out, expected_ddlog,
            "The emitted DDlog program does not match the expected output."
        );
    }

    // Test: Standalone Output Relation
    #[test]
    fn test_output_relation_standalone() {
        let output_relation = Relation {
            pos: nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "Alone".to_string(),
            rtype: DType::TInt { pos: nopos() },
            primary_key: None,
        };
        let mut program = DatalogProgram::new();
        program.add_relation(output_relation);
        let out = format!("{}", program);
        let expected_ddlog = r#"output relation Alone(bigint);
"#;
        assert_eq!(
            out, expected_ddlog,
            "The emitted DDlog program does not match the expected output."
        );
    }

    // Test: Output Relation in Code
    #[test]
    fn test_output_relation_in_code() {
        let mut program = DatalogProgram::new();
        program
            .add_relation(Relation {
                pos: nopos(),
                role: RelationRole::RelOutput,
                semantics: RelationSemantics::RelSet,
                name: "SampleOut".to_string(),
                rtype: DType::TString { pos: nopos() },
                primary_key: None,
            })
            .add_rule(DdlogRule {
                pos: nopos(),
                module: ModuleName { path: vec![] },
                lhs: vec![RuleLHS {
                    pos: nopos(),
                    atom: Atom {
                        pos: nopos(),
                        relation: "SampleOut".to_string(),
                        delay: Delay::zero(),
                        diff: false,
                        value: Expr::interpolated("FinalValue ${result}"),
                    },
                    location: None,
                }],
                rhs: vec![RuleRHS::RHSCondition {
                    pos: nopos(),
                    expr: Expr::interpolated("Compute ${result} from data"),
                }],
            });
        let output = format!("{}", program);
        let expected_ddlog = r#"output relation SampleOut(string);
SampleOut([|FinalValue ${result}|]) :- [|Compute ${result} from data|].
"#;
        assert_eq!(
            output, expected_ddlog,
            "The emitted DDlog program does not match the expected output."
        );
    }

    // Test: Transformation from LangX AST to DDLog
    #[test]
    fn test_langx_transformation() {
        #[derive(Debug, Clone, PartialEq, Eq)]
        enum LangXNode {
            Emit(String),
        }

        fn transform_langx_to_ddlog(node: &LangXNode) -> DatalogProgram {
            let mut prog = DatalogProgram::new();
            match node {
                LangXNode::Emit(val) => {
                    let output_rel = Relation {
                        pos: nopos(),
                        role: RelationRole::RelOutput,
                        semantics: RelationSemantics::RelSet,
                        name: "LangXOut".to_string(),
                        rtype: DType::TString { pos: nopos() },
                        primary_key: None,
                    };
                    prog.add_relation(output_rel);
                    let rule = DdlogRule {
                        pos: nopos(),
                        module: ModuleName { path: vec![] },
                        lhs: vec![RuleLHS {
                            pos: nopos(),
                            atom: Atom {
                                pos: nopos(),
                                relation: "LangXOut".to_string(),
                                delay: Delay::zero(),
                                diff: false,
                                value: Expr::interpolated(&format!("Value from X: {}", val)),
                            },
                            location: None,
                        }],
                        rhs: vec![RuleRHS::RHSCondition {
                            pos: nopos(),
                            expr: Expr::interpolated("Some check from DSL X"),
                        }],
                    };
                    prog.add_rule(rule);
                }
            }
            prog
        }

        let node = LangXNode::Emit("xyz_value".to_string());
        let ddlog_prog = transform_langx_to_ddlog(&node);
        let printed = format!("{}", ddlog_prog);
        let expected_ddlog = r#"output relation LangXOut(string);
LangXOut([|Value from X: xyz_value|]) :- [|Some check from DSL X|].
"#;
        assert_eq!(
            printed, expected_ddlog,
            "The emitted DDlog program does not match the expected output."
        );
    }

    // Test: Mutable API Usage
    #[test]
    fn test_mut_api_usage() {
        let mut prog = DatalogProgram::new();
        prog.add_relation(Relation {
            pos: nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: "TestRel".to_string(),
            rtype: DType::TInt { pos: nopos() },
            primary_key: None,
        })
        .add_rule(DdlogRule {
            pos: nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: nopos(),
                atom: Atom {
                    pos: nopos(),
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
        let expected_ddlog = r#"output relation TestRel(bigint);
TestRel(42) :- .
"#;
        assert_eq!(
            output, expected_ddlog,
            "The emitted DDlog program does not match the expected output."
        );
    }
}






