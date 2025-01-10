use crate::ast::Lval;
use crate::ddlog_lang::{
    Atom, DType, DatalogProgram, Delay, Expr, ExprNode, ModuleName, Pos, Relation, RelationRole,
    RelationSemantics, Rule as DdlogRule, RuleLHS, RuleRHS,
};
use crate::error::{BlisprResult, Error};
use crate::parser::{lval_read, Grammar, Rule};
use pest::Parser;
use std::collections::{BTreeMap, HashMap};
use std::sync::atomic::{AtomicUsize, Ordering};

static RULE_COUNTER: AtomicUsize = AtomicUsize::new(1);
static RELATION_COUNTER: AtomicUsize = AtomicUsize::new(1);

fn to_ast(input: &str) -> BlisprResult {
    let pairs = Grammar::parse(Rule::program, input)
        .map_err(|e| Error::Parse(format!("Parse error: {}", e)))?;
    let program_pair = pairs
        .into_iter()
        .next()
        .ok_or_else(|| Error::Parse("No content found in the DSL program.".to_string()))?;
    lval_read(program_pair)
}

pub fn transform_ast_to_ddlog(ast: &Lval) -> DatalogProgram {
    let mut program = DatalogProgram::new();
    traverse_lval(ast, &mut program, None);
    program
}

fn traverse_lval(node: &Lval, program: &mut DatalogProgram, parent_rel: Option<String>) {
    match node {
        Lval::Query(children) => {
            for child in children {
                traverse_lval(child, program, parent_rel.clone());
            }
        }
        Lval::CaptureForm(node_type, attributes, nested_opt, _) => {
            handle_capture_form(node_type, attributes, nested_opt.as_deref(), program, parent_rel);
        }
        Lval::Emit(node_type, attrs) => {
            handle_emit_form(node_type, attrs, program, parent_rel);
        }
        Lval::WhenForm(cond, emits) => {
            traverse_lval(cond, program, parent_rel.clone());
            for em in emits {
                traverse_lval(em, program, parent_rel.clone());
            }
        }
        Lval::Sexpr(children)
        | Lval::Qexpr(children)
        | Lval::DoForm(children)
        | Lval::Logical(_, children)
        | Lval::KeyVal(children) => {
            for child in children {
                traverse_lval(child, program, parent_rel.clone());
            }
        }
        Lval::Capture(_) | Lval::Num(_) | Lval::Sym(_) | Lval::Fun(_) => {}
    }
}

fn handle_capture_form(
    node_type: &str,
    attributes: &HashMap<String, Box<Lval>>,
    nested: Option<&Lval>,
    program: &mut DatalogProgram,
    parent_rel: Option<String>,
) {
    let input_rel_name = create_input_relation_for_capture(node_type, program, parent_rel);

    for (attr_name, val) in attributes {
        let rid = RULE_COUNTER.fetch_add(1, Ordering::SeqCst);
        let output_rel_name = format!("Captured_{}_{}_{}", node_type, attr_name, rid);
        let output_relation = Relation {
            pos: Pos::nopos(),
            role: RelationRole::RelOutput,
            semantics: RelationSemantics::RelSet,
            name: output_rel_name.clone(),
            rtype: DType::TString { pos: Pos::nopos() },
            primary_key: None,
        };
        program.add_relation(output_relation);

        let captured_expr = match **val {
            Lval::Capture(ref cap_name) => Expr::new(ExprNode::EInterpolated {
                pos: Pos::nopos(),
                value: format!("Captured Value from {}", cap_name),
            }),
            _ => Expr::new(ExprNode::EString {
                pos: Pos::nopos(),
                value: format!("Non-capture data for attribute {}", attr_name),
            }),
        };

        let lhs_atom = Atom {
            pos: Pos::nopos(),
            relation: output_rel_name,
            delay: Delay::zero(),
            diff: false,
            value: captured_expr,
        };
        let lhs = RuleLHS {
            pos: Pos::nopos(),
            atom: lhs_atom,
            location: None,
        };

        let cond_expr = Expr::new(ExprNode::EInterpolated {
            pos: Pos::nopos(),
            value: format!("FromRelation {}", input_rel_name),
        });
        let rhs_condition = RuleRHS::RHSCondition {
            pos: Pos::nopos(),
            expr: cond_expr,
        };

        let ddlog_rule = DdlogRule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![lhs],
            rhs: vec![rhs_condition],
        };
        program.add_rule(ddlog_rule);
    }
    if let Some(n) = nested {
        traverse_lval(n, program, Some(input_rel_name));
    }
}

fn create_input_relation_for_capture(
    node_type: &str,
    program: &mut DatalogProgram,
    parent_rel: Option<String>,
) -> String {
    let index = RELATION_COUNTER.fetch_add(1, Ordering::SeqCst);
    let input_rel_name = format!("InCapture_{}_{}", node_type, index);

    let relation = Relation {
        pos: Pos::nopos(),
        role: RelationRole::RelInput,
        semantics: RelationSemantics::RelSet,
        name: input_rel_name.clone(),
        rtype: DType::TStruct {
            pos: Pos::nopos(),
            name: format!("{}_Struct", node_type),
            fields: vec![],
        },
        primary_key: None,
    };
    program.add_relation(relation);

    if let Some(parent_name) = parent_rel {
        // Create a rule linking parent_name to child input_rel_name
        let lhs_atom = Atom {
            pos: Pos::nopos(),
            relation: input_rel_name.clone(),
            delay: Delay::zero(),
            diff: false,
            value: Expr::string_lit("Child of parent capture"),
        };
        let lhs = RuleLHS {
            pos: Pos::nopos(),
            atom: lhs_atom,
            location: None,
        };
        let rhs_condition = RuleRHS::RHSCondition {
            pos: Pos::nopos(),
            expr: Expr::new(ExprNode::EInterpolated {
                pos: Pos::nopos(),
                value: format!("ParentRelation {}", parent_name),
            }),
        };
        let link_rule = DdlogRule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![lhs],
            rhs: vec![rhs_condition],
        };
        program.add_rule(link_rule);
    }

    input_rel_name
}

fn handle_emit_form(
    node_type: &str,
    attrs: &HashMap<String, Box<Lval>>,
    program: &mut DatalogProgram,
    parent_rel: Option<String>,
) {
    let out_rel_name = create_output_relation_for_emit(node_type, program);

    let rule_id = RULE_COUNTER.fetch_add(1, Ordering::SeqCst);
    let mut fields_vec = Vec::new();
    for (k, v) in attrs {
        match **v {
            Lval::Capture(ref cap_name) => {
                // Suppose we rely on a "Captured_*" relation to map the capture
                let input_relation = guess_relation_for_capture(cap_name);
                let field_expr = Expr::new(ExprNode::EInterpolated {
                    pos: Pos::nopos(),
                    value: format!("Emitting from {}", cap_name),
                });
                fields_vec.push((k.clone(), field_expr, input_relation));
            }
            _ => {
                let field_expr = Expr::new(ExprNode::EString {
                    pos: Pos::nopos(),
                    value: format!("Literal value for {}", k),
                });
                fields_vec.push((k.clone(), field_expr, String::new()));
            }
        }
    }

    let fields_interpolated = Expr::new(ExprNode::EInterpolated {
        pos: Pos::nopos(),
        value: format!("Emit node {}", node_type),
    });

    let lhs_atom = Atom {
        pos: Pos::nopos(),
        relation: out_rel_name.clone(),
        delay: Delay::zero(),
        diff: false,
        value: fields_interpolated,
    };
    let lhs = RuleLHS {
        pos: Pos::nopos(),
        atom: lhs_atom,
        location: None,
    };

    let mut rhses = Vec::new();
    for (_, _expr, input_rel) in fields_vec {
        if !input_rel.is_empty() {
            rhses.push(RuleRHS::RHSCondition {
                pos: Pos::nopos(),
                expr: Expr::new(ExprNode::EInterpolated {
                    pos: Pos::nopos(),
                    value: format!("CapturedRelation {}", input_rel),
                }),
            });
        }
    }
    if let Some(ref parent_name) = parent_rel {
        rhses.push(RuleRHS::RHSCondition {
            pos: Pos::nopos(),
            expr: Expr::new(ExprNode::EInterpolated {
                pos: Pos::nopos(),
                value: format!("ParentRelation {}", parent_name),
            }),
        });
    }
    let ddlog_rule = DdlogRule {
        pos: Pos::nopos(),
        module: ModuleName { path: vec![] },
        lhs: vec![lhs],
        rhs: rhses,
    };
    program.add_rule(ddlog_rule);

    // If there are nested Lval inside the attributes, we traverse them
    for v in attrs.values() {
        traverse_lval(v, program, Some(out_rel_name.clone()));
    }
}

fn create_output_relation_for_emit(node_type: &str, program: &mut DatalogProgram) -> String {
    let rid = RELATION_COUNTER.fetch_add(1, Ordering::SeqCst);
    let out_rel_name = format!("EmitOut_{}_{}", node_type, rid);
    let out_relation = Relation {
        pos: Pos::nopos(),
        role: RelationRole::RelOutput,
        semantics: RelationSemantics::RelSet,
        name: out_rel_name.clone(),
        rtype: DType::TStruct {
            pos: Pos::nopos(),
            name: format!("{}_Struct", node_type),
            fields: vec![],
        },
        primary_key: None,
    };
    program.add_relation(out_relation);
    out_rel_name
}

fn guess_relation_for_capture(_cap_name: &str) -> String {
    // For now just dummy out something
    "CapturedRelation_Inferred".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Lval;

    #[test]
    fn test_single_capture() {
        let mut attrs = HashMap::new();
        attrs.insert(
            "k".to_string(),
            Box::new(Lval::Capture("@x".to_string())),
        );
        let capture_node = Lval::CaptureForm("MyNode".to_string(), attrs, None, None);
        let ast = Lval::Query(vec![Box::new(capture_node)]);
        let prog = transform_ast_to_ddlog(&ast);
        let out = format!("{}", prog);
        assert!(out.contains("output relation Captured_MyNode_k_"));
        assert!(out.contains("Captured Value from @x"));
        assert!(out.contains("FromRelation InCapture_MyNode_"));
    }

    #[test]
    fn test_nested_capture() {
        let mut bottom_attrs = HashMap::new();
        bottom_attrs.insert(
            "bk".to_string(),
            Box::new(Lval::Capture("@bot".to_string())),
        );
        let bottom_cf = Lval::CaptureForm("Bottom".to_string(), bottom_attrs, None, None);
        let mut top_attrs = HashMap::new();
        top_attrs.insert(
            "tk".to_string(),
            Box::new(Lval::Capture("@top".to_string())),
        );
        let top_cf = Lval::CaptureForm("Top".to_string(), top_attrs, Some(Box::new(bottom_cf)), None);
        let ast = Lval::Query(vec![Box::new(top_cf)]);
        let prog = transform_ast_to_ddlog(&ast);
        let out = format!("{}", prog);
               
        assert!(out.contains("output relation Captured_Top_tk_"));
        assert!(out.contains("output relation Captured_Bottom_bk_"));
        assert!(out.contains("Captured Value from @top"));
        assert!(out.contains("Captured Value from @bot"));
        assert!(out.contains("FromRelation InCapture_Bottom_"));
    }

    #[test]
    fn test_capture_and_emit_integration() {
        let input = r#"
            (emit OutNode
                (att1 @capA)
            )
            (capture MyNode
                (k1 @capA)
            )
        "#;

        let ast = to_ast(input).expect("Parse error");
        let ddlog_prog = transform_ast_to_ddlog(&ast);

        // We check that the program has the expected number of relations/rules
        // 1. Input relation for capture MyNode
        // 2. Output relation for captured attribute
        // 3. Rule linking them
        // 4. Output relation for emit OutNode
        // 5. Rule linking captured attribute -> emit
        assert!(!ddlog_prog.relations.is_empty());
        assert!(!ddlog_prog.rules.is_empty());

        // Example of partial check:
        // There's a relation named something like "InCapture_MyNode_1" or "Captured_MyNode_k1_"
        let rel_names: Vec<_> = ddlog_prog.relations.keys().cloned().collect();
        assert!(rel_names.iter().any(|r| r.contains("InCapture_MyNode")));
        assert!(rel_names.iter().any(|r| r.contains("Captured_MyNode_k1_")));
        assert!(rel_names.iter().any(|r| r.contains("EmitOut_OutNode_")));

        let rule_count = ddlog_prog.rules.len();
        assert!(rule_count >= 2);
    }

    #[test]
    fn test_nested_capture_emit() {
        let input = r#"
            (emit FinalOutput
                (fkey @childVal)
            )
            (capture TopNode
                (topKey @topVal)
                (capture ChildNode
                    (childKey @childVal)
                )
            )
       "#;

        let ast = to_ast(input).expect("Parse error");
        let ddlog_prog = transform_ast_to_ddlog(&ast);

        // We expect input relations for the top node and child node,
        // output relations for the captured attributes,
        // plus the final emit relation and rules connecting them.
        let rel_names: Vec<_> = ddlog_prog.relations.keys().cloned().collect();
        assert!(rel_names.iter().any(|r| r.contains("InCapture_TopNode")));
        assert!(rel_names.iter().any(|r| r.contains("InCapture_ChildNode")));
        assert!(rel_names.iter().any(|r| r.contains("Captured_TopNode_topKey_")));
        assert!(rel_names.iter().any(|r| r.contains("Captured_ChildNode_childKey_")));
        assert!(rel_names.iter().any(|r| r.contains("EmitOut_FinalOutput_")));
        let rule_count = ddlog_prog.rules.len();
        assert!(rule_count >= 3);
    }
}
