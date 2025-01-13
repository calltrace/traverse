use crate::ddlog_lang::{
    Atom, DType, Delay, Expr, ModuleName, Pos, Relation, RelationRole, RelationSemantics, Rule,
    RuleLHS, RuleRHS,
};
use crate::ir::IRGraph;
use crate::{ast::Lval, ddlog_lang::DatalogProgram};
use std::collections::{HashMap, HashSet, VecDeque};

pub struct Compiler {
    pub(crate) ir: IRGraph,
    env: HashMap<String, u64>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            ir: IRGraph::new(),
            env: HashMap::new(),
        }
    }

    pub fn compile_lval(&mut self, lval: &Lval) -> u64 {
        match lval {
            Lval::CaptureForm(node_type, attributes, nested_captures, q_expr) => self
                .compile_capture_form(
                    node_type,
                    attributes,
                    nested_captures.as_deref(),
                    q_expr.as_deref(),
                ),
            Lval::Emit(node_type, attributes) => self.compile_emit(node_type, attributes),
            Lval::Capture(name) => {
                if let Some(&id) = self.env.get(name) {
                    id
                } else {
                    let new_id = self.ir.add_rhs_node(
                        &format!("bare_capture_{}", name),
                        HashSet::new(),
                        vec![],
                    );
                    self.env.insert(name.clone(), new_id);
                    new_id
                }
            }
            _ => self.compile_other(lval),
        }
    }

    fn compile_capture_form(
        &mut self,
        node_type: &str,
        attributes: &HashMap<String, Box<Lval>>,
        nested_captures: Option<&Lval>,
        q_expr: Option<&Lval>,
    ) -> u64 {
        let capture_name = format!("capture_{}", node_type);

        let mut referenced_vars = HashSet::new();
        for key in attributes.keys() {
            referenced_vars.insert(key.clone());
        }
        let lhs_id = self.ir.add_lhs_node(&capture_name, referenced_vars);

        let mut parents = Vec::new();
        if let Some(nested) = nested_captures {
            let nested_id = self.compile_lval(nested);
            parents.push(nested_id);
        }

        let mut rhs_vars = HashSet::new();
        for key in attributes.keys() {
            rhs_vars.insert(key.to_owned());
        }

        let capture_rhs_id = self.ir.add_rhs_node(&capture_name, rhs_vars, parents);

        let expr_string = format!("{}", q_expr.map(|q| q.to_string()).unwrap_or_default());
        self.ir.add_rule(lhs_id, capture_rhs_id, &expr_string);

        self.env.insert(node_type.to_string(), capture_rhs_id);

        if let Some(q) = q_expr {
            let _ = self.compile_lval(q);
        }

        capture_rhs_id
    }

    fn compile_emit(&mut self, node_type: &str, attributes: &HashMap<String, Box<Lval>>) -> u64 {
        let emit_name = format!("emit_{}", node_type);

        let mut referenced_vars = HashSet::new();
        for key in attributes.keys() {
            referenced_vars.insert(key.clone());
        }
        let lhs_id = self.ir.add_lhs_node(&emit_name, referenced_vars);

        let mut parents = Vec::new();
        let mut rhs_vars = HashSet::new();
        for (key, val) in attributes {
            rhs_vars.insert(key.clone());
            let child_id = self.compile_lval(val);
            parents.push(child_id);
        }

        let emit_rhs_id = self.ir.add_rhs_node(&emit_name, rhs_vars, parents);

        let expr_string = format!("{}", Lval::Emit(node_type.to_string(), attributes.clone()));
        self.ir.add_rule(lhs_id, emit_rhs_id, &expr_string);

        emit_rhs_id
    }

    fn compile_other(&mut self, lval: &Lval) -> u64 {
        let auto_name = format!("node_{}", self.ir.next_id());
        let rhs_vars = HashSet::new();

        let node_id = self.ir.add_rhs_node(&auto_name, rhs_vars, vec![]);

        let lhs_id = self
            .ir
            .add_lhs_node(&format!("lhs_{}", auto_name), HashSet::new());
        self.ir.add_rule(lhs_id, node_id, &format!("{}", lval));

        node_id
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl From<IRGraph> for DatalogProgram {
    fn from(ir: IRGraph) -> Self {
        let mut program = DatalogProgram::new();

        let mut all_relations = HashSet::new();
        for lhs in ir.lhs_nodes.values() {
            all_relations.insert(lhs.relation_name.clone());
        }
        for rhs in ir.rhs_nodes.values() {
            all_relations.insert(rhs.relation_name.clone());
        }

        for rel_name in all_relations {
            let relation = Relation {
                pos: Pos::nopos(),
                role: RelationRole::RelInternal,
                semantics: RelationSemantics::RelSet,
                name: rel_name.clone(),
                rtype: DType::TString { pos: Pos::nopos() },
                primary_key: None,
            };
            program.add_relation(relation);
        }

        for rule_node in ir.rules.values() {
            let lhs_atom = Atom {
                pos: Pos::nopos(),
                relation: rule_node.lhs.relation_name.clone(),
                delay: Delay::zero(),
                diff: false,
                value: Expr::interpolated(&rule_node.expr),
            };

            let mut rhs_clauses = vec![];

            rhs_clauses.push(RuleRHS::RHSLiteral {
                pos: Pos::nopos(),
                polarity: true,
                atom: Atom {
                    pos: Pos::nopos(),
                    relation: rule_node.rhs.relation_name.clone(),
                    delay: Delay::zero(),
                    diff: false,
                    value: Expr::interpolated(&format!(
                        "RHS(id: {}, vars: {:?})",
                        rule_node.rhs.id, rule_node.rhs.variables
                    )),
                },
            });

            // Recursive BFS to include all ancestor relationships
            let mut visited = HashSet::new();
            let mut queue = VecDeque::new();
            queue.push_back(rule_node.rhs.id);

            while let Some(current_id) = queue.pop_front() {
                if visited.contains(&current_id) {
                    continue;
                }
                visited.insert(current_id);

                if let Some(node) = ir.rhs_nodes.get(&current_id) {
                    for &parent_id in &node.parents {
                        if let Some(parent_node) = ir.rhs_nodes.get(&parent_id) {
                            rhs_clauses.push(RuleRHS::RHSLiteral {
                                pos: Pos::nopos(),
                                polarity: true,
                                atom: Atom {
                                    pos: Pos::nopos(),
                                    relation: parent_node.relation_name.clone(),
                                    delay: Delay::zero(),
                                    diff: false,
                                    value: Expr::interpolated(&format!(
                                        "Parent(id: {}, vars: {:?})",
                                        parent_node.id, parent_node.variables
                                    )),
                                },
                            });
                            queue.push_back(parent_id);
                        }
                    }
                }
            }

            let rule = Rule {
                pos: Pos::nopos(),
                module: ModuleName { path: vec![] },
                lhs: vec![RuleLHS {
                    pos: Pos::nopos(),
                    atom: lhs_atom,
                    location: None,
                }],
                rhs: rhs_clauses,
            };

            program.add_rule(rule);
        }

        program
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_rhs_node() {
        let mut ir_graph = IRGraph::new();
        let id = ir_graph.add_rhs_node(
            "parent",
            HashSet::from(["X".to_string(), "Z".to_string()]),
            vec![],
        );

        let node = ir_graph.rhs_nodes.get(&id).unwrap();
        assert_eq!(node.relation_name, "parent");
        assert!(node.variables.contains("X"));
        assert!(node.variables.contains("Z"));
    }

    #[test]
    fn test_add_lhs_node() {
        let mut ir_graph = IRGraph::new();
        let id = ir_graph.add_lhs_node(
            "relation",
            HashSet::from(["X".to_string(), "Y".to_string()]),
        );

        let node = ir_graph.lhs_nodes.get(&id).unwrap();
        assert_eq!(node.relation_name, "relation");
        assert!(node.referenced_variables.contains("X"));
        assert!(node.referenced_variables.contains("Y"));
    }

    #[test]
    fn test_add_rule() {
        let mut ir_graph = IRGraph::new();
        let rhs_id = ir_graph.add_rhs_node(
            "parent",
            HashSet::from(["X".to_string(), "Z".to_string()]),
            vec![],
        );
        let lhs_id = ir_graph.add_lhs_node(
            "relation",
            HashSet::from(["X".to_string(), "Y".to_string()]),
        );

        let rule_id = ir_graph.add_rule(lhs_id, rhs_id, "relation(X, Y) :- parent(X, Z).");
        let rule = ir_graph.rules.get(&rule_id).unwrap();

        assert_eq!(rule.expr, "relation(X, Y) :- parent(X, Z).");
        assert_eq!(rule.lhs.relation_name, "relation");
        assert_eq!(rule.rhs.relation_name, "parent");
    }

    #[test]
    fn test_traverse_from_rhs() {
        let mut ir_graph = IRGraph::new();
        let root_id = ir_graph.add_rhs_node("root", HashSet::new(), vec![]);
        let child_id = ir_graph.add_rhs_node("child", HashSet::new(), vec![root_id]);

        let mut visited = Vec::new();
        ir_graph.traverse_from_rhs(root_id, |node| {
            visited.push(node.relation_name.clone());
        });

        assert_eq!(visited, vec!["root".to_string(), "child".to_string()]);
    }

    #[test]
    fn test_lval_capture() {
        let cap = Lval::capture("foo");
        match cap.as_ref() {
            Lval::Capture(name) => assert_eq!(name, "foo"),
            _ => panic!("Expected a Capture variant"),
        }
    }

    #[test]
    fn test_lval_emit() {
        let mut attrs = HashMap::new();
        attrs.insert("bar".to_string(), Lval::num(99));
        let em = Lval::emit("testNode", attrs);
        match em.as_ref() {
            Lval::Emit(node_type, map) => {
                assert_eq!(node_type, "testNode");
                assert_eq!(map.len(), 1);
            }
            _ => panic!("Expected an Emit variant"),
        }
    }

    #[test]
    fn test_compiler_capture() {
        let mut compiler = Compiler::new();

        let mut attributes = HashMap::new();
        attributes.insert("foo".to_string(), Lval::num(42));

        let form = Lval::capture_form("mycap", attributes, None, None);

        let rhs_id = compiler.compile_lval(&form);
        assert!(compiler.ir.rhs_nodes.contains_key(&rhs_id));

        let stored_id = compiler.env.get("mycap").unwrap();
        assert_eq!(*stored_id, rhs_id);
    }

    #[test]
    fn test_compiler_emit() {
        let mut compiler = Compiler::new();

        let mut attributes = HashMap::new();
        attributes.insert("bar".to_string(), Lval::num(99));

        let form = Lval::emit("myemit", attributes);

        let rhs_id = compiler.compile_lval(&form);
        assert!(compiler.ir.rhs_nodes.contains_key(&rhs_id));
    }

    #[test]
    fn test_compiler_capture_then_emit() {
        let mut compiler = Compiler::new();

        let mut attrs_cap = HashMap::new();
        attrs_cap.insert("foo".to_string(), Lval::num(42));
        let cap_form = Lval::capture_form("capA", attrs_cap, None, None);

        let cap_id = compiler.compile_lval(&cap_form);

        let mut attrs_emit = HashMap::new();
        attrs_emit.insert("myRef".to_string(), Lval::capture("capA"));
        let emit_form = Lval::emit("someEmit", attrs_emit);

        let emit_id = compiler.compile_lval(&emit_form);

        let emit_node = compiler.ir.rhs_nodes.get(&emit_id).unwrap();
        assert_eq!(emit_node.parents.len(), 1);
        assert_eq!(emit_node.parents[0], cap_id);
    }

    #[test]
    fn test_nested_capture() {
        let mut compiler = Compiler::new();

        let mut outer_attrs = HashMap::new();
        outer_attrs.insert("x".to_string(), Lval::num(10));

        let mut inner_attrs = HashMap::new();
        inner_attrs.insert("y".to_string(), Lval::num(99));
        let inner_capture = Lval::capture_form("inner", inner_attrs, None, None);

        let outer_capture = Lval::capture_form("outer", outer_attrs, Some(inner_capture), None);

        let outer_id = compiler.compile_lval(&outer_capture);
        assert!(compiler.ir.rhs_nodes.contains_key(&outer_id));

        let inner_id = compiler.env.get("inner").expect("Inner capture not stored");

        let mut expected = vec![*inner_id];

        let outer_node = compiler.ir.rhs_nodes.get(&outer_id).unwrap();
        assert_eq!(outer_node.parents.len(), 1);
        assert_eq!(outer_node.parents[0], *inner_id);

        let mut emit_attrs = HashMap::new();
        emit_attrs.insert("a".to_string(), Lval::capture("outer"));
        emit_attrs.insert("b".to_string(), Lval::capture("inner"));

        let emit_form = Lval::emit("myEmit", emit_attrs);
        let emit_id = compiler.compile_lval(&emit_form);

        let emit_node = compiler.ir.rhs_nodes.get(&emit_id).unwrap();
        assert_eq!(emit_node.parents.len(), 2);

        let mut parent_set = emit_node.parents.clone();
        parent_set.sort();
        expected.push(outer_id);
        expected.sort();
        assert_eq!(parent_set, expected);
    }

    #[test]
    fn test_irgraph_to_datalog_program_basic() {
        let mut ir = IRGraph::new();

        let lhs_vars = HashSet::from(["X".to_string()]);
        let lhs_id = ir.add_lhs_node("LHSRel", lhs_vars);

        let rhs_vars = HashSet::from(["A".to_string()]);
        let rhs_id = ir.add_rhs_node("RHSRel", rhs_vars, vec![]);

        ir.add_rule(lhs_id, rhs_id, "LHSRel(X) :- RHSRel(A).");

        println!("{:?}", ir);

        let program: DatalogProgram = ir.into();

        assert!(program.relations.contains_key("LHSRel"));
        assert!(program.relations.contains_key("RHSRel"));

        assert_eq!(program.rules.len(), 1);
        let rule = &program.rules[0];
        assert_eq!(rule.lhs.len(), 1);
        assert_eq!(rule.lhs[0].atom.relation, "LHSRel");
        println!("{:?}", rule.rhs);
        assert!(rule.rhs.iter().any(
            |rhs| matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "RHSRel") 
        ));
    }

    #[test]
    fn test_irgraph_to_datalog_program_with_parents() {
        let mut ir = IRGraph::new();

        let parent1_id = ir.add_rhs_node("ParentRel1", HashSet::new(), vec![]);
        let parent2_id = ir.add_rhs_node("ParentRel2", HashSet::new(), vec![]);

        let child_rhs_id =
            ir.add_rhs_node("ChildRel", HashSet::new(), vec![parent1_id, parent2_id]);

        let lhs_vars = HashSet::from(["Y".to_string()]);
        let lhs_id = ir.add_lhs_node("LHSRelWithParents", lhs_vars);

        ir.add_rule(
            lhs_id,
            child_rhs_id,
            "LHSRelWithParents(Y) :- ChildRel(...).",
        );

        let program: DatalogProgram = ir.into();

        assert!(program.relations.contains_key("LHSRelWithParents"));
        assert!(program.relations.contains_key("ParentRel1"));
        assert!(program.relations.contains_key("ParentRel2"));
        assert!(program.relations.contains_key("ChildRel"));

        assert_eq!(program.rules.len(), 1);
        let rule = &program.rules[0];
        assert_eq!(rule.lhs.len(), 1);
        assert_eq!(rule.lhs[0].atom.relation, "LHSRelWithParents");

        assert!(rule.rhs.iter().any(|rhs| {
            matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "ParentRel1")
        }));
        assert!(rule.rhs.iter().any(|rhs| {
            matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "ParentRel2")
        }));
    }

    #[test]
    fn test_irgraph_to_datalog_program_complex_traversal() {
        let mut ir = IRGraph::new();

        let root_id = ir.add_rhs_node("RootRel", HashSet::new(), vec![]);
        let child1_id = ir.add_rhs_node("ChildRel1", HashSet::new(), vec![root_id]);
        let child2_id = ir.add_rhs_node("ChildRel2", HashSet::new(), vec![root_id]);
        let grandchild_id = ir.add_rhs_node("GrandChildRel", HashSet::new(), vec![child1_id]);

        let lhs_id = ir.add_lhs_node("LHSRelComplex", HashSet::new());

        ir.add_rule(
            lhs_id,
            grandchild_id,
            "LHSRelComplex(...) :- GrandChildRel(...).",
        );

        let program: DatalogProgram = ir.into();

        assert!(program.relations.contains_key("RootRel"));
        assert!(program.relations.contains_key("ChildRel1"));
        assert!(program.relations.contains_key("ChildRel2"));
        assert!(program.relations.contains_key("GrandChildRel"));

        assert_eq!(program.rules.len(), 1);
        let rule = &program.rules[0];
        assert_eq!(rule.lhs.len(), 1);
        assert_eq!(rule.lhs[0].atom.relation, "LHSRelComplex");

        assert!(rule.rhs.iter().any(|rhs| {
            matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "ChildRel1")
        }));
        assert!(rule.rhs.iter().any(|rhs| {
            matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "RootRel")
        }));
    }

    #[test]
    fn test_irgraph_to_datalog_program_with_rhs() {
        let mut ir = IRGraph::new();

        let lhs_vars = HashSet::from(["X".to_string()]);
        let lhs_id = ir.add_lhs_node("LHSRel", lhs_vars);

        let rhs_vars = HashSet::from(["A".to_string()]);
        let rhs_id = ir.add_rhs_node("RHSRel", rhs_vars.clone(), vec![]);

        ir.add_rule(lhs_id, rhs_id, "LHSRel(X) :- RHSRel(A).");

        let program: DatalogProgram = ir.into();

        assert!(program.relations.contains_key("LHSRel"));
        assert!(program.relations.contains_key("RHSRel"));

        assert_eq!(program.rules.len(), 1);
        let rule = &program.rules[0];
        assert_eq!(rule.lhs.len(), 1);
        assert_eq!(rule.lhs[0].atom.relation, "LHSRel");

        assert!(rule.rhs.iter().any(|rhs| {
            matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "RHSRel" && atom.value.to_string().contains("A"))
        }));
    }

    #[test]
    fn test_irgraph_to_datalog_program_with_parents_and_rhs() {
        let mut ir = IRGraph::new();

        let parent_id = ir.add_rhs_node("ParentRel", HashSet::new(), vec![]);

        let child_rhs_id = ir.add_rhs_node("ChildRel", HashSet::new(), vec![parent_id]);

        let lhs_vars = HashSet::from(["Y".to_string()]);
        let lhs_id = ir.add_lhs_node("LHSRelWithParents", lhs_vars);

        ir.add_rule(lhs_id, child_rhs_id, "LHSRelWithParents(Y) :- ChildRel(...).");

        let program: DatalogProgram = ir.into();

        assert!(program.relations.contains_key("LHSRelWithParents"));
        assert!(program.relations.contains_key("ParentRel"));
        assert!(program.relations.contains_key("ChildRel"));

        assert_eq!(program.rules.len(), 1);
        let rule = &program.rules[0];
        assert_eq!(rule.lhs.len(), 1);
        assert_eq!(rule.lhs[0].atom.relation, "LHSRelWithParents");

        assert!(rule.rhs.iter().any(|rhs| {
            matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "ChildRel")
        }));
        assert!(rule.rhs.iter().any(|rhs| {
            matches!(rhs, RuleRHS::RHSLiteral { atom, .. } if atom.relation == "ParentRel")
        }));
    }
}
