use crate::ast::Lval;
use crate::ir::IRGraph;
use std::collections::{HashMap, HashSet};

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
            Lval::CaptureForm(node_type, attributes, nested_captures, q_expr) => {
                self.compile_capture_form(node_type, attributes, nested_captures.as_deref(), q_expr.as_deref())
            }
            Lval::Emit(node_type, attributes) => {
                self.compile_emit(node_type, attributes)
            }
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

    fn compile_emit(
        &mut self,
        node_type: &str,
        attributes: &HashMap<String, Box<Lval>>,
    ) -> u64 {
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

        let lhs_id = self.ir.add_lhs_node(&format!("lhs_{}", auto_name), HashSet::new());
        self.ir.add_rule(lhs_id, node_id, &format!("{}", lval));

        node_id
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
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
}
