use indexmap::IndexMap;

use crate::dsl::{Lval, ProvenanceType};
use crate::syntax::{SyntaxHighlighter, SyntaxTheme};

pub struct Formatter {
    indent_level: usize,
    indent_size: usize,
    syntax_highlighter: Option<SyntaxHighlighter>,
}

impl Default for Formatter {
    fn default() -> Self {
        Self {
            indent_level: 0,

            indent_size: 2,
            syntax_highlighter: None,
        }
    }
}

impl Formatter {
    pub fn new(indent_size: usize) -> Self {
        Self {
            indent_level: 0,
            indent_size,
            syntax_highlighter: None,
        }
    }

    pub fn with_syntax_highlighting(mut self, theme: Option<SyntaxTheme>) -> Self {
        self.syntax_highlighter = Some(SyntaxHighlighter::new(theme.unwrap_or_default()));
        self
    }

    pub fn format_with_highlighting(&mut self, expr: &Lval) -> String {
        let formatted = self.format(expr);
        if let Some(highlighter) = &self.syntax_highlighter {
            highlighter.highlight(&formatted)
        } else {
            formatted
        }
    }

    fn indent(&self) -> String {
        " ".repeat(self.indent_level * self.indent_size)
    }

    pub fn format(&mut self, expr: &Lval) -> String {
        match expr {
            Lval::Rulebook(cells) => {
                let formatted = cells
                    .iter()
                    .map(|cell| format!("{}{}", self.indent(), self.format(cell)))
                    .collect::<Vec<_>>()
                    .join("\n");
                formatted
            }
            Lval::Emit(node_type, captures, when_form, do_form) => {
                let mut result = format!("(emit {}", node_type);

                // Format attributes
                if !captures.is_empty() {
                    self.indent_level += 1;
                    result.push('\n');
                    let attrs = format_captures(captures, self.indent());
                    result.push_str(&attrs);
                    self.indent_level -= 1;
                }

                // Format when form if present
                if let Some(when) = when_form {
                    self.indent_level += 1;
                    result.push('\n');
                    result.push_str(&format!("{}{}", self.indent(), self.format(when)));
                    self.indent_level -= 1;
                }

                // Format do form if present
                if let Some(do_expr) = do_form {
                    self.indent_level += 1;
                    result.push('\n');
                    result.push_str(&format!("{}{}", self.indent(), self.format(do_expr)));
                    self.indent_level -= 1;
                }

                if !captures.is_empty() || when_form.is_some() || do_form.is_some() {
                    result.push('\n');
                    result.push_str(&self.indent());
                }
                result.push(')');
                result
            }
            Lval::WhenForm(condition, body) => {
                let mut result = format!("(when {}", self.format(condition));
                if !body.is_empty() {
                    self.indent_level += 1;
                    for expr in body {
                        result.push('\n');
                        result.push_str(&format!("{}{}", self.indent(), self.format(expr)));
                    }
                    self.indent_level -= 1;
                    result.push('\n');
                    result.push_str(&self.indent());
                }
                result.push(')');
                result
            }
            Lval::DoForm(exprs) => {
                let mut result = String::from("(do");
                if !exprs.is_empty() {
                    self.indent_level += 1;
                    for expr in exprs {
                        result.push('\n');
                        result.push_str(&format!("{}{}", self.indent(), self.format(expr)));
                    }
                    self.indent_level -= 1;
                    result.push('\n');
                    result.push_str(&self.indent());
                }
                result.push(')');
                result
            }
            Lval::CaptureForm(
                node_type,
                attributes,
                capture_refs,
                nested_captures,
                when,
                q_expr,
            ) => {
                let mut result = format!("(capture {}", node_type);

                // Format attributes
                if !attributes.is_empty() {
                    self.indent_level += 1;
                    result.push('\n');
                    let attrs = format_attributes(attributes, self.indent());
                    result.push_str(&attrs);
                    self.indent_level -= 1;
                }

                result.push('\n');
                //
                // Format capture refs
                if !capture_refs.is_empty() {
                    self.indent_level += 1;
                    result.push_str(
                        &capture_refs
                            .iter()
                            .map(|ref_name| format!("{}{}", self.indent(), ref_name))
                            .collect::<Vec<_>>()
                            .join(" "),
                    );

                    self.indent_level -= 1;
                }

                // Format nested captures if present
                if let Some(nested) = nested_captures {
                    self.indent_level += 1;
                    result.push('\n');
                    result.push_str(&format!("{}{}", self.indent(), self.format(nested)));
                    self.indent_level -= 1;
                }

                // Format when form if present
                if let Some(when) = when {
                    self.indent_level += 1;
                    result.push('\n');
                    result.push_str(&format!("{}{}", self.indent(), self.format(when)));
                    self.indent_level -= 1;
                }

                // Format q-expression if present
                if let Some(q) = q_expr {
                    self.indent_level += 1;
                    result.push('\n');
                    result.push_str(&format!("{}{}", self.indent(), self.format(q)));
                    self.indent_level -= 1;
                }

                if !attributes.is_empty() || nested_captures.is_some() || q_expr.is_some() {
                    result.push('\n');
                    result.push_str(&self.indent());
                }

                result.push(')');
                result
            }
            Lval::Logical(op, operands) => {
                let mut result = format!("({}", self.format(op));
                if !operands.is_empty() {
                    for operand in operands {
                        result.push(' ');
                        result.push_str(&self.format(operand));
                    }
                }
                result.push(')');
                result
            }
            Lval::Sexpr(cells) => {
                if cells.is_empty() {
                    return "()".to_string();
                }
                let mut result = String::from("(");
                result.push_str(
                    &cells
                        .iter()
                        .map(|cell| self.format(cell))
                        .collect::<Vec<_>>()
                        .join(" "),
                );
                result.push(')');
                result
            }
            Lval::Qexpr(cells) => {
                if cells.is_empty() {
                    return "{}".to_string();
                }
                let mut result = String::from("{");
                result.push_str(
                    &cells
                        .iter()
                        .map(|cell| self.format(cell))
                        .collect::<Vec<_>>()
                        .join(" "),
                );
                result.push('}');
                result
            }
            Lval::Capture(name, provenance) => {
                match provenance {
                    Some(ProvenanceType::Default) => format!("@:{}", name),
                    Some(ProvenanceType::Path) => format!("@:path:{}", name),
                    Some(ProvenanceType::Downstream) => format!("@:downstream:{}", name),
                    Some(ProvenanceType::Upstream) => format!("@:upstream:{}", name),
                    Some(ProvenanceType::Span) => format!("@:span:{}", name),
                    Some(ProvenanceType::Full) => format!("@:full:{}", name),
                    None => format!("@{}", name),
                }
            }
            Lval::PredicateOperator(op) => op.clone(),
            Lval::String(s) => s.to_string(),
            Lval::Sym(s) => s.clone(),
            Lval::Num(n) => n.to_string(),
            Lval::RulesBlock(rules) => {
                let mut result = String::from("(rules");
                self.indent_level += 1;
                result.push_str(&format!("{}", self.indent()));

                if !rules.is_empty() {
                    for rule in rules {
                        result.push('\n');
                        result.push_str(&format!("{}{}", self.indent(), self.format(rule)));
                    }
                }
                self.indent_level -= 1;
                result.push('\n');
                result.push_str(&self.indent());
                result.push(')');
                result
            }
            Lval::Inference(relation, params, paths) => {
                let mut result = String::from("(infer");
                result.push_str(&format!(" {} ({})\n", relation, params.as_ref().map(|r| r.join(", ")).unwrap_or_default()));

                self.indent_level += 1;
                if paths.is_some() && !paths.as_ref().unwrap().is_empty() {
                    for path in paths.as_ref().unwrap() {
                        result.push_str(&format!("{}\n", self.format(path)));
                    }
                }
                self.indent_level -= 1;
                result.push_str(&self.indent());
                result.push(')');
                result
            }
            Lval::InferencePath(predicates, computation) => {
                let mut result = String::new();

                result.push_str(&format!("{}via (", self.indent()));

                self.indent_level += 1;
                if !predicates.is_empty() {
                    // Format the first predicate without additional indentation
                    if let Some(first_pred) = predicates.first() {
                        result.push_str(&format!("{}", self.format(first_pred)));
                        result.push('\n');
                    }
                    
                    // Format the remaining predicates with additional indentation
                    for pred in predicates.iter().skip(1) {
                        result.push_str(&format!("{}  {}", self.indent(), self.format(pred)));
                        result.push('\n');
                    }
                }

                if let Some(comp) = computation {
                    result.push_str(&format!("{}  {}", self.indent(), self.format(comp)));
                    result.push('\n');
                }
                self.indent_level -= 1;
                result.push_str(&self.indent());
                result.push(')');
                result
            }
            Lval::Predicate(identifier, arguments) => {
                format!("({} {})", identifier, arguments.join(", "))
            }
            Lval::Computation(variable, qexpr) => {
                format!("(compute {} {})", variable, self.format(qexpr))
            }
            _ => expr.to_string(),
        }
    }
}

fn format_attributes(attributes: &IndexMap<String, Box<Lval>>, indent: String) -> String {
    let attrs: Vec<_> = attributes.iter().collect();

    attrs
        .into_iter()
        .map(|(key, value)| format!("{}({} {})", indent, key, value))
        .collect::<Vec<_>>()
        .join("\n")
}

fn format_captures(captures: &[Box<Lval>], indent: String) -> String {
    captures
        .iter()
        .map(|capture| {
            if let Lval::KeyVal(cells) = &**capture {
                if cells.len() >= 2 {
                    if let Lval::Sym(key) = &*cells[0] {
                        return format!("{}({} {})", indent, key, cells[1]);
                    }
                }
            }
            format!("{}{}", indent, capture)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use indexmap::IndexMap;

    #[test]
    fn test_rules_inference_formatting() {
        let mut formatter = Formatter::default();

        let rules = vec![
            Lval::inference(
                "rule1",
                Some(vec!["?x".to_string(), "?y".to_string()]),
                Some(vec![Lval::inference_path(
                    vec![
                        Lval::predicate("pred1", vec!["?z".to_string()]),
                        Lval::computation("?result", Box::new(Lval::Qexpr(vec![]))),
                    ],
                    None,
                )]),
            )
        ];

        let rules_block = Lval::RulesBlock(rules);

        let formatted = formatter.format(&rules_block);
        assert!(formatted.contains("(rules"));
        assert!(formatted.contains("(infer rule1 (?x, ?y)"));
        assert!(formatted.contains("(pred1 ?z)"));
        assert!(formatted.contains("(compute ?result {})"));
    }

    #[test]
    fn test_capture_refs_formatting() {
        let mut formatter = Formatter::default();

        let mut attributes = IndexMap::new();
        attributes.insert(
            "attr1".to_string(),
            Box::new(Lval::String("value1".to_string())),
        );

        let capture_refs = vec![
            Box::new(Lval::Capture("ref1".to_string(), None)),
            Box::new(Lval::Capture("ref2".to_string(), None)),
        ];

        let capture = Lval::CaptureForm(
            "NodeType".to_string(), 
            attributes, 
            capture_refs, 
            None, 
            None,
            None,
        );

        let formatted = formatter.format(&capture);
        assert!(formatted.contains("(capture NodeType"));
        assert!(formatted.contains("(attr1 \"value1\")"));
    }

    #[test]
    fn test_basic_formatting() {
        let mut formatter = Formatter::default();

        // Test emit formatting
        let captures = vec![
            Box::new(Lval::KeyVal(vec![
                Box::new(Lval::Sym("key1".to_string())),
                Box::new(Lval::String("value1".to_string())),
            ])),
            Box::new(Lval::KeyVal(vec![
                Box::new(Lval::Sym("key2".to_string())),
                Box::new(Lval::Capture("var".to_string(), None)),
            ])),
        ];

        let emit = Lval::Emit("TestNode".to_string(), captures, None, None);

        let formatted = formatter.format(&emit);
        assert!(formatted.contains("(emit TestNode"));
        assert!(formatted.contains("(key1 \"value1\")"));
        assert!(formatted.contains("(key2 @var)"));
    }

    #[test]
    fn test_syntax_highlighting() {
        let mut formatter = Formatter::default().with_syntax_highlighting(None);

        // Test emit formatting
        let captures = vec![
            Box::new(Lval::KeyVal(vec![
                Box::new(Lval::Sym("key1".to_string())),
                Box::new(Lval::String("value1".to_string())),
            ])),
            Box::new(Lval::KeyVal(vec![
                Box::new(Lval::Sym("key2".to_string())),
                Box::new(Lval::Capture("var".to_string(), None)),
            ])),
        ];

        let emit = Lval::Emit("TestNode".to_string(), captures, None, None);

        let highlighted = formatter.format_with_highlighting(&emit);

        // Check that keywords are highlighted
        assert!(highlighted.contains("color: #569CD6")); // emit keyword
        // Check that variables are highlighted
        assert!(highlighted.contains("color: #9CDCFE")); // @var variable
        // Check that strings are highlighted
        assert!(highlighted.contains("color: #CE9178")); // string literal
    }

    #[test]
    fn test_nested_formatting() {
        let mut formatter = Formatter::default();

        // Create a when form with nested emit
        let op = Lval::predicate_operator("and");
        let operands = vec![
            Box::new(Lval::Capture("var1".to_string(), None)),
            Box::new(Lval::Capture("var2".to_string(), None)),
        ];
        let condition = Lval::Logical(op, operands);

        let inner_captures = vec![
            Box::new(Lval::KeyVal(vec![
                Box::new(Lval::Sym("inner".to_string())),
                Box::new(Lval::Capture("var1".to_string(), None)),
            ])),
        ];

        let when_body = vec![
            Box::new(Lval::Emit(
                "InnerNode".to_string(),
                inner_captures,
                None,
                None,
            ))
        ];

        let when_form = Lval::WhenForm(Box::new(condition), when_body);

        let formatted = formatter.format(&when_form);
        assert!(formatted.contains("(when (and @var1 @var2)"));
        assert!(formatted.contains("(emit InnerNode"));
        assert!(formatted.contains("(inner @var1)"));
    }
}
