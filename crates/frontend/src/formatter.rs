use crate::dsl::Lval;
use crate::syntax::{SyntaxHighlighter, SyntaxTheme};
use std::collections::HashMap;

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
            Lval::Query(cells) => {
                self.indent_level += 1;
                let formatted = cells
                    .iter()
                    .map(|cell| format!("{}{}", self.indent(), self.format(cell)))
                    .collect::<Vec<_>>()
                    .join("\n");
                self.indent_level -= 1;
                formatted
            }
            Lval::Emit(node_type, attributes, when_form, do_form) => {
                let mut result = format!("(emit {}", node_type);

                // Format attributes
                if !attributes.is_empty() {
                    self.indent_level += 1;
                    result.push('\n');
                    let attrs = format_attributes(attributes, self.indent());
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

                if !attributes.is_empty() || when_form.is_some() || do_form.is_some() {
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
            Lval::CaptureForm(node_type, attributes, nested_captures, q_expr) => {
                let mut result = format!("(capture {}", node_type);

                // Format attributes
                if !attributes.is_empty() {
                    self.indent_level += 1;
                    result.push('\n');
                    let attrs = format_attributes(attributes, self.indent());
                    result.push_str(&attrs);
                    self.indent_level -= 1;
                }

                // Format nested captures if present
                if let Some(nested) = nested_captures {
                    self.indent_level += 1;
                    result.push('\n');
                    result.push_str(&format!("{}{}", self.indent(), self.format(nested)));
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
            Lval::Capture(name) => name.to_string(),
            Lval::PredicateOperator(op) => op.clone(),
            Lval::String(s) => s.to_string(),
            Lval::Sym(s) => s.clone(),
            Lval::Num(n) => n.to_string(),
            _ => expr.to_string(),
        }
    }
}

fn format_attributes(attributes: &HashMap<String, Box<Lval>>, indent: String) -> String {
    let mut attrs: Vec<_> = attributes.iter().collect();
    attrs.sort_by(|(a, _), (b, _)| a.cmp(b));

    attrs
        .into_iter()
        .map(|(key, value)| format!("{}({} {})", indent, key, value))
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_basic_formatting() {
        let mut formatter = Formatter::default();

        // Test emit formatting
        let mut attributes = HashMap::new();
        attributes.insert("key1".to_string(), Lval::string_literal("value1"));
        attributes.insert("key2".to_string(), Lval::capture("var"));

        let emit = Lval::Emit("TestNode".to_string(), attributes, None, None);

        let formatted = formatter.format(&emit);
        assert_eq!(
            formatted,
            "(emit TestNode\n  (key1 \"value1\")\n  (key2 @var)\n)"
        );
    }

    #[test]
    fn test_syntax_highlighting() {
        let mut formatter = Formatter::default().with_syntax_highlighting(None);

        let mut attributes = HashMap::new();
        attributes.insert("key1".to_string(), Lval::string_literal("value1"));
        attributes.insert("key2".to_string(), Lval::capture("var"));

        let emit = Lval::Emit("TestNode".to_string(), attributes, None, None);

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
        let condition = Lval::logical(
            Lval::predicate_operator("and"),
            vec![Lval::capture("var1"), Lval::capture("var2")],
        );

        let mut inner_attrs = HashMap::new();
        inner_attrs.insert("inner".to_string(), Lval::capture("var1"));

        let when_body = vec![Lval::emit("InnerNode", inner_attrs, None, None)];

        let when_form = Lval::WhenForm(Box::new(*condition), when_body);

        let formatted = formatter.format(&when_form);
        assert!(formatted.contains("(when (and @var1 @var2)"));
        assert!(formatted.contains("  (emit InnerNode"));
        assert!(formatted.contains("    (inner @var1)"));
    }
}
