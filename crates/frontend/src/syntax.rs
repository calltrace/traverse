
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum TokenType {
    Keyword,      // emit, capture, when, do
    Function,     // Built-in functions
    Variable,     // @variables
    String,       // "string literals"
    Number,       // numeric literals
    Operator,     // and, or, not
    Delimiter,    // (), {}, []
    Attribute,    // attribute keys in emit/capture
    Comment,      // /* comments */
    Text,         // Other text
}

#[derive(Debug, Clone)]
pub struct SyntaxTheme {
    colors: HashMap<TokenType, String>,
}

impl Default for SyntaxTheme {
    fn default() -> Self {
        let mut colors = HashMap::new();
        colors.insert(TokenType::Keyword, "#569CD6".to_string());    // Blue
        colors.insert(TokenType::Function, "#DCDCAA".to_string());   // Yellow
        colors.insert(TokenType::Variable, "#9CDCFE".to_string());   // Light blue
        colors.insert(TokenType::String, "#CE9178".to_string());     // Orange
        colors.insert(TokenType::Number, "#B5CEA8".to_string());     // Green
        colors.insert(TokenType::Operator, "#C586C0".to_string());   // Purple
        colors.insert(TokenType::Delimiter, "#808080".to_string());  // Gray
        colors.insert(TokenType::Attribute, "#9CDCFE".to_string());  // Light blue
        colors.insert(TokenType::Comment, "#6A9955".to_string());    // Dark green
        colors.insert(TokenType::Text, "#D4D4D4".to_string());       // Light gray
        Self { colors }
    }
}

impl SyntaxTheme {
    pub fn get_color(&self, token_type: &TokenType) -> &str {
        self.colors.get(token_type).map(|s| s.as_str()).unwrap_or("#FFFFFF")
    }

    pub fn set_color(&mut self, token_type: TokenType, color: String) {
        self.colors.insert(token_type, color);
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub text: String,
    pub start: usize,
    pub end: usize,
}

pub struct SyntaxHighlighter {
    theme: SyntaxTheme,
    keywords: Vec<&'static str>,
    operators: Vec<&'static str>,
}

impl Default for SyntaxHighlighter {
    fn default() -> Self {
        Self {
            theme: SyntaxTheme::default(),
            keywords: vec![
                "emit", "capture", "when", "do",
                "rules", "infer", "via", "compute" // Add new keywords
            ],
            operators: vec!["and", "or", "not"],
        }
    }
}

impl SyntaxHighlighter {
    pub fn new(theme: SyntaxTheme) -> Self {
        Self {
            theme,
            ..Default::default()
        }
    }

    pub fn tokenize(&self, input: &str) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut chars = input.char_indices().peekable();

        fn is_variable_char(c: char) -> bool {
            c.is_alphanumeric() || c == '_' || c == '?' // Add support for ?-prefixed variables
        }
        
        while let Some((i, c)) = chars.next() {
            match c {
                // Handle comments
                '/' if chars.peek().map(|&(_, c)| c == '*').unwrap_or(false) => {
                    let mut end = i + 2;
                    while let Some((j, c1)) = chars.next() {
                        if c1 == '*' && chars.peek().map(|&(_, c2)| c2 == '/').unwrap_or(false) {
                            end = j + 2;
                            chars.next();
                            break;
                        }
                    }
                    tokens.push(Token {
                        token_type: TokenType::Comment,
                        text: input[i..end].to_string(),
                        start: i,
                        end,
                    });
                }

                // Handle string literals
                '"' => {
                    let mut end = i + 1;
                    while let Some((j, c)) = chars.next() {
                        end = j + 1;
                        if c == '"' {
                            break;
                        }
                    }
                    tokens.push(Token {
                        token_type: TokenType::String,
                        text: input[i..end].to_string(),
                        start: i,
                        end,
                    });
                }

                // Handle variables starting with @ or ?
                '@' | '?' => {
                    let mut end = i + 1;
                    while let Some(&(j, c)) = chars.peek() {
                        if !is_variable_char(c) {
                            break;
                        }
                        end = j + 1;
                        chars.next();
                    }
                    tokens.push(Token {
                        token_type: TokenType::Variable,
                        text: input[i..end].to_string(),
                        start: i,
                        end,
                    });
                }

                // Handle delimiters
                '(' | ')' | '{' | '}' => {
                    tokens.push(Token {
                        token_type: TokenType::Delimiter,
                        text: c.to_string(),
                        start: i,
                        end: i + 1,
                    });
                }

                // Handle words (keywords, operators, attributes)
                c if c.is_alphabetic() => {
                    let mut end = i + 1;
                    while let Some(&(j, c)) = chars.peek() {
                        if !c.is_alphanumeric() && c != '_' {
                            break;
                        }
                        end = j + 1;
                        chars.next();
                    }
                    let word = &input[i..end];
                    let token_type = if self.keywords.contains(&word) {
                        TokenType::Keyword
                    } else if self.operators.contains(&word) {
                        TokenType::Operator
                    } else {
                        // Check if this word is inside an emit/capture block and could be an attribute
                        TokenType::Text
                    };
                    tokens.push(Token {
                        token_type,
                        text: word.to_string(),
                        start: i,
                        end,
                    });
                }

                // Handle numbers
                c if c.is_numeric() => {
                    let mut end = i + 1;
                    while let Some(&(j, c)) = chars.peek() {
                        if !c.is_numeric() {
                            break;
                        }
                        end = j + 1;
                        chars.next();
                    }
                    tokens.push(Token {
                        token_type: TokenType::Number,
                        text: input[i..end].to_string(),
                        start: i,
                        end,
                    });
                }

                // Skip whitespace
                c if c.is_whitespace() => continue,

                // Handle any other characters as plain text
                _ => {
                    tokens.push(Token {
                        token_type: TokenType::Text,
                        text: c.to_string(),
                        start: i,
                        end: i + 1,
                    });
                }
            }
        }
        tokens
    }

   pub fn highlight(&self, input: &str) -> String {
        let tokens = self.tokenize(input);
        let mut result = String::new();
        let mut last_end = 0;

        for token in tokens {
            // Add any text between tokens
            if token.start > last_end {
                result.push_str(&input[last_end..token.start]);
            }

            // Add the colored token using ANSI escape codes
            let color = self.theme.get_color(&token.token_type);
            let ansi_color = match token.token_type {
                TokenType::Keyword => "\x1b[34m",    // Blue
                TokenType::Function => "\x1b[33m",   // Yellow
                TokenType::Variable => "\x1b[36m",   // Cyan
                TokenType::String => "\x1b[31m",     // Red
                TokenType::Number => "\x1b[32m",     // Green
                TokenType::Operator => "\x1b[35m",   // Magenta
                TokenType::Delimiter => "\x1b[90m",  // Bright black
                TokenType::Comment => "\x1b[32m",    // Green
                _ => "\x1b[37m",                    // White (default)
            };
            result.push_str(&format!("{}{}\x1b[0m", ansi_color, token.text));
            
            last_end = token.end;
        }

        // Add any remaining text
        if last_end < input.len() {
            result.push_str(&input[last_end..]);
        }

        result
    }


}

#[cfg(test)]
mod tests {
    #[test]
    fn test_rules_block_highlighting() {
        let highlighter = SyntaxHighlighter::default();
        let input = "(rules MyRelation\n  (infer rule1 (?x ?y)))";
        let highlighted = highlighter.highlight(input);
        
        // Check keywords are highlighted
        assert!(highlighted.contains("color: #569CD6"));  // rules keyword
        assert!(highlighted.contains("color: #569CD6"));  // infer keyword
        // Check variables are highlighted
        assert!(highlighted.contains("color: #9CDCFE"));  // ?x and ?y variables
        // Check delimiters
        assert!(highlighted.contains("color: #808080"));  // parentheses
    }

    #[test]
    fn test_inference_path_highlighting() {
        let highlighter = SyntaxHighlighter::default();
        let input = "(via (pred1 ?x) (compute ?result {+ 1 2}))";
        let highlighted = highlighter.highlight(input);
        
        // Check keywords
        assert!(highlighted.contains("color: #569CD6"));  // via keyword
        assert!(highlighted.contains("color: #569CD6"));  // compute keyword
        // Check variables
        assert!(highlighted.contains("color: #9CDCFE"));  // ?x and ?result variables
        // Check numbers
        assert!(highlighted.contains("color: #B5CEA8"));  // numeric literals
    }
    use super::*;

    #[test]
    fn test_basic_highlighting() {
        let highlighter = SyntaxHighlighter::default();
        let input = "(emit TestNode (key1 @var1))";
        let highlighted = highlighter.highlight(input);
        
        assert!(highlighted.contains("color: #569CD6"));  // emit keyword
        assert!(highlighted.contains("color: #9CDCFE"));  // @var1 variable
        assert!(highlighted.contains("color: #808080"));  // delimiters
    }

    #[test]
    fn test_comment_highlighting() {
        let highlighter = SyntaxHighlighter::default();
        let input = "/* This is a comment */ (emit TestNode)";
        let highlighted = highlighter.highlight(input);
        
        assert!(highlighted.contains("color: #6A9955"));  // comment
        assert!(highlighted.contains("color: #569CD6"));  // emit keyword
    }

    #[test]
    fn test_string_highlighting() {
        let highlighter = SyntaxHighlighter::default();
        let input = r#"(emit TestNode (key1 "string value"))"#;
        let highlighted = highlighter.highlight(input);
        
        assert!(highlighted.contains("color: #CE9178"));  // string
    }
}
