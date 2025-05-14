/*
    This module is responsible for parsing NatSpec documentation comments
    commonly found in Solidity source code. NatSpec comments provide a
    standardized way to document contracts, functions, parameters, return
    values, and other code elements.

    The primary functionality includes:
    - Defining data structures (`NatSpecKind`, `NatSpecItem`, `NatSpec`)
      to represent the parsed NatSpec information.
    - Implementing `nom` parsers to break down raw comment strings (both
      single-line `///` and multi-line `/** ... */`) into these
      structured types.
    - Handling various NatSpec tags like `@title`, `@author`, `@notice`,
      `@dev`, `@param`, `@return`, `@inheritdoc`, and custom tags
      (`@custom:...`).
    - Providing utility functions on the `NatSpec` struct to query and
      manipulate the parsed documentation, such as populating return item
      names and counting specific tag occurrences.

    The main entry point for parsing is the `parse_natspec_comment` function,
    which takes a raw comment string and attempts to parse it into a
    `NatSpec` struct.
*/
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_until, take_while1},
    character::complete::{
        anychar, char, line_ending, multispace0, not_line_ending, space0, space1,
    },
    combinator::{cut, map, not, opt, peek, recognize},
    error::{context, ParseError},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};
use serde::{Deserialize, Serialize};
use std::ops::Range;

pub mod extract;

#[derive(Default, Copy, Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize)]
pub struct TextIndex {
    pub utf8: usize,
    pub line: usize,
    pub column: usize,
}

pub type TextRange = Range<TextIndex>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub name: Option<String>,
    pub span: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NatSpecKind {
    Title,
    Author,
    Notice,
    Dev,
    Param { name: String },
    Return { name: Option<String> },
    Inheritdoc { parent: String },
    Custom { tag: String },
}

impl NatSpecKind {
    pub fn is_param(&self) -> bool {
        matches!(self, NatSpecKind::Param { .. })
    }
    pub fn is_return(&self) -> bool {
        matches!(self, NatSpecKind::Return { .. })
    }
    pub fn is_notice(&self) -> bool {
        matches!(self, NatSpecKind::Notice)
    }
    pub fn is_dev(&self) -> bool {
        matches!(self, NatSpecKind::Dev)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NatSpecItem {
    pub kind: NatSpecKind,
    pub comment: String,
}

impl NatSpecItem {
    pub fn populate_return(&mut self, returns: &[Identifier]) {
        if !matches!(self.kind, NatSpecKind::Return { name: _ }) {
            return;
        }
        let name = self
            .comment
            .split_whitespace()
            .next()
            .filter(|first_word| {
                returns.iter().any(|r| match &r.name {
                    Some(name) => first_word == name,
                    None => false,
                })
            })
            .map(ToOwned::to_owned);

        if let Some(name_val) = &name {
            if let Some(stripped_comment) = self.comment.strip_prefix(name_val) {
                self.comment = stripped_comment.trim_start().to_string();
            }
        }
        self.kind = NatSpecKind::Return { name };
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.kind == NatSpecKind::Notice && self.comment.is_empty()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct NatSpec {
    pub items: Vec<NatSpecItem>,
}

impl NatSpec {
    pub fn append(&mut self, other: &mut Self) {
        self.items.append(&mut other.items);
    }

    #[must_use]
    pub fn populate_returns(mut self, returns: &[Identifier]) -> Self {
        for i in &mut self.items {
            i.populate_return(returns);
        }
        self
    }

    #[must_use]
    pub fn count_param(&self, ident: &Identifier) -> usize {
        let Some(ident_name) = &ident.name else {
            return 0;
        };
        self.items
            .iter()
            .filter(|n| match &n.kind {
                NatSpecKind::Param { name } => name == ident_name,
                _ => false,
            })
            .count()
    }

    #[must_use]
    pub fn count_return(&self, ident: &Identifier) -> usize {
        let Some(ident_name) = &ident.name else {
            return 0;
        };
        self.items
            .iter()
            .filter(|n| match &n.kind {
                NatSpecKind::Return { name: Some(name) } => name == ident_name,
                _ => false,
            })
            .count()
    }

    #[must_use]
    pub fn count_unnamed_returns(&self) -> usize {
        self.items
            .iter()
            .filter(|n| matches!(&n.kind, NatSpecKind::Return { name: None }))
            .count()
    }

    #[must_use]
    pub fn count_all_returns(&self) -> usize {
        self.items.iter().filter(|n| n.kind.is_return()).count()
    }

    #[must_use]
    pub fn has_param(&self) -> bool {
        self.items.iter().any(|n| n.kind.is_param())
    }

    #[must_use]
    pub fn has_return(&self) -> bool {
        self.items.iter().any(|n| n.kind.is_return())
    }

    #[must_use]
    pub fn has_notice(&self) -> bool {
        self.items.iter().any(|n| n.kind.is_notice())
    }

    #[must_use]
    pub fn has_dev(&self) -> bool {
        self.items.iter().any(|n| n.kind.is_dev())
    }
}

impl From<NatSpecItem> for NatSpec {
    fn from(value: NatSpecItem) -> Self {
        Self { items: vec![value] }
    }
}

fn trim_str(input: &str) -> String {
    input.trim().to_string()
}

fn parse_identifier_str(input: &str) -> IResult<&str, String> {
    let mut parser = map(take_while1(|c: char| !c.is_whitespace()), |s: &str| {
        s.to_string()
    });
    parser.parse(input)
}

fn parse_natspec_kind(input: &str) -> IResult<&str, NatSpecKind> {
    let mut parser = alt((
        map(tag("@title"), |_| NatSpecKind::Title),
        map(tag("@author"), |_| NatSpecKind::Author),
        map(tag("@notice"), |_| NatSpecKind::Notice),
        map(tag("@dev"), |_| NatSpecKind::Dev),
        map(
            preceded(pair(tag("@param"), space1), parse_identifier_str),
            |name| NatSpecKind::Param { name },
        ),
        map(tag("@return"), |_| NatSpecKind::Return { name: None }),
        map(
            preceded(pair(tag("@inheritdoc"), space1), parse_identifier_str),
            |parent| NatSpecKind::Inheritdoc { parent },
        ),
        map(
            preceded(tag("@custom:"), parse_identifier_str),
            |tag_name| NatSpecKind::Custom { tag: tag_name },
        ),
    ));
    parser.parse(input)
}

fn parse_comment_text(input: &str) -> IResult<&str, String> {
    let mut parser = map(not_line_ending, trim_str);
    parser.parse(input)
}

fn parse_multiline_comment_text(input: &str) -> IResult<&str, String> {
    let mut parser = map(
        recognize(many0(preceded(
            not(peek(alt((line_ending, tag("*/"))))),
            anychar,
        ))),
        |s: &str| s.trim().to_string(),
    );
    parser.parse(input)
}

fn parse_one_multiline_natspec_item(input: &str) -> IResult<&str, NatSpecItem> {
    // First check if we're at the closing part of the comment
    if input.trim_start().starts_with("*/") {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Char,
        )));
    }

    let (remaining_input, (lead_space_consumed, star_opt, mid_space_consumed, kind_opt, trail_space_consumed, comment_str)) = tuple((
        space0,
        opt(char('*')),
        space0,
        opt(parse_natspec_kind),
        space0,
        parse_multiline_comment_text,
    ))(input)?;

    let item = NatSpecItem {
        kind: kind_opt.unwrap_or(NatSpecKind::Notice),
        comment: comment_str,
    };

    Ok((remaining_input, item))
}

fn parse_multiline_comment(input: &str) -> IResult<&str, NatSpec> {
    let mut parser = map(
        delimited(
            // Changed multispace0 to space0 after tag("/**").
            // space0 will consume spaces/tabs on the same line as "/**", but not a newline.
            // If there's a newline after "/**", the first parse_one_multiline_natspec_item's
            // leading space0 or the separated_list0's line_ending logic will handle it.
            tuple((tag("/**"), space0)),
            separated_list0(line_ending, parse_one_multiline_natspec_item),
            preceded(multispace0, tag("*/")),
        ),
        |items| {
            // Filter out any completely empty NatSpecItems (Notice with empty comment)
            // that might arise from lines like " * " or the final " */" if not handled by line_ending.
            let filtered_items = items.into_iter().filter(|item| !item.is_empty()).collect();
            NatSpec { items: filtered_items }
        },
    );
    parser.parse(input)
}

fn parse_empty_multiline_comment(input: &str) -> IResult<&str, NatSpec> {
    let mut parser = map(
        recognize(tuple((tag("/**"), space0, many0(char('*')), char('/')))),
        |_| NatSpec::default(),
    );
    parser.parse(input)
}

fn parse_single_line_natspec_item(input: &str) -> IResult<&str, NatSpecItem> {
    let mut parser = map(
        tuple((space0, opt(parse_natspec_kind), space0, parse_comment_text)),
        |(_, kind_opt, _, comment_str)| NatSpecItem {
            kind: kind_opt.unwrap_or(NatSpecKind::Notice),
            comment: comment_str,
        },
    );
    parser.parse(input)
}

fn parse_single_line_comment(input: &str) -> IResult<&str, NatSpec> {
    let mut parser = map(
        preceded(
            tuple((tag("///"), cut(not(char('/'))))),
            parse_single_line_natspec_item,
        ),
        |item| {
            if item.is_empty() {
                NatSpec::default()
            } else {
                NatSpec { items: vec![item] }
            }
        },
    );
    parser.parse(input)
}

fn do_parse_natspec_comment(input: &str) -> IResult<&str, NatSpec> {
    let trimmed_input = input.trim();
    let mut parser = alt((
        parse_single_line_comment,
        parse_multiline_comment,
        parse_empty_multiline_comment,
    ));
    parser.parse(trimmed_input)
}

/// Parses a raw Natspec comment string into a structured `NatSpec` object.
///
/// This function handles both single-line (`///`) and multi-line (`/** ... */`)
/// Natspec comments. It trims the input string before parsing.
///
/// # Arguments
///
/// * `input`: A string slice representing the raw Natspec comment.
///
/// # Returns
///
/// * `anyhow::Result<NatSpec>`: A result containing the parsed `NatSpec` on success,
///   or an `anyhow::Error` if parsing fails.
pub fn parse_natspec_comment(input: &str) -> anyhow::Result<NatSpec> {
    use nom::Finish; // Keep Finish scoped to this function
    match do_parse_natspec_comment(input).finish() {
        Ok((_, natspec)) => Ok(natspec),
        Err(e) => {
            // Use a simpler error message approach that doesn't rely on convert_error
            Err(anyhow::anyhow!(
                "Failed to parse Natspec comment: {}",
                e
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::Finish;

    #[test]
    fn test_parse_identifier_str_parser() {
        assert_eq!(
            parse_identifier_str("foo bar"),
            Ok((" bar", "foo".to_string()))
        );
        assert_eq!(parse_identifier_str("foo"), Ok(("", "foo".to_string())));
    }

    #[test]
    fn test_natspec_kind_parser() {
        assert_eq!(parse_natspec_kind("@title"), Ok(("", NatSpecKind::Title)));
        assert_eq!(parse_natspec_kind("@author"), Ok(("", NatSpecKind::Author)));
        assert_eq!(parse_natspec_kind("@notice"), Ok(("", NatSpecKind::Notice)));
        assert_eq!(parse_natspec_kind("@dev"), Ok(("", NatSpecKind::Dev)));
        assert_eq!(
            parse_natspec_kind("@param foo"),
            Ok((
                "",
                NatSpecKind::Param {
                    name: "foo".to_string()
                }
            ))
        );
        assert_eq!(
            parse_natspec_kind("@return"),
            Ok(("", NatSpecKind::Return { name: None }))
        );
        assert_eq!(
            parse_natspec_kind("@inheritdoc ISome"),
            Ok((
                "",
                NatSpecKind::Inheritdoc {
                    parent: "ISome".to_string()
                }
            ))
        );
        assert_eq!(
            parse_natspec_kind("@custom:tagname"),
            Ok((
                "",
                NatSpecKind::Custom {
                    tag: "tagname".to_string()
                }
            ))
        );
    }

    #[test]
    fn test_one_multiline_item_parser() {
        let cases = [
            ("* @dev Hello world", NatSpecKind::Dev, "Hello world"),
            (" @title The Title", NatSpecKind::Title, "The Title"),
            (
                "* @author McGyver <hi@buildanything.com>",
                NatSpecKind::Author,
                "McGyver <hi@buildanything.com>",
            ),
            (
                " @param foo The bar",
                NatSpecKind::Param {
                    name: "foo".to_string(),
                },
                "The bar",
            ),
            (
                " @return something The return value",
                NatSpecKind::Return { name: None },
                "something The return value",
            ),
            (
                "* @custom:foo bar",
                NatSpecKind::Custom {
                    tag: "foo".to_string(),
                },
                "bar",
            ),
            ("  lorem ipsum", NatSpecKind::Notice, "lorem ipsum"),
            ("lorem ipsum", NatSpecKind::Notice, "lorem ipsum"),
            ("*  foobar", NatSpecKind::Notice, "foobar"),
        ];
        for (input, kind, comment) in cases {
            let res = parse_one_multiline_natspec_item(input).finish();
            assert!(
                res.is_ok(),
                "Failed on input: '{}', Error: {:?}",
                input,
                res.err()
            );
            let (_, item) = res.unwrap();
            assert_eq!(item.kind, kind);
            assert_eq!(item.comment, comment.to_string());
        }
    }

    #[test]
    fn test_single_line_comment_parser() {
        let cases = [
            ("/// Foo bar", NatSpecKind::Notice, "Foo bar"),
            ("///  Baz", NatSpecKind::Notice, "Baz"),
            (
                "/// @notice  Hello world",
                NatSpecKind::Notice,
                "Hello world",
            ),
            (
                "/// @param foo This is bar",
                NatSpecKind::Param {
                    name: "foo".to_string(),
                },
                "This is bar",
            ),
            (
                "/// @return The return value",
                NatSpecKind::Return { name: None },
                "The return value",
            ),
            (
                "/// @custom:foo  This is bar",
                NatSpecKind::Custom {
                    tag: "foo".to_string(),
                },
                "This is bar",
            ),
        ];
        for (input, kind, comment) in cases {
            let res = parse_natspec_comment(input);
            assert!(
                res.is_ok(),
                "Failed on input: '{}', Error: {:?}",
                input,
                res.err()
            );
            let natspec = res.unwrap();
            assert_eq!(natspec.items.len(), 1);
            assert_eq!(natspec.items[0].kind, kind);
            assert_eq!(natspec.items[0].comment, comment.to_string());
        }
    }

    #[test]
    fn test_single_line_empty() {
        let res = parse_natspec_comment("///");
        assert!(res.is_ok(), "{:?}", res.err());
        let natspec = res.unwrap();
        assert_eq!(natspec, NatSpec::default());

        let res = parse_natspec_comment("/// ");
        assert!(res.is_ok(), "{:?}", res.err());
        let natspec = res.unwrap();
        assert_eq!(natspec, NatSpec::default());
    }

    #[test]
    fn test_single_line_invalid_delimiter() {
        let res = parse_natspec_comment("//// Hello");
        assert!(res.is_err());
    }

    #[test]
    fn test_multiline_comment_parser() {
        let comment = "/**\n     * @notice Some notice text.\n     */";
        let res = parse_natspec_comment(comment);
        assert!(res.is_ok(), "{:?}", res.err());
        let natspec = res.unwrap();
        assert_eq!(natspec.items.len(), 1);
        assert_eq!(
            natspec.items[0],
            NatSpecItem {
                kind: NatSpecKind::Notice,
                comment: "Some notice text.".to_string()
            }
        );
    }

    #[test]
    fn test_multiline_two_items() {
        let comment = "/**\n     * @notice Some notice text.\n     * @custom:something\n     */";
        let res = parse_natspec_comment(comment);
        assert!(res.is_ok(), "{:?}", res.err());
        let natspec = res.unwrap();
        assert_eq!(natspec.items.len(), 2);
        assert_eq!(
            natspec.items[0],
            NatSpecItem {
                kind: NatSpecKind::Notice,
                comment: "Some notice text.".to_string()
            }
        );
        assert_eq!(
            natspec.items[1],
            NatSpecItem {
                kind: NatSpecKind::Custom {
                    tag: "something".to_string()
                },
                comment: "".to_string()
            }
        );
    }

    #[test]
    fn test_multiline_mixed_leading_asterisks() {
        let comment = "/** @notice First line.\n  Another line, no asterisk.\n\t* @param p The param\n ** @dev Dev comment */";
        let res = parse_natspec_comment(comment);
        assert!(res.is_ok(), "Input: '{}'\nError: {:?}", comment, res.err());
        let natspec = res.unwrap();

        assert_eq!(natspec.items.len(), 4);
        assert_eq!(
            natspec.items[0],
            NatSpecItem {
                kind: NatSpecKind::Notice,
                comment: "First line.".to_string()
            }
        );
        assert_eq!(
            natspec.items[1],
            NatSpecItem {
                kind: NatSpecKind::Notice,
                comment: "Another line, no asterisk.".to_string()
            }
        );
        assert_eq!(
            natspec.items[2],
            NatSpecItem {
                kind: NatSpecKind::Param {
                    name: "p".to_string()
                },
                comment: "The param".to_string()
            }
        );
        assert_eq!(
            natspec.items[3],
            NatSpecItem {
                kind: NatSpecKind::Dev,
                comment: "Dev comment".to_string()
            }
        );
    }

    #[test]
    fn test_multiline_empty_comment() {
        let comment = "/**\n        */";
        let res = parse_natspec_comment(comment);
        assert!(res.is_ok(), "{:?}", res.err());
        let natspec = res.unwrap();
        assert_eq!(natspec, NatSpec::default());

        let comment = "/** */";
        let res = parse_natspec_comment(comment);
        assert!(res.is_ok(), "{:?}", res.err());
        let natspec = res.unwrap();
        assert_eq!(natspec, NatSpec::default());

        let comment = "/***/";
        let res = parse_natspec_comment(comment);
        assert!(res.is_ok(), "{:?}", res.err());
        let natspec = res.unwrap();
        assert_eq!(natspec, NatSpec::default());
    }

    #[test]
    fn test_multiline_invalid_delimiter() {
        let comment = "/*** @notice Some text\n    ** */";
        let res = parse_natspec_comment(comment);
        assert!(res.is_err(), "Expected error for input: {}", comment);
    }

    #[test]
    fn test_populate_returns_logic() {
        let mut item = NatSpecItem {
            kind: NatSpecKind::Return { name: None },
            comment: "value The value returned".to_string(),
        };
        let identifiers = vec![
            Identifier {
                name: Some("value".to_string()),
                span: TextRange::default(),
            },
            Identifier {
                name: Some("success".to_string()),
                span: TextRange::default(),
            },
        ];
        item.populate_return(&identifiers);
        assert_eq!(
            item.kind,
            NatSpecKind::Return {
                name: Some("value".to_string())
            }
        );
        assert_eq!(item.comment, "The value returned".to_string());

        let mut natspec = NatSpec { items: vec![item] };
        natspec = natspec.populate_returns(&identifiers);
        assert_eq!(
            natspec.items[0].kind,
            NatSpecKind::Return {
                name: Some("value".to_string())
            }
        );
        assert_eq!(natspec.items[0].comment, "The value returned".to_string());
    }

    #[test]
    fn test_populate_returns_no_match() {
        let mut item = NatSpecItem {
            kind: NatSpecKind::Return { name: None },
            comment: "Something else".to_string(),
        };
        let identifiers = vec![Identifier {
            name: Some("value".to_string()),
            span: TextRange::default(),
        }];
        item.populate_return(&identifiers);
        assert_eq!(item.kind, NatSpecKind::Return { name: None });
        assert_eq!(item.comment, "Something else".to_string());
    }
}
