use crate::lexer::*;
use anyhow::*;
use im::Vector;
use itertools::Itertools;

#[derive(Debug, Clone)]
struct Line1 {
    tokens: Vector<Token>,
    line_number: usize,
}

#[derive(Debug, Clone)]
pub struct Line2<T>
where
    T: Clone,
{
    pub tokens: Vector<T>,
    pub indent: usize,
    pub line_number: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BracketedToken<T>
where
    T: Clone,
{
    SingleToken(T),
    Bracketed(BracketType, Vector<Self>),
}

fn split_lines(tokens: impl Iterator<Item = Token>) -> Result<Vector<Line1>> {

    let tokens = tokens.collect::<Vector<_>>();
    dbg!(tokens.clone());


    let mut lines = Vector::new();
    let mut current_line = Line1 {
        tokens: Vector::new(),
        line_number: 0,
    };
    let mut brackets = Vector::new();
    let mut newline_ignored = false;

    for token in tokens {
        match token {
            Token::Newline => {
                current_line.line_number += 1;
                if current_line.tokens.is_empty() {
                    current_line.tokens = Vector::new();
                } else if !newline_ignored && brackets.is_empty() {
                    lines.push_back(current_line.clone());
                    current_line.tokens = Vector::new();
                }
                continue;
            }
            Token::Whitespace(ref s) if s.contains('\\') => {
                newline_ignored = true;
            }
            Token::OpenBracket(b) => {
                brackets.push_back(b);
            }
            Token::CloseBracket(b) => {
                let last_bracket = brackets.pop_back();
                ensure!(last_bracket == Some(b), "unexpected close bracket")
            }
            _ => newline_ignored = false,
        }
        current_line.tokens.push_back(token);
    }

    if !brackets.is_empty() {
        bail!("unclosed bracket");
    }
    lines.push_back(current_line);
    Ok(lines)
}



fn get_indent(indent_type: &str, mut indent: &str) -> Result<usize> {
    let mut count = 0;
    while indent.starts_with(indent_type) {
        indent = &indent[indent_type.len()..];
        count += 1;
    }
    if !indent.is_empty() {
        bail!("indentation type mismatch");
    }
    Ok(count)
}

fn is_line_empty(line: &Line1) -> bool {
    fn is_empty(token: &Token) -> bool {
        match token {
            Token::Newline | Token::Whitespace(_) | Token::Comment(_) => true,
            _ => false,
        }
    }
    line.tokens.is_empty() || line.tokens.iter().all(is_empty)
}

pub fn convert_tokens_to_lines(tokens: impl Iterator<Item = Token>) -> Result<Vector<Line2<BracketedToken<Token>>>> {
    let lines = split_lines(tokens)?;
    let lines = lines.into_iter().filter(|l| !is_line_empty(l));

    let mut line_converter = LineConverter {
        indent_type: None,
        current_indent: 0,
        should_start_new_block: false,
    };
    lines
        .into_iter()
        .map(|a| line_converter.convert_line(a.clone()).with_context(|| anyhow!("on line {}", a.line_number)))
        .try_collect()
}

struct LineConverter {
    indent_type: Option<String>,
    current_indent: usize,
    should_start_new_block: bool,
}

use crate::lexer4::BLOCK_IDENTIFERS;

fn should_start_new_block(tokens: &Vector<Token>) -> bool {
    let ident_pos = tokens
        .iter()
        .position(|t| matches!(t, Token::Identifier(_)))
        .unwrap_or(0);
    let identifer = match tokens.get(ident_pos) {
        Some(Token::Identifier(s)) => s,
        _ => return false,
    };
    BLOCK_IDENTIFERS.contains(&identifer.as_str())
}

impl LineConverter {
    fn convert_line(&mut self, line: Line1) -> Result<Line2<BracketedToken<Token>>> {
        let Line1 {
            tokens,
            line_number,
        } = line;
        let first_token = &tokens[0];

        let indent_level = match first_token {
            Token::Whitespace(s) => {
                if self.indent_type.is_none() {
                    self.indent_type = Some(s.to_string());
                }
                let indent_type = self.indent_type.as_ref().unwrap();
                get_indent(indent_type, s)?
            }
            _ => 0,
        };

        if self.should_start_new_block {
            ensure!(
                indent_level == self.current_indent + 1,
                "expected indentation. indent_level: {}, current_indent: {}",
                indent_level,
                self.current_indent,
            );
        } else {
            ensure!(
                indent_level <= self.current_indent,
                "expected no indentation. indent_level: {}, current_indent: {}",
                indent_level,
                self.current_indent,
            );
        }
        
        

        self.should_start_new_block = should_start_new_block(&tokens);
        self.current_indent = indent_level;

        let result = Line2 {
            tokens: to_brackets(tokens)?,
            indent: indent_level,
            line_number,
        };
        Ok(result)
    }
}

fn to_brackets(tokens: Vector<Token>) -> Result<Vector<BracketedToken<Token>>> {
    use BracketedToken::*;
    use Token::*;
    let mut tokens: Vector<_> = tokens.into_iter().map(|t| SingleToken(t)).collect();

    fn is_open_bracket(t: &BracketedToken<Token>) -> bool {
        matches!(t, SingleToken(OpenBracket(_)))
    }

    while let Some(start_bracket) = tokens.iter().rposition(is_open_bracket) {
        let bracket_type = match &tokens[start_bracket] {
            SingleToken(OpenBracket(b)) => *b,
            _ => unreachable!(),
        };

        let end_bracket = tokens
            .iter()
            .skip(start_bracket + 1)
            .position(|t| matches!(t, SingleToken(CloseBracket(b)) if *b == bracket_type))
            .map(|i| start_bracket + i + 1)
            .ok_or(anyhow!("unclosed bracket"))?;

        let mut bracketed_tokens = tokens.slice(start_bracket..=end_bracket);

        bracketed_tokens.pop_back();
        bracketed_tokens.pop_front();

        tokens.insert(start_bracket, Bracketed(bracket_type, bracketed_tokens));
    }

    Ok(tokens)
}

#[cfg(test)]
#[test]
fn test() {
    let lines = r#"
print("Hello, world!")
print("Hello, world!")
    "#;
    let lexer = Lexer::new(lines);
    let tokens = lexer.collect::<Result<Vector<_>, _>>().unwrap();
    println!("{:#?}", tokens);
    let lines = convert_tokens_to_lines(tokens.into_iter()).unwrap();

    for line in lines {
        println!("{:#?}", line);
    }
}
