use anyhow::*;
use num_bigint::BigInt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BracketType {
    RoundBracket,
    SquareBracket,
    CurlyBracket,
}
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Symbol(String),
    OpenBracket(BracketType),
    CloseBracket(BracketType),
    IntLiteral(BigInt),
    FloatLiteral(f64),
    StringLiteral(String),
    Newline,
    Comment(String),
    Whitespace(String),
}

const BRACKETS: [char; 6] = ['(', ')', '[', ']', '{', '}'];
const QUOTES: [char; 2] = ['\"', '\''];

struct DetectionResult {
    token: Token,
    length: usize,
}
pub struct Lexer<'a> {
    source: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    fn next_token(&mut self) -> Result<Option<Token>> {
        let source = &self.source[self.position..];
        let result = detect_token(source)?;
        if let Some(result) = result {
            self.position += result.length;

            Ok(Some(result.token))
        } else {
            Ok(None)
        }
    }

    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            position: 0,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}

fn detect_token(source: &str) -> Result<Option<DetectionResult>> {
    let first_char = match source.chars().next() {
        Some(c) => c,
        None => return Ok(None),
    };
    let result = match first_char {
        c if c.is_alphabetic() || c == '_' => detect_identifier(source),
        c if BRACKETS.contains(&c) => detect_brackets(c),
        c if QUOTES.contains(&c) => detect_string_literal(source, c)?,
        c if is_python_whitespace(c) => detect_whitespace(source),
        '\n' => detect_newline(),
        c if c.is_ascii_digit() => detect_number(source)?,
        '#' => detect_comment(source),
        c if is_symbol(c) => detect_symbol(source),
        c => unreachable!("{c}"),
    };
    Ok(Some(result))
}

fn is_python_whitespace(c: char) -> bool {
    c.is_whitespace() && c != '\n' || c == '\t'
}

fn is_symbol(c: char) -> bool {
    !(BRACKETS.contains(&c) || c.is_whitespace() || c.is_alphanumeric() || QUOTES.contains(&c))
}

macro_rules! get_detect_function_with_string {
    ($fn_name: ident, $predicate: expr, $token_type: path) => {
        fn $fn_name(source: &str) -> DetectionResult {
            let first_char = source.chars().take(1);
            let rest_of_string = source.chars().skip(1).take_while($predicate);
            let string: String = itertools::chain!(first_char, rest_of_string).collect();
            let length = string.len();
            let token = $token_type(string);
            DetectionResult { token, length }
        }
    };
}

get_detect_function_with_string!(
    detect_identifier,
    |c: &char| c.is_alphanumeric() || *c == '_',
    Token::Identifier
);

get_detect_function_with_string!(detect_symbol, |c: &char| is_symbol(*c), Token::Symbol);

get_detect_function_with_string!(
    detect_whitespace,
    |c: &char| is_python_whitespace(*c),
    Token::Whitespace
);

get_detect_function_with_string!(detect_comment, |c: &char| c != &'\n', Token::Comment);

fn detect_number(source: &str) -> Result<DetectionResult> {
    let mut length = 1;
    let mut is_float = false;
    for c in source.chars().skip(1) {
        match c {
            c if c.is_ascii_digit() => (),
            '.' => is_float = true,
            _ => break,
        }
        length += 1;
    }
    let string = &source[..length];
    let token = if is_float {
        let float = string.parse().with_context(||anyhow!("tried parsing '{string}'"))?;
        Token::FloatLiteral(float)
    } else {
        let int = string.parse().with_context(||anyhow!("tried parsing '{string}'"))?;
        Token::IntLiteral(int)
    };

    Ok(DetectionResult { token, length })
}

fn detect_brackets(c: char) -> DetectionResult {
    let token = match c {
        '(' => Token::OpenBracket(BracketType::RoundBracket),
        ')' => Token::CloseBracket(BracketType::RoundBracket),
        '[' => Token::OpenBracket(BracketType::SquareBracket),
        ']' => Token::CloseBracket(BracketType::SquareBracket),
        '{' => Token::OpenBracket(BracketType::CurlyBracket),
        '}' => Token::CloseBracket(BracketType::CurlyBracket),
        _ => unreachable!(),
    };
    DetectionResult { token, length: 1 }
}

fn detect_newline() -> DetectionResult {
    let token = Token::Newline;
    DetectionResult { token, length: 1 }
}

fn detect_string_literal(source: &str, c: char) -> Result<DetectionResult> {
    let quote_type = c;
    let mut length = 1;
    let mut is_escaped = false;
    let mut string = String::new();
    for c in source.chars().skip(1) {
        length += 1;
        if is_escaped {
            let escaped_char = get_escaped_char(c);
            string.push(escaped_char);
            is_escaped = false;
            continue;
        }
        match c {
            '\\' => is_escaped = true,
            c if c == quote_type => break,
            _ => string.push(c),
        }
    }

    if string.len() == source.len() - 1 {
        bail!("Unclosed string literal");
    }

    let token = Token::StringLiteral(string);
    Ok(DetectionResult { token, length })
}

fn get_escaped_char(c: char) -> char {
    match c {
        'n' => '\n',
        't' => '\t',
        'r' => '\r',
        _ => c,
    }
}

#[cfg(test)]
#[test]
fn test_token() {
    let source = "123.456";
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::FloatLiteral(123.456));
    assert_eq!(result.length, 7);

    let source = "123";
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::IntLiteral(BigInt::from(123)));
    assert_eq!(result.length, 3);

    let source = "abc";
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::Identifier("abc".to_string()));
    assert_eq!(result.length, 3);

    let source = "abc123";
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::Identifier("abc123".to_string()));
    assert_eq!(result.length, 6);

    let source = "abc123_";
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::Identifier("abc123_".to_string()));
    assert_eq!(result.length, 7);

    let source = r#""abc""#;
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::StringLiteral("abc".to_string()));
    assert_eq!(result.length, 5);

    let source = r#""abc\"""#;
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::StringLiteral(r#"abc""#.to_string()));
    assert_eq!(result.length, 7);

    let source = r#""abc\n""#;
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::StringLiteral("abc\n".to_string()));
    assert_eq!(result.length, 7);

    assert_eq!('\t'.is_whitespace(), true);

    let source = "\t";
    let result = detect_token(source).unwrap().unwrap();
    assert_eq!(result.token, Token::Whitespace("\t".to_string()));
}
#[cfg(test)]
#[test]
fn test_tokens() {
    use itertools::Itertools;

    let source = "print(\"hello world\")";
    let lexer = Lexer::new(source);
    let tokens: Vec<_> = lexer.try_collect().unwrap();
    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0], Token::Identifier("print".to_string()));
    assert_eq!(tokens[1], Token::OpenBracket(BracketType::RoundBracket));
    assert_eq!(tokens[2], Token::StringLiteral("hello world".to_string()));
    assert_eq!(tokens[3], Token::CloseBracket(BracketType::RoundBracket));

    let source = "print(\"hello world\")\nprint(\"hello world\")";
    println!("{source}");
    let lexer = Lexer::new(source);
    let tokens: Vec<_> = lexer.try_collect().unwrap();
    assert_eq!(tokens.len(), 9);
    assert_eq!(tokens[0], Token::Identifier("print".to_string()));
    assert_eq!(tokens[1], Token::OpenBracket(BracketType::RoundBracket));
    assert_eq!(tokens[2], Token::StringLiteral("hello world".to_string()));
    assert_eq!(tokens[3], Token::CloseBracket(BracketType::RoundBracket));
    assert_eq!(tokens[4], Token::Newline);
    assert_eq!(tokens[5], Token::Identifier("print".to_string()));
    assert_eq!(tokens[6], Token::OpenBracket(BracketType::RoundBracket));
    assert_eq!(tokens[7], Token::StringLiteral("hello world".to_string()));
    assert_eq!(tokens[8], Token::CloseBracket(BracketType::RoundBracket));
}
