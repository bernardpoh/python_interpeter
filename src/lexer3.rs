use crate::lexer::*;
use crate::lexer2::*;
use anyhow::*;
use im::{vector, Vector};
use itertools::Itertools;
use num_bigint::BigInt;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprToken {
    Operator(String),
    Identifier(String),
    Literal(Literal),
    Comma,
    Colon,
    Dot,
}
#[derive(Debug, Clone, PartialEq)]
pub enum StmtToken {
    Symbol(String),
    Assignment(String),
    Identifier(String),
    Literal(Literal),
    Comma,
    Colon,
    Dot,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Literal {
    Int(BigInt),
    Float(f64),
    String(String),
    Bool(bool),
    None,
}

fn convert_expr_token(t: StmtToken) -> Result<Vec<ExprToken>> {
    let token = match t {
        StmtToken::Symbol(s) => return separate_operator(s),
        StmtToken::Identifier(s) => match s.as_str() {
            "and" | "or" | "not" | "is" | "in" => ExprToken::Operator(s),
            _ => ExprToken::Identifier(s),
        },
        StmtToken::Literal(l) => ExprToken::Literal(l),
        StmtToken::Comma => ExprToken::Comma,
        StmtToken::Colon => ExprToken::Colon,
        StmtToken::Dot => ExprToken::Dot,
        StmtToken::Assignment(_) => unreachable!(),
    };
    Ok(vec![token])
}

fn separate_operator(mut string: String) -> Result<Vec<ExprToken>> {
    let operator = match string.as_str() {
        "+" | "-" | "*" | "/" | "%" | "**" | "//" | "<<" | ">>" | "==" | "!=" | "<" | ">"
        | "<=" | ">=" | "~" => ExprToken::Operator(string),
        s if matches!(s.chars().last(), Some('+' | '-' | '~')) => {
            let last_operator = string.pop().unwrap();
            let mut operators = separate_operator(string)?;
            operators.push(ExprToken::Operator(last_operator.to_string()));
            return Ok(operators);
        }
        _ => bail!("invalid symbol: {}", string),
    };
    Ok(vec![operator])
}

fn convert_statement_token(t: Token) -> Result<Vec<StmtToken>> {
    let token = match t {
        Token::Symbol(s) => match s.as_str() {
            "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "//=" | "<<=" | ">>=" => {
                StmtToken::Assignment(s)
            }
            "," => StmtToken::Comma,
            ":" => StmtToken::Colon,
            "." => StmtToken::Dot,
            _ => StmtToken::Symbol(s),
        },
        Token::Identifier(s) => match s.as_str() {
            "True" => StmtToken::Literal(Literal::Bool(true)),
            "False" => StmtToken::Literal(Literal::Bool(false)),
            "None" => StmtToken::Literal(Literal::None),
            _ => StmtToken::Identifier(s),
        },
        Token::IntLiteral(i) => StmtToken::Literal(Literal::Int(i)),
        Token::FloatLiteral(f) => StmtToken::Literal(Literal::Float(f)),
        Token::StringLiteral(s) => StmtToken::Literal(Literal::String(s)),
        Token::OpenBracket(_) => bail!("unexpected open bracket"),
        Token::CloseBracket(_) => bail!("unexpected close bracket"),
        _ => return Ok(vec![]),
    };
    Ok(vec![token])
}

fn convert_bracketed_tokens<T, U, V, F>(
    tokens: BracketedTokens<T>,
    f: F,
) -> Result<BracketedTokens<U>>
where
    T: Clone + PartialEq,
    U: Clone + PartialEq,
    F: Fn(T) -> Result<V> + Copy,
    V: IntoIterator<Item = U> + Clone,
{
    let convert = |t: BracketedToken<T>| -> Result<BracketedTokens<U>> {
        let result = match t {
            BracketedToken::SingleToken(t) => f(t)?
                .into_iter()
                .map(|t| BracketedToken::SingleToken(t))
                .collect(),
            BracketedToken::Bracketed(b, v) => vector![BracketedToken::Bracketed(
                b,
                convert_bracketed_tokens(v, f)?
            )],
        };
        Ok(result)
    };
    let x: Vector<BracketedTokens<U>> = tokens.into_iter().map(convert).try_collect()?;
    Ok(x.into_iter().flatten().collect())
}

use crate::lexer4::BracketedTokens;

pub fn bracketed_tokens_to_stmt_tokens(
    tokens: BracketedTokens<Token>,
) -> Result<BracketedTokens<StmtToken>> {
    convert_bracketed_tokens(tokens, convert_statement_token)
}

pub fn bracketed_stmt_tokens_to_expr_tokens(
    tokens: BracketedTokens<StmtToken>,
) -> Result<BracketedTokens<ExprToken>> {
    let tokens = convert_bracketed_tokens(tokens, convert_expr_token)?;
    let tokens = join_operators(tokens);
    Ok(tokens)
}

fn join_operators(tokens: BracketedTokens<ExprToken>) -> BracketedTokens<ExprToken> {
    use BracketedToken::*;
    use ExprToken::*;
    let mut result = Vector::new();
    for t in tokens {
        if let (Some(SingleToken(Operator(a))), SingleToken(Operator(b))) = (result.last(), &t) {
            let (a, b) = (a.clone(), b.clone());
            if matches!((a.as_str(), b.as_str()), ("is", "not") | ("not", "in")) {
                result.pop_back();
                result.push_back(SingleToken(Operator(format!("{} {}", a, b))));
                continue;
            }
        }

        let next_token = match t {
            Bracketed(b, v) => Bracketed(b, join_operators(v)),
            SingleToken(t) => SingleToken(t),
        };
        result.push_back(next_token);
    }
    result
}

