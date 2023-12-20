use crate::lexer::*;
use crate::lexer2::*;
use crate::lexer3::*;
use crate::lexer4::*;
use crate::parser::split_vector;
use crate::parser_assign::*;
use anyhow::*;
use im::Vector;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier(String),
    Tuple(Vector<Expr>),
    List(Vector<Expr>),
    Dict(Vector<(Expr, Expr)>),
    Index(Box<Expr>, Box<Expr>),
    Slice(Box<Expr>, [Option<Box<Expr>>; 3]),
    Literal(Literal),
    BinaryOp(Box<Expr>, String, Box<Expr>),
    UnaryOp(String, Box<Expr>),
    FunctionCall(Box<Expr>, Vector<Expr>),
    MethodCall(Box<Expr>, String, Vector<Expr>),
    Attribute(Box<Expr>, String),
    ListComprehension(Box<Expr>, String, Box<Expr>),
    DictComprehension(Box<[Expr; 2]>, Box<Expr>, Box<Expr>),
    Lambda(Vector<String>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
enum PartialExpr {
    Expr(Expr),
    Token(BracketedToken<ExprToken>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperatorType {
    Binary,
    Unary,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OtherOperator {
    Attribute,
    Subscript,
    FunctionCall,
    MethodCall,
}

use OperatorType::*;
const OPERATORS_BY_PRECEDENCE: &[(&[&str], OperatorType)] = &[
    (&["**"], Binary),
    (&["~", "+", "-"], Unary),
    (&["*", "/", "//", "%"], Binary),
    (&["+", "-"], Binary),
    (&["<<", ">>"], Binary),
    (&["&"], Binary),
    (&["^"], Binary),
    (&["|"], Binary),
    (
        &[
            "in", "not in", "is", "is not", "<", "<=", ">", ">=", "!=", "==",
        ],
        Binary,
    ),
    (&["not"], Unary),
    (&["and"], Binary),
    (&["or"], Binary),
];

pub fn parse_expr(tokens: BracketedTokens<StmtToken>) -> Result<Expr> {
    let tokens = bracketed_stmt_tokens_to_expr_tokens(tokens)?;
    let error_message = anyhow!("parsing expr {:#?}", tokens);
    let expr = parse_expr_tokens(tokens).with_context(|| error_message)?;
    Ok(expr)
}

pub fn parse_expr_tokens(tokens: BracketedTokens<ExprToken>) -> Result<Expr> {
    if let Some(colon_pos) = find_colon(&tokens) {
        return parse_lambda_expression(tokens, colon_pos)
            .context("found colon, parsing lambda expression");
    }
    let tokens: Vector<_> = tokens.into_iter().map(|t| PartialExpr::Token(t)).collect();
    parse_partial_exprs(tokens)
}

fn parse_partial_exprs(mut tokens: Vector<PartialExpr>) -> Result<Expr> {
    loop {
        if let Some((operator_pos, operator_type)) = find_other_operator(&tokens) {
            use OtherOperator::*;
            let length = match operator_type {
                FunctionCall | Subscript => 2,
                Attribute => 3,
                MethodCall => 4,
            };
            let start = operator_pos
                .checked_sub(length - 1)
                .ok_or(anyhow!("parse error"))?;
            let end = operator_pos;
            let operator_tokens = tokens.slice(start..=end);
            let expr = match operator_type {
                Attribute => parse_attribute(operator_tokens)?,
                Subscript => parse_subscript(operator_tokens)?,
                FunctionCall => parse_function(operator_tokens)?,
                MethodCall => parse_method_call(operator_tokens)?,
            };
            tokens.insert(start, PartialExpr::Expr(expr));
            continue;
        }
        if let Some((operator_pos, operator_type)) = find_operator_by_precedence(&tokens) {
            let start = match operator_type {
                OperatorType::Binary => {
                    operator_pos.checked_sub(1).ok_or(anyhow!("parse error"))?
                }
                OperatorType::Unary => operator_pos,
            };

            let end = tokens
                .iter()
                .skip(operator_pos)
                .position(|t| is_value(t))
                .ok_or(anyhow!("parse error"))?
                + operator_pos;

            let operator_tokens = tokens.slice(start..=end);
            let expr = match operator_type {
                OperatorType::Binary => parse_binary_expr(operator_tokens)?,
                OperatorType::Unary => parse_unary_expr(operator_tokens)?,
            };
            tokens.insert(start, PartialExpr::Expr(expr));
            continue;
        }
        if tokens.len() == 1 {
            break;
        } else {
            bail!("parse error - did not find operator");
        }
    }

    let final_value = tokens.into_iter().next().unwrap();
    let result = parse_partial_expr(final_value)?;
    Ok(result)
}

fn parse_binary_expr(mut tokens: Vector<PartialExpr>) -> Result<Expr> {
    let left = parse_partial_expr(tokens.pop_front().unwrap())?;
    let operator = get_operator_string(tokens.pop_front().unwrap())?;
    let right = parse_partial_exprs(tokens)?;
    Ok(Expr::BinaryOp(Box::new(left), operator, Box::new(right)))
}

fn parse_unary_expr(mut tokens: Vector<PartialExpr>) -> Result<Expr> {
    let operator = get_operator_string(tokens.pop_front().unwrap())?;
    let right = parse_partial_exprs(tokens)?;
    Ok(Expr::UnaryOp(operator, Box::new(right)))
}

fn get_operator_string(token: PartialExpr) -> Result<String> {
    match token {
        PartialExpr::Token(BracketedToken::SingleToken(ExprToken::Operator(s))) => Ok(s),
        _ => bail!("parse error"),
    }
}

fn find_other_operator(tokens: &Vector<PartialExpr>) -> Option<(usize, OtherOperator)> {
    use BracketType::*;
    use BracketedToken::*;
    use ExprToken::*;
    use PartialExpr::*;
    let mut prev_tokens = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        const ARRAY_LEN: usize = 4;
        prev_tokens.push(token);
        let slice = prev_tokens.len().saturating_sub(ARRAY_LEN);
        if matches!(
            prev_tokens[slice..],
            [
                expr,
                &Token(SingleToken(Dot)),
                &Token(SingleToken(Identifier(_))),
                &Token(Bracketed(RoundBracket, _))
            ] if is_value(expr)
        ) {
            return Some((i, OtherOperator::MethodCall));
        }
        prev_tokens.truncate(ARRAY_LEN);
    }
    let mut prev_tokens = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        const ARRAY_LEN: usize = 3;
        prev_tokens.push(token);
        let slice = prev_tokens.len().saturating_sub(ARRAY_LEN);
        if matches!(
            prev_tokens[slice..],
            [
                expr,
                &Token(SingleToken(Dot)),
                &Token(SingleToken(Identifier(_))),
            ] if is_value(expr)
        ) {
            return Some((i, OtherOperator::Attribute));
        }
        prev_tokens.truncate(ARRAY_LEN);
    }
    let mut prev_tokens = Vec::new();
    for (i, token) in tokens.iter().enumerate() {
        const ARRAY_LEN: usize = 2;
        prev_tokens.push(token);
        let slice = prev_tokens.len().saturating_sub(ARRAY_LEN);
        match prev_tokens[slice..] {
            [expr, &Token(Bracketed(RoundBracket, _))] if is_value(expr) => {
                return Some((i, OtherOperator::FunctionCall))
            }
            [expr, &Token(Bracketed(SquareBracket, _))] if is_value(expr) => {
                return Some((i, OtherOperator::Subscript))
            }
            _ => (),
        }
        prev_tokens.truncate(ARRAY_LEN);
    }
    None
}

fn find_operator_by_precedence(tokens: &Vector<PartialExpr>) -> Option<(usize, OperatorType)> {
    OPERATORS_BY_PRECEDENCE
        .into_iter()
        .find_map(|(operators, operator_type)| find_operators(tokens, operators, *operator_type))
}

fn find_operators(
    tokens: &Vector<PartialExpr>,
    operators: &[&str],
    operator_type: OperatorType,
) -> Option<(usize, OperatorType)> {
    let result = match operator_type {
        OperatorType::Binary => find_binary_operator(tokens, operators),
        OperatorType::Unary => find_unary_operator(tokens, operators),
    };
    result.map(|x| (x, operator_type))
}

fn find_unary_operator(tokens: &Vector<PartialExpr>, operators: &[&str]) -> Option<usize> {
    let mut is_prev_token_value = false;

    for (i, token) in tokens.iter().enumerate() {
        if is_specific_operator(token, operators) && !is_prev_token_value {
            return Some(i);
        }

        is_prev_token_value = is_value(token);
    }

    None
}

fn find_binary_operator(tokens: &Vector<PartialExpr>, operators: &[&str]) -> Option<usize> {
    tokens
        .iter()
        .position(|t| is_specific_operator(t, operators))
}

fn is_specific_operator(token: &PartialExpr, operators: &[&str]) -> bool {
    matches!(token, PartialExpr::Token(BracketedToken::SingleToken(ExprToken::Operator(s))) if operators.contains(&s.as_str()))
}

fn is_value(token: &PartialExpr) -> bool {
    match token {
        PartialExpr::Token(
            BracketedToken::SingleToken(ExprToken::Identifier(_))
            | BracketedToken::SingleToken(ExprToken::Literal(_))
            | BracketedToken::Bracketed(_, _),
        )
        | PartialExpr::Expr(_) => true,
        _ => false,
    }
}

fn parse_single_token(token: ExprToken) -> Result<Expr> {
    match token {
        ExprToken::Identifier(s) => Ok(Expr::Identifier(s)),
        ExprToken::Literal(l) => Ok(Expr::Literal(l)),
        _ => bail!("parse error"),
    }
}

fn parse_partial_expr(token: PartialExpr) -> Result<Expr> {
    let result = match token {
        PartialExpr::Expr(e) => e,
        PartialExpr::Token(token) => match token {
            BracketedToken::SingleToken(t) => parse_single_token(t)?,
            BracketedToken::Bracketed(BracketType::RoundBracket, b) => parse_tuple(b)?,
            BracketedToken::Bracketed(BracketType::SquareBracket, b) => parse_list(b)?,
            BracketedToken::Bracketed(BracketType::CurlyBracket, b) => parse_dict(b)?,
        },
    };
    Ok(result)
}

fn separate_expr(
    tokens: BracketedTokens<ExprToken>,
    seperator_token: ExprToken,
) -> Vector<BracketedTokens<ExprToken>> {
    let comparison_item = BracketedToken::SingleToken(seperator_token);
    let mut items = Vector::new();
    let mut current_item = Vector::new();
    for token in tokens {
        if &token == &comparison_item {
            items.push_back(std::mem::take(&mut current_item))
        } else {
            current_item.push_back(token)
        }
    }
    items.push_back(current_item);
    items
}

pub fn separate_commas(tokens: BracketedTokens<ExprToken>) -> Vector<BracketedTokens<ExprToken>> {
    let mut items = separate_expr(tokens, ExprToken::Comma);
    if matches!(items.last(), Some(v) if v.is_empty()) {
        items.pop_back();
    }
    items
}

pub fn separate_colons(tokens: BracketedTokens<ExprToken>) -> Vector<BracketedTokens<ExprToken>> {
    separate_expr(tokens, ExprToken::Colon)
}

fn parse_dict(tokens: BracketedTokens<ExprToken>) -> Result<Expr> {
    let items = separate_commas(tokens);
    let values: Vector<_> = items.into_iter().map(get_key_value).try_collect()?;
    Ok(Expr::Dict(values))
}

fn get_key_value(tokens: BracketedTokens<ExprToken>) -> Result<(Expr, Expr)> {
    let colon = tokens
        .iter()
        .position(|t| matches!(t, BracketedToken::SingleToken(ExprToken::Colon)))
        .ok_or(anyhow!("missing colon when parsing dict"))?;

    let (left, _, right) = split_vector(tokens, colon);
    let key = parse_expr_tokens(left)?;
    let value = parse_expr_tokens(right)?;
    Ok((key, value))
}

pub fn parse_comma_separated_list(tokens: BracketedTokens<ExprToken>) -> Result<Vector<Expr>> {
    let items = separate_commas(tokens);
    let values: Vector<_> = items
        .into_iter()
        .map(|i| parse_expr_tokens(i))
        .try_collect()
        .context("parsing comma separated list")?;
    Ok(values)
}

fn parse_list(tokens: BracketedTokens<ExprToken>) -> Result<Expr> {
    let values = parse_comma_separated_list(tokens).context("parsing list")?;
    Ok(Expr::List(values))
}

fn parse_tuple(tokens: BracketedTokens<ExprToken>) -> Result<Expr> {
    let values = parse_comma_separated_list(tokens)?;
    let result = match values.len() {
        1 => values.into_iter().next().unwrap(),
        _ => Expr::Tuple(values),
    };

    Ok(result)
}

fn parse_function(tokens: Vector<PartialExpr>) -> Result<Expr> {
    let (first, second) = tokens.into_iter().tuples().into_iter().next().unwrap();
    let function = parse_partial_expr(first)?;
    let args = match second {
        PartialExpr::Token(BracketedToken::Bracketed(BracketType::RoundBracket, v)) => v,
        _ => unreachable!(),
    };
    let args = parse_comma_separated_list(args)?;
    Ok(Expr::FunctionCall(Box::new(function), args))
}

fn parse_subscript(tokens: Vector<PartialExpr>) -> Result<Expr> {
    let (first, second) = tokens.into_iter().next_tuple().unwrap();
    let expr = parse_partial_expr(first)?;
    let subscript = match second {
        PartialExpr::Token(BracketedToken::Bracketed(BracketType::SquareBracket, v)) => v,
        _ => unreachable!(),
    };
    let identifier_data = parse_indexing(subscript)?;
    match identifier_data {
        IdentifierData::Index(index) => Ok(Expr::Index(Box::new(expr), Box::new(index))),
        IdentifierData::Slice(slice) => Ok(Expr::Slice(
            Box::new(expr),
            slice.map(|a| a.map(|a| Box::new(a))),
        )),
        _ => unreachable!(),
    }
}

fn parse_attribute(tokens: Vector<PartialExpr>) -> Result<Expr> {
    let (first, _, third) = tokens.into_iter().next_tuple().unwrap();
    let expr = parse_partial_expr(first)?;
    let s = get_operator_string(third).unwrap();
    Ok(Expr::Attribute(Box::new(expr), s))
}

fn parse_method_call(tokens: Vector<PartialExpr>) -> Result<Expr> {
    let (first, _, third, fourth) = tokens.into_iter().next_tuple().unwrap();
    let expr = parse_partial_expr(first)?;
    let s = match third {
        PartialExpr::Token(BracketedToken::SingleToken(ExprToken::Identifier(s))) => s,
        _ => unreachable!(),
    };
    let args = match fourth {
        PartialExpr::Token(BracketedToken::Bracketed(BracketType::RoundBracket, v)) => v,
        _ => unreachable!(),
    };
    let args = parse_comma_separated_list(args)?;
    Ok(Expr::MethodCall(Box::new(expr), s, args))
}

fn find_colon(tokens: &BracketedTokens<ExprToken>) -> Option<usize> {
    tokens
        .iter()
        .position(|t| matches!(t, BracketedToken::SingleToken(ExprToken::Colon)))
}

fn parse_lambda_expression(tokens: BracketedTokens<ExprToken>, colon_pos: usize) -> Result<Expr> {
    let (mut left, _, right) = split_vector(tokens, colon_pos);
    match left.pop_front() {
        Some(BracketedToken::SingleToken(ExprToken::Identifier(s))) if s == "lambda" => (),
        _ => bail!("parse error"),
    }
    let args = separate_colons(left);
    let args = args.into_iter().map(get_identifier).try_collect()?;
    let expr = parse_expr_tokens(right)?;
    Ok(Expr::Lambda(args, Box::new(expr)))
}

fn get_identifier(tokens: BracketedTokens<ExprToken>) -> Result<String> {
    ensure!(tokens.len() == 1, "parse error");
    match tokens.into_iter().next().unwrap() {
        BracketedToken::SingleToken(ExprToken::Identifier(s)) => Ok(s),
        _ => bail!("parse error"),
    }
}
