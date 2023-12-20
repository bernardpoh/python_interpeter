use crate::lexer::*;
use crate::lexer2::*;
use crate::lexer3::*;
use crate::lexer4::*;
use crate::parser_expr::*;
use anyhow::*;
use im::Vector;
use itertools::Itertools;

#[derive(Debug, Clone, PartialEq)]
pub enum AssignValue {
    List(Vector<Self>),
    Identifier(String, Vector<IdentifierData>),
}
#[derive(Debug, Clone, PartialEq)]
pub enum IdentifierData {
    Index(Expr),
    Attribute(String),
    Slice([Option<Expr>; 3]),
}

pub fn parse_assign_value(tokens: BracketedTokens<StmtToken>) -> Result<AssignValue> {
    let tokens = bracketed_stmt_tokens_to_expr_tokens(tokens)?;
    parse_assign_tokens(tokens)
}

fn parse_assign_tokens(tokens: BracketedTokens<ExprToken>) -> Result<AssignValue> {
    let tokens = separate_commas(tokens);

    let result = match tokens.len() {
        0 => bail!("cannot assign value to nothing"),
        1 => parse_single_assign_value(tokens.into_iter().next().unwrap())?,
        _ => parse_list(tokens)?,
    };
    Ok(result)
}

fn parse_list(tokens: Vector<BracketedTokens<ExprToken>>) -> Result<AssignValue> {
    Ok(AssignValue::List(
        tokens
            .into_iter()
            .map(parse_single_assign_value)
            .try_collect()
            .context("parsing multiple assignments")?,
    ))
}

fn parse_single_assign_value(mut tokens: BracketedTokens<ExprToken>) -> Result<AssignValue> {
    let first_token = tokens.pop_front().unwrap();
    use BracketType::*;
    use BracketedToken::*;
    let identifier = match first_token {
        SingleToken(ExprToken::Identifier(s)) => s,
        _ if tokens.len() > 1 => bail!("cannot assign value to {:?}", first_token),
        Bracketed(RoundBracket, v) => return parse_assign_tokens(v),
        Bracketed(SquareBracket, v) => return parse_list(separate_commas(v)),
        _ => bail!("cannot assign value to {:?}", first_token),
    };

    let mut identifier_data = Vector::new();
    while let Some(token) = tokens.pop_front() {
        let data = match token {
            SingleToken(ExprToken::Dot) => {
                let token = tokens.pop_front().ok_or(anyhow!("invalid attribute"))?;
                let attribute = match token {
                    SingleToken(ExprToken::Identifier(s)) => s,
                    _ => bail!("invalid attribute"),
                };
                IdentifierData::Attribute(attribute)
            }
            Bracketed(SquareBracket, v) => parse_indexing(v).context("parsing indexing")?,
            _ => bail!("invalid identifier data"),
        };
        identifier_data.push_back(data);
    }
    Ok(AssignValue::Identifier(identifier, identifier_data))
}

pub fn parse_indexing(v: Vector<BracketedToken<ExprToken>>) -> Result<IdentifierData> {
    let items = separate_colons(v);
    let result = if items.len() == 1 {
        let tokens = items.into_iter().next().unwrap();
        let expr = parse_expr_tokens(tokens).context("parsing subscription")?;
        IdentifierData::Index(expr)
    } else if items.len() > 3 {
        bail!("invalid slice")
    } else {
        let mut items = items.into_iter();
        let items = [items.next(), items.next(), items.next()];
        let items: Vec<_> = items.into_iter().map(get_expr_or_none).try_collect().context("parsing slice")?;
        IdentifierData::Slice(items.try_into().unwrap())
    };
    Ok(result)
}

fn get_expr_or_none(tokens: Option<BracketedTokens<ExprToken>>) -> Result<Option<Expr>> {
    let tokens = match tokens {
        Some(tokens) => tokens,
        None => return Ok(None),
    };
    if tokens.is_empty() {
        Ok(None)
    } else {
        let expr = parse_expr_tokens(tokens)?;
        Ok(Some(expr))
    }
}
