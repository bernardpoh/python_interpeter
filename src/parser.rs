use crate::lexer::*;
use crate::lexer2::*;
use crate::lexer3::*;
use crate::lexer4::*;
use crate::parser_assign::*;
use crate::parser_expr::*;
use anyhow::*;
use im::Vector;
use itertools::Itertools;
use BracketedToken::*;
use StmtToken::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub stmt: StmtType,
    pub line_number: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtType {
    If {
        condition: Expr,
        body: Vector<Stmt>,
        elif: Vector<(Expr, Vector<Stmt>)>,
        else_: Option<Vector<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vector<Stmt>,
    },
    For {
        target: AssignValue,
        iter: Expr,
        body: Vector<Stmt>,
    },
    Def {
        fn_name: String,
        parameters: Vector<String>,
        body: Vector<Stmt>,
    },
    Try {
        body: Vector<Stmt>,
        except: Vector<(Expr, Vec<Stmt>)>,
        else_: Option<Vec<Stmt>>,
        finally: Option<Vec<Stmt>>,
    },
    Return(Expr),
    Break,
    Continue,
    Pass,
    Expr(Expr),
    Del(AssignValue),
    Raise(Expr),
    Global(String),
    Nonlocal(String),
    Import(String),
    FromImport(String, Vec<String>),
    Assert(Expr, Option<Expr>),
    Assign(AssignValue, String, Expr),
}

fn parse_grouped_block(block: GroupedBlock) -> Result<Stmt> {
    dbg!(block.clone());
    match block {
        GroupedBlock::Line(line) => parse_line(line).context("parsing line"),
        GroupedBlock::StatementBlocks(v) => parse_statement_block_group(v).context("parsing block"),
    }
}

pub fn parse_grouped_blocks(blocks: Vector<GroupedBlock>) -> Result<Vector<Stmt>> {
    blocks.into_iter().map(parse_grouped_block).try_collect()
}

fn parse_line(line: BlockLine) -> Result<Stmt> {
    let Line2 {
        line_number,
        tokens,
        ..
    } = line;
    let stmt = parse_line_tokens(tokens).with_context(|| anyhow!("on line {}", line_number))?;
    Ok(Stmt { stmt, line_number })
}

fn parse_line_tokens(tokens: BracketedTokens<StmtToken>) -> Result<StmtType> {
    if let Some(p) = find_assign_position(&tokens) {
        return parse_assign_stmt(tokens, p).context("parsing assign statement");
    }
    if let SingleToken(Identifier(s)) = &tokens[0] {
        let result = parse_keyword_stmt(s, tokens.clone()).context("parsing keyword")?;
        if let Some(result) = result {
            return Ok(result);
        }
    }
    let error_message = anyhow!("parsing expression {:#?}", tokens);
    let expr = parse_expr(tokens).with_context(|| error_message)?;
    Ok(StmtType::Expr(expr))
}

fn parse_keyword_stmt(s: &str, mut tokens: BracketedTokens<StmtToken>) -> Result<Option<StmtType>> {
    let result = match s {
        "break" => StmtType::Break,
        "continue" => StmtType::Continue,
        "pass" => StmtType::Pass,
        "return" => {
            let expr = parse_expr(tokens.split_off(1))?;
            StmtType::Return(expr)
        }
        "del" => {
            let assign_value = parse_assign_value(tokens.split_off(1))?;
            StmtType::Del(assign_value)
        }
        "assert" => {
            todo!()
        }
        "raise" => {
            let expr = parse_expr(tokens.split_off(1))?;
            StmtType::Raise(expr)
        }
        "global" => {
            todo!()
        }
        "nonlocal" => {
            todo!()
        }
        "import" => {
            todo!()
        }
        "from" => {
            todo!()
        }
        _ => return Ok(None),
    };
    Ok(Some(result))
}

fn parse_assign_stmt(tokens: BracketedTokens<StmtToken>, p: usize) -> Result<StmtType> {
    let (left, mid, right) = split_vector(tokens, p);
    let operator = match mid {
        SingleToken(Assignment(s)) => s,
        _ => unreachable!(),
    };
    let assign_value = parse_assign_value(left).context("parsing lhs of assign statement")?;
    let expr = parse_expr(right).context("parsing rhs of assign statement")?;
    Ok(StmtType::Assign(assign_value, operator, expr))
}

fn find_assign_position(tokens: &Vector<BracketedToken<StmtToken>>) -> Option<usize> {
    tokens
        .iter()
        .position(|t| matches!(t, SingleToken(Assignment(_))))
}

fn parse_statement_block_group(blocks: Vector<StmtBlock>) -> Result<Stmt> {
    for StmtBlock { line, .. } in &blocks {
        if line.tokens.last() != Some(&SingleToken(Colon)) {
            bail!("missing colon on line {}", line.line_number)
        }
    }
    let line_number = blocks[0].line.line_number;

    use Blocktype::*;
    let statement = match &blocks[0].blocktype {
        If => {
            let mut blocks = blocks;
            let else_ = blocks
                .iter()
                .position(|StmtBlock { blocktype: b, .. }| matches!(b, Else));

            let else_ = match else_ {
                None => None,
                Some(_) => blocks.pop_back().map(|StmtBlock { blocks: b, .. }| b),
            };

            let else_ = else_.map(parse_grouped_blocks).transpose()?;

            let if_ = blocks.pop_front().unwrap();

            fn get_condition_and_expr(
                StmtBlock {
                    mut line, blocks, ..
                }: StmtBlock,
            ) -> Result<(Expr, Vector<Stmt>)> {
                trim_ends(&mut line);
                let condition = parse_expr(line.tokens)
                    .with_context(|| anyhow!("on line {}", line.line_number))?;
                let body = parse_grouped_blocks(blocks)?;
                Ok((condition, body))
            }

            let (condition, body) = get_condition_and_expr(if_).context("on if statement")?;
            let elif = blocks
                .into_iter()
                .map(get_condition_and_expr)
                .try_collect()
                .context("on elif statement")?;

            StmtType::If {
                condition,
                body,
                elif,
                else_,
            }
        }
        Try => {
            todo!()
        }
        While => {
            let block = blocks.into_iter().next().unwrap();
            let StmtBlock {
                mut line, blocks, ..
            } = block;
            trim_ends(&mut line);
            let condition = parse_expr(line.tokens)
                .with_context(|| anyhow!("invalid while loop on line {}", line.line_number))?;
            let body = parse_grouped_blocks(blocks)?;
            StmtType::While { condition, body }
        }
        For => {
            let block = blocks.into_iter().next().unwrap();
            let StmtBlock {
                mut line, blocks, ..
            } = block;
            trim_ends(&mut line);
            let in_ = line
                .tokens
                .iter()
                .position(|t| {
                    matches!(
                        t,
                        SingleToken(Identifier(s)) if s == "in"
                    )
                })
                .ok_or(anyhow!("invalid for loop on line {}", line.line_number))?;
            let (mut target, iter) = line.tokens.split_at(in_ + 1);

            let a = target.pop_back();
            assert_eq!(a, Some(SingleToken(Identifier("in".to_string()))));

            let target = parse_assign_value(target)
                .with_context(|| anyhow!("on line {}", line.line_number))?;
            let iter = parse_expr(iter)
                .with_context(|| anyhow!("invalid for loop iter on line {}", line.line_number))?;
            let body = parse_grouped_blocks(blocks)?;
            StmtType::For { target, iter, body }
        }
        Def => {
            let block = blocks.into_iter().next().unwrap();
            let StmtBlock {
                mut line, blocks, ..
            } = block;

            trim_ends(&mut line);
            let fn_name = line.tokens.pop_front();
            let parameters = line.tokens.pop_front();
            dbg!(&fn_name, &parameters);

            let fn_name = match fn_name {
                Some(SingleToken(Identifier(s))) => s,
                _ => bail!("invalid function name on line {}", line.line_number),
            };

            let parameters = match parameters {
                Some(Bracketed(BracketType::RoundBracket, v)) => {
                    parse_comma_separated_list(bracketed_stmt_tokens_to_expr_tokens(v)?)?
                }
                _ => bail!("invalid function arguments on line {}", line.line_number),
            };

            let parameters = parameters.into_iter().map(make_string).try_collect()?;

            let body = parse_grouped_blocks(blocks)?;

            StmtType::Def {
                fn_name,
                parameters,
                body,
            }
        }
        _ => unreachable!(),
    };
    Ok(Stmt {
        stmt: statement,
        line_number,
    })
}

fn make_string(token: Expr) -> Result<String> {
    match token {
        Expr::Identifier(s) => Ok(s),
        _ => bail!("invalid argument"),
    }
}

fn trim_ends(line: &mut BlockLine) {
    let a = line.tokens.pop_front();
    assert!(matches!(a, Some(SingleToken(Identifier(_)))));
    let b = line.tokens.pop_back();
    assert_eq!(b, Some(SingleToken(Colon)));
}

pub fn split_vector<T>(v: Vector<T>, p: usize) -> (Vector<T>, T, Vector<T>)
where
    T: Clone,
{
    let (left, mut right) = v.split_at(p);
    let mid = right.pop_front().unwrap();
    (left, mid, right)
}
