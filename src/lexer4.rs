use std::iter;

use crate::lexer::*;
use crate::lexer2::*;
use crate::lexer3::*;
use anyhow::*;
use im::Vector;
use itertools::Itertools;

#[derive(Debug, Clone)]
enum Block {
    Block(Blocktype, BlockLine, Vector<Self>),
    Line(BlockLine),
}
#[derive(Debug, Clone)]
pub enum GroupedBlock {
    StatementBlocks(Vector<StmtBlock>),
    Line(BlockLine),
}
#[derive(Debug, Clone)]
pub struct StmtBlock {
    pub blocktype: Blocktype,
    pub line: BlockLine,
    pub blocks: Vector<GroupedBlock>,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Blocktype {
    If,
    Elif,
    Else,
    While,
    For,
    Def,
    Try,
    Except,
    Finally,
}

pub type BracketedTokens<T> = Vector<BracketedToken<T>>;
pub type ParserLine = Line2<BracketedToken<Token>>;
pub type ParserLines = Vector<ParserLine>;
pub type BlockLine = Line2<BracketedToken<StmtToken>>;
pub type BlockLines = Vector<BlockLine>;

pub fn to_block_line(line: ParserLine) -> Result<BlockLine> {
    let tokens = bracketed_tokens_to_stmt_tokens(line.tokens)?;
    Ok(BlockLine {
        tokens,
        indent: line.indent,
        line_number: line.line_number,
    })
}

fn lines_to_block(mut lines: BlockLines, starting_indent: usize) -> Result<Vector<Block>> {

    let line_positions = lines.iter().positions(|l| l.indent == starting_indent);

    let differences = line_positions
        .chain(iter::once(lines.len()))
        .tuple_windows()
        .map(|(a, b)| b - a)
        .collect_vec();

    let chunks = differences.iter().map(|i| lines.slice(..i));

    let mut blocks = Vector::new();

    for mut chunk in chunks {
        let block = if chunk.len() == 1 {
            Block::Line(chunk.pop_front().unwrap())
        } else {
            let header = chunk.pop_front().unwrap();
            let block_type = get_block_type(&header);
            Block::Block(
                block_type,
                header,
                lines_to_block(chunk, starting_indent + 1)?,
            )
        };
        blocks.push_back(block);
    }
    Ok(blocks)
}

pub const BLOCK_IDENTIFERS: &[&str] = &[
    "if", "elif", "else", "while", "for", "def", "try", "except", "finally",
];
fn get_block_type(line: &BlockLine) -> Blocktype {
    if let BracketedToken::SingleToken(StmtToken::Identifier(s)) = &line.tokens[0] {
        match s.as_str() {
            "if" => Blocktype::If,
            "elif" => Blocktype::Elif,
            "else" => Blocktype::Else,
            "while" => Blocktype::While,
            "for" => Blocktype::For,
            "def" => Blocktype::Def,
            "try" => Blocktype::Try,
            "except" => Blocktype::Except,
            "finally" => Blocktype::Finally,
            _ => unreachable!(),
        }
    } else {
        unreachable!()
    }
}

fn group_blocks(blocks: Vector<Block>) -> Result<Vector<GroupedBlock>> {
    use Blocktype::*;
    let mut result = Vector::new();

    let mut current_block: Vector<StmtBlock> = Vector::new();

    macro_rules! end_block {
        () => {
            if !current_block.is_empty() {
                result.push_back(GroupedBlock::StatementBlocks(std::mem::take(
                    &mut current_block,
                )));
            }
        };
    }

    for block in blocks {
        if let Block::Block(b, l, _) = &block {
            let line = l.line_number;
            macro_rules! check_prev_block {
                ($previous:pat) => {
                    if !matches!(
                        current_block.last(),
                        Some(StmtBlock {
                            blocktype: $previous,
                            ..
                        })
                    ) {
                        bail!(
                            "{:?} block must be preceded by {} block on line {line}",
                            current_block.last(),
                            stringify!($previous)
                        );
                    }
                };
            }
            match *b {
                If | For | While | Try | Def => end_block!(),
                Elif => check_prev_block!(If | Elif),
                Except => check_prev_block!(Try | Except),
                Else => check_prev_block!(If | Elif | Try | Except),
                Finally => check_prev_block!(Try | Except | Else),
            }
            let block = match block {
                Block::Block(blocktype, line, v) => StmtBlock {
                    blocktype,
                    line,
                    blocks: group_blocks(v)?,
                },
                _ => unreachable!(),
            };
            current_block.push_back(block);
        } else if let Block::Line(l) = block {
            end_block!();
            result.push_back(GroupedBlock::Line(l));
        }
    }
    end_block!();

    Ok(result)
}

pub fn group_lines(lines: BlockLines) -> Result<Vector<GroupedBlock>> {
    let blocks = lines_to_block(lines, 0)?;
    let blocks = group_blocks(blocks)?;
    Ok(blocks)
}
