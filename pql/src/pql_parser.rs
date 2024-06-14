use crate::pql_lexer::{PqlLexerError, PqlToken};
use crate::query::query_builder::{QueryBuilder, ResultType};
use crate::query::Query;
use lexing::lexer::Lexer;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;

pub struct PqlParser {
    lexer: Box<dyn Lexer<PqlToken, PqlLexerError>>,
}

#[derive(Debug)]
pub enum PqlParserError {
    UnexpectedEndOfInput,
    UnexpectedDeclaration(String),
    LexerError(PqlLexerError),
    ExpectedResultType,
    ExpectedStatementArgument,
    UnexpectedToken {
        unexpected: PqlToken,
        expected: Vec<PqlToken>,
        near_tokens: Vec<PqlToken>,
    },
}

impl Display for PqlParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PqlParserError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            PqlParserError::LexerError(err) => write!(f, "Lexer error: {}", err),
            PqlParserError::UnexpectedDeclaration(declaration) => {
                write!(f, "Unexpected declaration: {}", declaration)
            }
            PqlParserError::UnexpectedToken {
                unexpected,
                expected,
                near_tokens,
            } => write!(
                f,
                "Unexpected token: {:?}, expected: {:?}, near tokens: {:?}",
                unexpected, expected, near_tokens
            ),
            PqlParserError::ExpectedResultType => write!(f, "Expected result type"),
            PqlParserError::ExpectedStatementArgument => write!(f, "Expected statement argument"),
        }
    }
}

impl Error for PqlParserError {}

impl PqlParser {
    pub fn new(lexer: Box<dyn Lexer<PqlToken, PqlLexerError>>) -> Self {
        Self { lexer }
    }

    pub fn parse(&mut self, input: &str) -> Result<Query, PqlParserError> {
        let mut tokens = self
            .lexer
            .tokenize(input)
            .map_err(|err| PqlParserError::LexerError(err))?
            .into_iter()
            .peekable();

        let mut query_builder = QueryBuilder::default();

        select(&mut tokens, &mut query_builder)?;

        let query = query_builder.build();

        Ok(query)
    }
}

fn expect_token(
    tokens: &mut impl Iterator<Item = PqlToken>,
    expected: PqlToken,
) -> Result<(), PqlParserError> {
    let token = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;

    match token == expected {
        true => Ok(()),
        false => Err(PqlParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![expected],
            near_tokens: tokens.take(6).collect(),
        }),
    }
}

fn select(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken>>,
    builder: &mut QueryBuilder,
) -> Result<(), PqlParserError> {
    while let Some(token) = tokens.peek() {
        if let PqlToken::Select(select) = token {
            result(select, builder)?;
            tokens.next();
            break;
        }

        let declaration = declaration(tokens)?;
        builder.add_declaration(declaration);
    }

    condition(tokens, builder)
}

fn declaration(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken>>,
) -> Result<(String, Vec<String>), PqlParserError> {
    let token = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;

    if let PqlToken::Word(declaration) = token {
        let result = match declaration.as_str() {
            "stmt" => Ok(("stmt".to_string(), declaration_names(tokens)?)),
            "assign" => Ok(("assign".to_string(), declaration_names(tokens)?)),
            "while" => Ok(("while".to_string(), declaration_names(tokens)?)),
            "if" => Ok(("if".to_string(), declaration_names(tokens)?)),
            "call" => Ok(("call".to_string(), declaration_names(tokens)?)),
            _ => Err(PqlParserError::UnexpectedDeclaration(declaration)),
        }?;

        expect_token(tokens, PqlToken::SemiColon)?;

        Ok(result)
    } else {
        Err(PqlParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![
                PqlToken::Word("stmt".to_string()),
                PqlToken::Word("assign".to_string()),
                PqlToken::Word("while".to_string()),
                PqlToken::Word("if".to_string()),
                PqlToken::Word("call".to_string()),
            ],
            near_tokens: tokens.take(6).collect(),
        })
    }
}

fn declaration_names(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken>>,
) -> Result<Vec<String>, PqlParserError> {
    let mut names = vec![];

    loop {
        let token = tokens.peek().ok_or(PqlParserError::UnexpectedEndOfInput)?;

        match token {
            PqlToken::Word(name) => {
                names.push(name.clone());
                tokens.next();
            }
            PqlToken::Comma => {
                tokens.next();
                continue;
            }
            _ => break,
        }
    }

    Ok(names)
}

fn result(select: &Vec<String>, builder: &mut QueryBuilder) -> Result<(), PqlParserError> {
    if select.len() == 1 {
        let token = select.first().unwrap();

        if token == "BOOLEAN" {
            builder.set_result(ResultType::Boolean);
        } else {
            builder.set_result(ResultType::Single(token.clone()));
        }

        Ok(())
    } else {
        Err(PqlParserError::ExpectedResultType)
    }
}

fn condition(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken>>,
    builder: &mut QueryBuilder,
) -> Result<(), PqlParserError> {
    let token = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;

    match token {
        PqlToken::SuchThat => such_that(tokens, builder),
        _ => Err(PqlParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![PqlToken::SuchThat],
            near_tokens: tokens.take(6).collect(),
        }),
    }
}

fn such_that(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken>>,
    builder: &mut QueryBuilder,
) -> Result<(), PqlParserError> {
    let token = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;

    match token {
        PqlToken::Word(ref word) => match word.as_str() {
            "Follows" => follows(tokens, builder),
            "Parent" => parent(tokens, builder),
            _ => Err(create_unexpected_token_error(token, tokens)),
        },
        _ => Err(create_unexpected_token_error(token, tokens)),
    }?;

    and_such_that(tokens, builder)?;

    return Ok(());

    fn create_unexpected_token_error(
        token: PqlToken,
        tokens: &mut Peekable<impl Iterator<Item = PqlToken>>,
    ) -> PqlParserError {
        PqlParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![
                PqlToken::Word("Follows".to_string()),
                PqlToken::Word("Parent".to_string()),
            ],
            near_tokens: tokens.take(6).collect(),
        }
    }
}

fn and_such_that(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken>>,
    builder: &mut QueryBuilder,
) -> Result<(), PqlParserError> {
    let token = tokens.peek().ok_or(PqlParserError::UnexpectedEndOfInput)?;

    match token {
        PqlToken::Word(ref word) => match word.as_str() {
            "and" => {
                tokens.next();
                such_that(tokens, builder)
            }
            _ => Ok(()),
        },
        _ => Ok(()),
    }
}

fn follows(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken> + Sized>,
    builder: &mut QueryBuilder,
) -> Result<(), PqlParserError> {
    expect_token(tokens, PqlToken::LeftParenthesis)?;
    let predecessor = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::Comma)?;
    let follower = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::RightParenthesis)?;

    let predecessor = match predecessor {
        PqlToken::Underscore => "_".to_string(),
        PqlToken::Word(word) => word,
        _ => return Err(PqlParserError::ExpectedStatementArgument),
    };

    let follower = match follower {
        PqlToken::Underscore => "_".to_string(),
        PqlToken::Word(word) => word,
        _ => return Err(PqlParserError::ExpectedStatementArgument),
    };

    builder.add_follows(predecessor, follower);

    Ok(())
}

fn parent(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken> + Sized>,
    builder: &mut QueryBuilder,
) -> Result<(), PqlParserError> {
    expect_token(tokens, PqlToken::LeftParenthesis)?;
    let parent = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::Comma)?;
    let child = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::RightParenthesis)?;

    let parent = match parent {
        PqlToken::Underscore => "_".to_string(),
        PqlToken::Word(word) => word,
        _ => return Err(PqlParserError::ExpectedStatementArgument),
    };

    let child = match child {
        PqlToken::Underscore => "_".to_string(),
        PqlToken::Word(word) => word,
        _ => return Err(PqlParserError::ExpectedStatementArgument),
    };

    builder.add_parent(parent, child);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pql_lexer::PqlLexer;
    use lexing::lexer::Lexer;
}