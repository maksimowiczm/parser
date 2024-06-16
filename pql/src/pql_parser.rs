use crate::pql_lexer::{PqlLexerError, PqlToken};
use crate::query::query_builder::{QueryBuilder, ResultType};
use crate::query::Query;
use lexing::lexer::Lexer;
use pkb::pkb_context::PkbContext;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::rc::Rc;

pub struct PqlParser {
    lexer: Box<dyn Lexer<PqlToken, PqlLexerError>>,
    builder: Box<dyn QueryBuilder>,
    pkb_context: Rc<PkbContext>,
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
    pub fn new(
        lexer: Box<dyn Lexer<PqlToken, PqlLexerError>>,
        builder: Box<dyn QueryBuilder>,
        pkb_context: Rc<PkbContext>,
    ) -> Self {
        Self {
            lexer,
            builder,
            pkb_context,
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<Query, PqlParserError> {
        let mut tokens = self
            .lexer
            .tokenize(input)
            .map_err(|err| PqlParserError::LexerError(err))?
            .into_iter()
            .peekable();

        select(&mut tokens, self.builder.as_mut())?;

        let query = self.builder.build(self.pkb_context.clone());

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
    builder: &mut dyn QueryBuilder,
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

fn result(select: &Vec<String>, builder: &mut dyn QueryBuilder) -> Result<(), PqlParserError> {
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
    builder: &mut dyn QueryBuilder,
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
    builder: &mut dyn QueryBuilder,
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
    builder: &mut dyn QueryBuilder,
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
    builder: &mut dyn QueryBuilder,
) -> Result<(), PqlParserError> {
    expect_token(tokens, PqlToken::LeftParenthesis)?;
    let predecessor = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::Comma)?;
    let follower = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::RightParenthesis)?;

    let predecessor = argument_token_to_string(predecessor)?;
    let follower = argument_token_to_string(follower)?;

    builder.add_follows(predecessor, follower);

    Ok(())
}

fn parent(
    tokens: &mut Peekable<impl Iterator<Item = PqlToken> + Sized>,
    builder: &mut dyn QueryBuilder,
) -> Result<(), PqlParserError> {
    expect_token(tokens, PqlToken::LeftParenthesis)?;
    let parent = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::Comma)?;
    let child = tokens.next().ok_or(PqlParserError::UnexpectedEndOfInput)?;
    expect_token(tokens, PqlToken::RightParenthesis)?;

    let parent = argument_token_to_string(parent)?;
    let child = argument_token_to_string(child)?;

    builder.add_parent(parent, child);

    Ok(())
}

fn argument_token_to_string(token: PqlToken) -> Result<String, PqlParserError> {
    let result = match token {
        PqlToken::Underscore => "_".to_string(),
        PqlToken::Word(word) => word,
        PqlToken::Number(number) => number.to_string(),
        _ => return Err(PqlParserError::ExpectedStatementArgument),
    };

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::query::query_builder::MockQueryBuilder;
    use mockall::predicate::eq;
    use rstest::rstest;

    #[rstest]
    #[case::parent_both_underscore(
        &[
            PqlToken::Word("Parent".to_string()),
            PqlToken::LeftParenthesis,
            PqlToken::Underscore,
            PqlToken::Comma,
            PqlToken::Underscore,
            PqlToken::RightParenthesis,
            PqlToken::Eof,
        ],
        |b: &mut MockQueryBuilder| {
            b.expect_add_parent().with(eq("_".to_string()), eq("_".to_string())).times(1).returning(|_, _| {});
        }
    )]
    #[case::parent_both_words(
        &[
            PqlToken::Word("Parent".to_string()),
            PqlToken::LeftParenthesis,
            PqlToken::Word("p".to_string()),
            PqlToken::Comma,
            PqlToken::Word("c".to_string()),
            PqlToken::RightParenthesis,
            PqlToken::Eof,
        ],
        |b: &mut MockQueryBuilder| {
            b.expect_add_parent().with(eq("p".to_string()), eq("c".to_string())).times(1).returning(|_, _| {});
        }
    )]
    #[case::follows_both_underscore(
        &[
            PqlToken::Word("Follows".to_string()),
            PqlToken::LeftParenthesis,
            PqlToken::Underscore,
            PqlToken::Comma,
            PqlToken::Underscore,
            PqlToken::RightParenthesis,
            PqlToken::Eof,
        ],
        |b: &mut MockQueryBuilder| {
            b.expect_add_follows().with(eq("_".to_string()), eq("_".to_string())).times(1).returning(|_, _| {});
        }
    )]
    #[case::follows_both_words(
        &[
            PqlToken::Word("Follows".to_string()),
            PqlToken::LeftParenthesis,
            PqlToken::Word("p".to_string()),
            PqlToken::Comma,
            PqlToken::Word("c".to_string()),
            PqlToken::RightParenthesis,
            PqlToken::Eof,
        ],
        |b: &mut MockQueryBuilder| {
            b.expect_add_follows().with(eq("p".to_string()), eq("c".to_string())).times(1).returning(|_, _| {});
        }
    )]
    #[case::both_numbers(
        &[
            PqlToken::Word("Follows".to_string()),
            PqlToken::LeftParenthesis,
            PqlToken::Number(1),
            PqlToken::Comma,
            PqlToken::Number(2),
            PqlToken::RightParenthesis,
            PqlToken::Eof,
        ],
        |b: &mut MockQueryBuilder| {
            b.expect_add_follows().with(eq("1".to_string()), eq("2".to_string())).times(1).returning(|_, _| {});
        }
    )]
    fn test_parent(
        #[case] tokens: &[PqlToken],
        #[case] mock_setup: fn(&mut MockQueryBuilder),
    ) -> Result<(), PqlParserError> {
        let mut builder = MockQueryBuilder::new();
        mock_setup(&mut builder);

        let mut tokens = tokens.iter().cloned().peekable();
        such_that(&mut tokens, &mut builder)
    }

    struct TestLexer {
        tokens: Vec<PqlToken>,
    }
    impl Lexer<PqlToken, PqlLexerError> for TestLexer {
        fn tokenize(&self, _input: &str) -> Result<Vec<PqlToken>, PqlLexerError> {
            Ok(self.tokens.clone())
        }
    }

    #[rstest]
    #[case::follows_query_with_declarations(
        vec![
            PqlToken::Word("stmt".to_string()),
            PqlToken::Word("s".to_string()),
            PqlToken::SemiColon,
            PqlToken::Word("assign".to_string()),
            PqlToken::Word("a".to_string()),
            PqlToken::Comma,
            PqlToken::Word("b".to_string()),
            PqlToken::SemiColon,
            PqlToken::Select(vec!["s".to_string()]),
            PqlToken::SuchThat,
            PqlToken::Word("Follows".to_string()),
            PqlToken::LeftParenthesis,
            PqlToken::Underscore,
            PqlToken::Comma,
            PqlToken::Word("s".to_string()),
            PqlToken::RightParenthesis,
            PqlToken::Eof,
        ],
        |b: &mut MockQueryBuilder| {
            b.expect_add_declaration().with(eq(("stmt".to_string(), vec!["s".to_string()]))).times(1).returning(|_| {});
            b.expect_add_declaration().with(eq(("assign".to_string(), vec!["a".to_string(), "b".to_string()]))).times(1).returning(|_| {});
            b.expect_set_result().with(eq(ResultType::Single("s".to_string()))).times(1).returning(|_| {});
            b.expect_add_follows().with(eq("_".to_string()), eq("s".to_string())).times(1).returning(|_, _| {});
            b.expect_build().times(1).returning(|_| Query::default());
        }
    )]
    #[case::with_numbers(
        vec![
            PqlToken::Word("stmt".to_string()),
            PqlToken::Word("s".to_string()),
            PqlToken::SemiColon,
            PqlToken::Select(vec!["s".to_string()]),
            PqlToken::SuchThat,
            PqlToken::Word("Parent".to_string()),
            PqlToken::LeftParenthesis,
            PqlToken::Word("1".to_string()),
            PqlToken::Comma,
            PqlToken::Word("2".to_string()),
            PqlToken::RightParenthesis,
            PqlToken::Eof,
        ],
        |b: &mut MockQueryBuilder| {
            b.expect_add_declaration().with(eq(("stmt".to_string(), vec!["s".to_string()]))).times(1).returning(|_| {});
            b.expect_set_result().with(eq(ResultType::Single("s".to_string()))).times(1).returning(|_| {});
            b.expect_add_parent().with(eq("1".to_string()), eq("2".to_string())).times(1).returning(|_, _| {});
            b.expect_build().times(1).returning(|_| Query::default());
        }
    )]
    fn test_parse(
        #[case] tokens: Vec<PqlToken>,
        #[case] mock_setup: fn(&mut MockQueryBuilder),
    ) -> Result<(), PqlParserError> {
        let mut builder = MockQueryBuilder::new();
        mock_setup(&mut builder);

        let context = Default::default();
        let mut parser = PqlParser::new(
            Box::new(TestLexer { tokens }),
            Box::new(builder),
            Rc::new(context),
        );
        parser.parse("")?;

        Ok(())
    }
}
