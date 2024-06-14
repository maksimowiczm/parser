use lexing::lexer::basic_lexer::{BasicLexer, LexerError, Token};
use lexing::lexer::Lexer;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(Default)]
pub struct PqlLexer {
    base_lexer: BasicLexer,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(test, derive(Clone))]
pub enum PqlToken {
    Number(i32),
    Select(Vec<String>),
    SuchThat,
    Comma,
    Word(String),
    Eof,
    Underscore,
    LeftParenthesis,
    RightParenthesis,
    Declaration(String, Vec<String>),
    SemiColon,
}

#[derive(Debug)]
pub enum PqlLexerError {
    ExpectedSuchThat,
    UnexpectedCharacter(char),
    UnexpectedToken(Token),
}

impl Display for PqlLexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PqlLexerError::ExpectedSuchThat => write!(f, "Expected 'such that'"),
            PqlLexerError::UnexpectedCharacter(ch) => write!(f, "Unexpected character '{}'", ch),
            PqlLexerError::UnexpectedToken(token) => write!(f, "Unexpected token {:?}", token),
        }
    }
}

impl Error for PqlLexerError {}

impl Lexer<PqlToken, PqlLexerError> for PqlLexer {
    fn tokenize(&self, input: &str) -> Result<Vec<PqlToken>, PqlLexerError> {
        let tokens = self.base_lexer.tokenize(input).map_err(|e| match e {
            LexerError::UnexpectedCharacter(ch) => PqlLexerError::UnexpectedCharacter(ch),
        })?;

        let mut iter = tokens.iter().peekable();
        let mut result = Vec::new();

        while let Some(token) = iter.next() {
            match token {
                Token::Word(word) => match word.as_str() {
                    "select" | "Select" | "SELECT" => {
                        let mut select_clause = vec![];
                        while let Some(token) = iter.next() {
                            match token {
                                Token::Word(word) => {
                                    if !word.eq_ignore_ascii_case("such") {
                                        select_clause.push(word.clone());
                                    } else {
                                        break;
                                    }
                                }
                                _ => continue,
                            }
                        }

                        if !iter
                            .next()
                            .and_then(|token| match token {
                                Token::Word(word) => Some(word.eq_ignore_ascii_case("that")),
                                _ => None,
                            })
                            .unwrap_or(false)
                        {
                            return Err(PqlLexerError::ExpectedSuchThat);
                        }

                        result.push(PqlToken::Select(select_clause));
                        result.push(PqlToken::SuchThat);
                    }
                    _ => {
                        if let Some(Token::Multiply) = iter.peek() {
                            iter.next();
                            result.push(PqlToken::Word(format!("{}*", word.clone())));
                        } else {
                            result.push(PqlToken::Word(word.clone()));
                        }
                    }
                },
                _ => result.push(map_token(token)?),
            }
        }

        Ok(result)
    }
}

fn map_token(token: &Token) -> Result<PqlToken, PqlLexerError> {
    let result = match token {
        Token::Number(number) => PqlToken::Number(*number),
        Token::Eof => PqlToken::Eof,
        Token::LeftParenthesis => PqlToken::LeftParenthesis,
        Token::RightParenthesis => PqlToken::RightParenthesis,
        Token::Comma => PqlToken::Comma,
        Token::Word(word) => PqlToken::Word(word.clone()),
        Token::Underscore => PqlToken::Underscore,
        Token::SemiColon => PqlToken::SemiColon,
        _ => return Err(PqlLexerError::UnexpectedToken(token.clone())),
    };

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::follows_underscore_reference("Select s such that Follows(_, s)", &[
        PqlToken::Select(vec!["s".to_string()]),
        PqlToken::SuchThat,
        PqlToken::Word("Follows".to_string()),
        PqlToken::LeftParenthesis,
        PqlToken::Underscore,
        PqlToken::Comma,
        PqlToken::Word("s".to_string()),
        PqlToken::RightParenthesis,
        PqlToken::Eof,
    ])]
    #[case::follows_transitive_underscore_reference("Select s such that Follows*(_, s)", &[
        PqlToken::Select(vec!["s".to_string()]),
        PqlToken::SuchThat,
        PqlToken::Word("Follows*".to_string()),
        PqlToken::LeftParenthesis,
        PqlToken::Underscore,
        PqlToken::Comma,
        PqlToken::Word("s".to_string()),
        PqlToken::RightParenthesis,
        PqlToken::Eof,
    ])]
    #[case::parent_underscore_both_references("Select s such that Parent(_, _)", &[
        PqlToken::Select(vec!["s".to_string()]),
        PqlToken::SuchThat,
        PqlToken::Word("Parent".to_string()),
        PqlToken::LeftParenthesis,
        PqlToken::Underscore,
        PqlToken::Comma,
        PqlToken::Underscore,
        PqlToken::RightParenthesis,
        PqlToken::Eof,
    ])]
    #[case::parent_transitive_reference_underscore("Select s such that Parent*(s, _)", &[
        PqlToken::Select(vec!["s".to_string()]),
        PqlToken::SuchThat,
        PqlToken::Word("Parent*".to_string()),
        PqlToken::LeftParenthesis,
        PqlToken::Word("s".to_string()),
        PqlToken::Comma,
        PqlToken::Underscore,
        PqlToken::RightParenthesis,
        PqlToken::Eof,
    ])]
    fn test_select(#[case] input: &str, #[case] expected: &[PqlToken]) {
        let pql_lexer = PqlLexer::default();
        let tokens = pql_lexer.tokenize(input).unwrap();

        assert_eq!(tokens, expected);
    }
}
