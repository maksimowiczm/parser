use basic_lexer::LexerError;
use lexing::lexer::basic_lexer::{BasicLexer, Token};
use lexing::lexer::{basic_lexer, Lexer};
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Default)]
pub struct SimpleLexer {
    base_lexer: BasicLexer,
}

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleToken {
    Number(i32),
    Plus,
    Minus,
    Multiply,
    Eof,
    LeftParenthesis,
    RightParenthesis,
    LeftBrace,
    RightBrace,
    Equal,
    SemiColon,
    Procedure(String),
    Reference(String),
    While(String),
    If(String),
    Else,
    Call(String),
}

impl Display for SimpleToken {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleToken::Number(number) => write!(f, "Number {}", number),
            SimpleToken::Plus => write!(f, "Plus"),
            SimpleToken::Minus => write!(f, "Minus"),
            SimpleToken::Multiply => write!(f, "Multiply"),
            SimpleToken::Eof => write!(f, "Eof"),
            SimpleToken::LeftParenthesis => write!(f, "LeftParenthesis"),
            SimpleToken::RightParenthesis => write!(f, "RightParenthesis"),
            SimpleToken::LeftBrace => write!(f, "LeftBrace"),
            SimpleToken::RightBrace => write!(f, "RightBrace"),
            SimpleToken::Equal => write!(f, "Equal"),
            SimpleToken::SemiColon => write!(f, "SemiColon"),
            SimpleToken::Procedure { .. } => write!(f, "Procedure"),
            SimpleToken::Reference(name) => write!(f, "Reference {}", name),
            SimpleToken::While { .. } => write!(f, "While"),
            SimpleToken::If { .. } => write!(f, "If"),
            SimpleToken::Else => write!(f, "Else"),
            SimpleToken::Call(name) => write!(f, "Call {}", name),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SimpleLexerError {
    ExpectedProcedureName,
    ExpectedReferenceName,
    UnexpectedCharacter(char),
}

impl Display for SimpleLexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleLexerError::ExpectedProcedureName => write!(f, "Expected procedure name"),
            SimpleLexerError::ExpectedReferenceName => write!(f, "Expected reference name"),
            SimpleLexerError::UnexpectedCharacter(ch) => {
                write!(f, "Unexpected character: {}", ch)
            }
        }
    }
}

impl Error for SimpleLexerError {}

impl Lexer<SimpleToken, SimpleLexerError> for SimpleLexer {
    fn tokenize(&self, input: &str) -> Result<Vec<SimpleToken>, SimpleLexerError> {
        let tokens = self.base_lexer.tokenize(input).map_err(|e| match e {
            LexerError::UnexpectedCharacter(ch) => SimpleLexerError::UnexpectedCharacter(ch),
        })?;

        let mut iter = tokens.iter();
        let mut result = Vec::new();

        while let Some(token) = iter.next() {
            match token {
                Token::Word(word) => match word.as_str() {
                    "procedure" => {
                        if let Some(Token::Word(name)) = iter.next() {
                            result.push(SimpleToken::Procedure(name.clone()));
                        } else {
                            return Err(SimpleLexerError::ExpectedProcedureName);
                        }
                    }
                    "while" => {
                        if let Some(Token::Word(name)) = iter.next() {
                            result.push(SimpleToken::While(name.clone()));
                        } else {
                            return Err(SimpleLexerError::ExpectedReferenceName);
                        }
                    }
                    "if" => {
                        let name = match iter.next() {
                            Some(Token::Word(name)) => name,
                            _ => return Err(SimpleLexerError::ExpectedReferenceName),
                        };

                        let then = match iter.next() {
                            Some(Token::Word(then)) => then,
                            _ => return Err(SimpleLexerError::ExpectedReferenceName),
                        };

                        if then != "then" {
                            return Err(SimpleLexerError::ExpectedReferenceName);
                        }

                        result.push(SimpleToken::If(name.clone()));
                    }
                    "else" => result.push(SimpleToken::Else),
                    "call" => {
                        if let Some(Token::Word(name)) = iter.next() {
                            result.push(SimpleToken::Call(name.clone()));
                        } else {
                            return Err(SimpleLexerError::ExpectedReferenceName);
                        }
                    }
                    _ => result.push(SimpleToken::Reference(word.clone())),
                },
                _ => result.push(map_token(token)?),
            }
        }

        Ok(result)
    }
}

fn map_token(token: &Token) -> Result<SimpleToken, SimpleLexerError> {
    let result = match token {
        Token::Number(n) => SimpleToken::Number(n.clone()),
        Token::Plus => SimpleToken::Plus,
        Token::Minus => SimpleToken::Minus,
        Token::Multiply => SimpleToken::Multiply,
        Token::Eof => SimpleToken::Eof,
        Token::LeftParenthesis => SimpleToken::LeftParenthesis,
        Token::RightParenthesis => SimpleToken::RightParenthesis,
        Token::LeftBrace => SimpleToken::LeftBrace,
        Token::RightBrace => SimpleToken::RightBrace,
        Token::Equal => SimpleToken::Equal,
        Token::SemiColon => SimpleToken::SemiColon,
        _ => panic!("Unexpected token"),
    };

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::procedure_main("procedure main { }", &[
        SimpleToken::Procedure("main".to_string()),
        SimpleToken::LeftBrace,
        SimpleToken::RightBrace,
        SimpleToken::Eof,
    ])]
    #[case::reference_while("while x { }", &[
        SimpleToken::While("x".to_string()),
        SimpleToken::LeftBrace,
        SimpleToken::RightBrace,
        SimpleToken::Eof,
    ])]
    #[case::if_statement("if x then { }", &[
        SimpleToken::If("x".to_string()),
        SimpleToken::LeftBrace,
        SimpleToken::RightBrace,
        SimpleToken::Eof,
    ])]
    #[case::simple_expression("1 + 2 * 3", &[
        SimpleToken::Number(1),
        SimpleToken::Plus,
        SimpleToken::Number(2),
        SimpleToken::Multiply,
        SimpleToken::Number(3),
        SimpleToken::Eof,
    ])]
    #[case::simple_expression_with_parenthesis("1 + (2 * 3)", &[
        SimpleToken::Number(1),
        SimpleToken::Plus,
        SimpleToken::LeftParenthesis,
        SimpleToken::Number(2),
        SimpleToken::Multiply,
        SimpleToken::Number(3),
        SimpleToken::RightParenthesis,
        SimpleToken::Eof,
    ])]
    fn test_simple_lexer(#[case] input: &'static str, #[case] output: &[SimpleToken]) {
        let lexer = SimpleLexer::default();
        let tokens = lexer.tokenize(input).unwrap();

        assert_eq!(tokens, output);
    }
}
