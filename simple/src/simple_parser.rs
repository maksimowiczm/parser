use crate::simple_lexer::{SimpleLexerError, SimpleToken};
use lexing::lexer::Lexer;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;

pub struct SimpleParser {
    lexer: Box<dyn Lexer<SimpleToken, SimpleLexerError>>,
}

#[derive(Debug)]
pub enum SimpleParserError {
    UnexpectedToken(SimpleToken),
    UnexpectedEndOfInput,
    LexerError(SimpleLexerError),
}

impl Display for SimpleParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleParserError::UnexpectedToken(token) => {
                write!(f, "Unexpected token: {:?}", token)
            }
            SimpleParserError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            SimpleParserError::LexerError(err) => write!(f, "Lexer error: {}", err),
        }
    }
}

impl Error for SimpleParserError {}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Times,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Program {
        procedures: Vec<Box<Node>>,
    },
    Procedure {
        name: String,
        body: Vec<Box<Node>>,
    },
    StatementList {
        statements: Vec<Box<Node>>,
    },
    Assign {
        line: u32,
        variable: String,
        expression: Box<Node>,
    },
    While {
        line: u32,
        variable: String,
        statements: Box<Node>,
    },
    If {
        line: u32,
        variable: String,
        if_statements: Box<Node>,
        else_statements: Box<Node>,
    },
    Call {
        line: u32,
        name: String,
    },
    Expression {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },
    Reference {
        name: String,
    },
    Constant {
        value: i32,
    },
}

impl SimpleParser {
    pub fn new(lexer: Box<dyn Lexer<SimpleToken, SimpleLexerError>>) -> Self {
        SimpleParser { lexer }
    }

    pub fn parse_expression(&self, input: &str) -> Result<Node, SimpleParserError> {
        let mut tokens = self
            .lexer
            .tokenize(input)
            .map_err(|err| SimpleParserError::LexerError(err))?
            .into_iter()
            .peekable();

        expression(&mut tokens)
    }

    pub fn parse_program(&self, input: &str) -> Result<Node, SimpleParserError> {
        let mut tokens = self
            .lexer
            .tokenize(input)
            .map_err(|err| SimpleParserError::LexerError(err))?
            .into_iter()
            .peekable();

        let mut procedures = Vec::new();
        while let Some(_) = tokens.peek() {
            if tokens.peek().unwrap() == &SimpleToken::Eof {
                break;
            }
            procedures.push(Box::new(procedure(&mut tokens, 1)?));
        }

        Ok(Node::Program { procedures })
    }
}

fn expect_token(
    tokens: &mut impl Iterator<Item = SimpleToken>,
    expected: SimpleToken,
) -> Result<(), SimpleParserError> {
    match tokens
        .next()
        .ok_or(SimpleParserError::UnexpectedEndOfInput)?
        .eq(&expected)
    {
        true => Ok(()),
        false => Err(SimpleParserError::UnexpectedToken(expected)),
    }
}

fn procedure(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: u32,
) -> Result<Node, SimpleParserError> {
    let name = match tokens.next() {
        Some(SimpleToken::Procedure(name)) => name,
        _ => return Err(SimpleParserError::UnexpectedEndOfInput),
    };

    expect_token(tokens, SimpleToken::LeftBrace)?;

    let mut body = Vec::new();
    while let Some(token) = tokens.peek() {
        match token {
            SimpleToken::RightBrace => {
                tokens.next();
                break;
            }
            _ => body.push(Box::new(statement(tokens, line)?)),
        }
    }

    Ok(Node::Procedure { name, body })
}

fn statement(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: u32,
) -> Result<Node, SimpleParserError> {
    match tokens.peek() {
        Some(SimpleToken::Reference(_)) => assign(tokens, line),
        _ => Err(SimpleParserError::UnexpectedToken(
            tokens.next().unwrap_or(SimpleToken::Eof),
        )),
    }
}

fn assign(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: u32,
) -> Result<Node, SimpleParserError> {
    let variable = match tokens.next() {
        Some(SimpleToken::Reference(name)) => name,
        _ => return Err(SimpleParserError::UnexpectedEndOfInput),
    };

    expect_token(tokens, SimpleToken::Equal)?;

    let expression = expression(tokens)?;

    expect_token(tokens, SimpleToken::SemiColon)?;

    Ok(Node::Assign {
        line,
        variable,
        expression: Box::new(expression),
    })
}

fn expression(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
) -> Result<Node, SimpleParserError> {
    let mut node = term(tokens)?;

    while let Some(token) = tokens.peek() {
        match token {
            SimpleToken::Plus => {
                tokens.next();
                let right = expression(tokens)?;
                node = Node::Expression {
                    left: Box::new(node),
                    operator: Operator::Plus,
                    right: Box::new(right),
                };
            }
            SimpleToken::Minus => {
                tokens.next();
                let right = expression(tokens)?;
                node = Node::Expression {
                    left: Box::new(node),
                    operator: Operator::Minus,
                    right: Box::new(right),
                };
            }
            _ => break,
        }
    }

    Ok(node)
}

fn term(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
) -> Result<Node, SimpleParserError> {
    let mut node = factor(tokens)?;

    while let Some(token) = tokens.peek() {
        match token {
            SimpleToken::Multiply => {
                tokens.next();
                let right = factor(tokens)?;
                node = Node::Expression {
                    left: Box::new(node),
                    operator: Operator::Times,
                    right: Box::new(right),
                };
            }
            _ => break,
        }
    }

    Ok(node)
}

fn factor(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
) -> Result<Node, SimpleParserError> {
    let token = tokens
        .next()
        .ok_or(SimpleParserError::UnexpectedEndOfInput)?;

    match token {
        SimpleToken::Number(value) => Ok(Node::Constant { value }),
        SimpleToken::LeftParenthesis => {
            let node = expression(tokens)?;

            expect_token(tokens, SimpleToken::RightParenthesis)?;

            Ok(node)
        }
        SimpleToken::Reference(name) => Ok(Node::Reference { name }),
        _ => Err(SimpleParserError::UnexpectedToken(token)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case::constant(
        &[SimpleToken::Number(1), SimpleToken::Eof],
        Node::Constant { value: 1 }
    )]
    #[case::subtraction(
        &[SimpleToken::Number(1), SimpleToken::Minus, SimpleToken::Number(2), SimpleToken::Eof],
        Node::Expression {
            left: Box::new(Node::Constant { value: 1 }),
            operator: Operator::Minus,
            right: Box::new(Node::Constant { value: 2 }),
        }
    )]
    #[case::addition(
        &[SimpleToken::Number(1), SimpleToken::Plus, SimpleToken::Number(2), SimpleToken::Eof],
        Node::Expression {
            left: Box::new(Node::Constant { value: 1 }),
            operator: Operator::Plus,
            right: Box::new(Node::Constant { value: 2 }),
        }
    )]
    #[case::expression_with_multiple_operators(
        &[SimpleToken::Number(1), SimpleToken::Plus, SimpleToken::Number(2), SimpleToken::Multiply, SimpleToken::Number(3), SimpleToken::Eof],
        Node::Expression {
            left: Box::new(Node::Constant { value: 1 }),
            operator: Operator::Plus,
            right: Box::new(Node::Expression {
                left: Box::new(Node::Constant { value: 2 }),
                operator: Operator::Times,
                right: Box::new(Node::Constant { value: 3 }),
            }),
        }
    )]
    #[case::parenthesis(
        &[SimpleToken::Number(1),
        SimpleToken::Plus,
        SimpleToken::LeftParenthesis,
        SimpleToken::Reference("x".to_string()),
        SimpleToken::Plus,
        SimpleToken::Number(2),
        SimpleToken::RightParenthesis,
        SimpleToken::Multiply,
        SimpleToken::Reference("z".to_string()),
        SimpleToken::Minus,
        SimpleToken::Number(3),
        SimpleToken::Eof],
        Node::Expression {
            left: Box::new(Node::Constant { value: 1 }),
            operator: Operator::Plus,
            right: Box::new(Node::Expression {
                left: Box::new(Node::Expression {
                    left: Box::new(Node::Expression {
                        left: Box::new(Node::Reference { name: "x".to_string() }),
                        operator: Operator::Plus,
                        right: Box::new(Node::Constant { value: 2 }),
                    }),
                    operator: Operator::Times,
                    right: Box::new(Node::Reference { name: "z".to_string() }),
                }),
                operator: Operator::Minus,
                right: Box::new(Node::Constant { value: 3 }),
            }),
        }
    )]
    fn test_parse_expression(
        #[case] tokens: &[SimpleToken],
        #[case] expected: Node,
    ) -> Result<(), SimpleParserError> {
        struct TestLexer {
            tokens: Vec<SimpleToken>,
        }
        impl Lexer<SimpleToken, SimpleLexerError> for TestLexer {
            fn tokenize(&self, _input: &str) -> Result<Vec<SimpleToken>, SimpleLexerError> {
                Ok(self.tokens.clone())
            }
        }
        let test_lexer: Box<dyn Lexer<SimpleToken, SimpleLexerError>> = Box::new(TestLexer {
            tokens: tokens.to_vec(),
        });

        let parser = SimpleParser::new(test_lexer);

        let ast = parser.parse_expression("")?;

        assert_eq!(ast, expected);

        Ok(())
    }
}
