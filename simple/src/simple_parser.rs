use crate::simple_lexer::{SimpleLexerError, SimpleToken};
use itertools::Itertools;
use lexing::lexer::Lexer;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Peekable;

pub struct SimpleParser {
    lexer: Box<dyn Lexer<SimpleToken, SimpleLexerError>>,
}

pub enum SimpleParserError {
    UnexpectedToken {
        unexpected: SimpleToken,
        expected: Vec<SimpleToken>,
        near_tokens: Vec<SimpleToken>,
    },
    UnexpectedEndOfInput,
    LexerError(SimpleLexerError),
}

impl Display for SimpleParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SimpleParserError::UnexpectedToken {
                unexpected,
                expected,
                near_tokens,
            } => {
                let expected = expected.iter().map(|e| format!("{}", e)).join(", ");
                let near = [unexpected.clone()]
                    .iter()
                    .chain(near_tokens)
                    .map(|e| format!("{}", e))
                    .join(", ");
                write!(
                    f,
                    "Unexpected: {}. Expected: [{}]. Near: [{}]",
                    unexpected, expected, near
                )
            }
            SimpleParserError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
            SimpleParserError::LexerError(err) => write!(f, "Lexer error: {}", err),
        }
    }
}

impl Error for SimpleParserError {}

impl Debug for SimpleParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Times,
}

impl Debug for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Operator::Plus => {
                write!(f, "+")
            }
            Operator::Minus => {
                write!(f, "-")
            }
            Operator::Times => {
                write!(f, "*")
            }
        }
    }
}

#[derive(PartialEq)]
pub enum Node {
    Program {
        procedures: Vec<Box<Node>>,
    },
    Procedure {
        name: String,
        body: Box<Node>,
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

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Node::Program { procedures } => {
                write!(f, "Program {:?}", procedures)
            }
            Node::Procedure { name, body } => {
                write!(f, "Procedure: {} {:?}", name, body)
            }
            Node::StatementList { statements } => {
                write!(f, "{:?}", statements)
            }
            Node::Assign {
                line,
                variable,
                expression,
                ..
            } => {
                write!(f, "{}. {} = {:?};", line, variable, expression)
            }
            Node::While {
                line,
                variable,
                statements,
                ..
            } => {
                write!(f, "{}. while {} {:?}", line, variable, statements)
            }
            Node::If {
                line,
                variable,
                if_statements,
                else_statements,
            } => {
                write!(
                    f,
                    "{}. if {} then {:?} else {:?}",
                    line, variable, if_statements, else_statements
                )
            }
            Node::Expression { .. } => {
                write!(f, "<expression>")
            }
            Node::Reference { name } => {
                write!(f, "{}", name)
            }
            Node::Constant { value } => {
                write!(f, "{}", value)
            }
            Node::Call { line, name } => {
                write!(f, "{}. call {}", line, name)
            }
        }
    }
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
        let mut line = 1;
        while let Some(_) = tokens.peek() {
            if tokens.peek().unwrap() == &SimpleToken::Eof {
                break;
            }
            procedures.push(Box::new(procedure(&mut tokens, &mut line)?));
        }

        Ok(Node::Program { procedures })
    }
}

fn expect_token(
    tokens: &mut impl Iterator<Item = SimpleToken>,
    expected: SimpleToken,
) -> Result<(), SimpleParserError> {
    let token = tokens
        .next()
        .ok_or(SimpleParserError::UnexpectedEndOfInput)?;

    match token == expected {
        true => Ok(()),
        false => Err(SimpleParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![expected],
            near_tokens: tokens.take(6).collect(),
        }),
    }
}

fn procedure(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: &mut u32,
) -> Result<Node, SimpleParserError> {
    let name = match tokens.next() {
        Some(SimpleToken::Procedure(name)) => name,
        _ => return Err(SimpleParserError::UnexpectedEndOfInput),
    };

    let body = Box::new(statement_list(tokens, line)?);

    Ok(Node::Procedure { name, body })
}

fn statement_list(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: &mut u32,
) -> Result<Node, SimpleParserError> {
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
        *line += 1;
    }

    Ok(Node::StatementList { statements: body })
}

fn statement(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: &mut u32,
) -> Result<Node, SimpleParserError> {
    match tokens.peek() {
        Some(SimpleToken::Reference(_)) => assign(tokens, *line),
        Some(SimpleToken::While(_)) => while_statement(tokens, line),
        Some(SimpleToken::If(_)) => if_statement(tokens, line),
        _ => Err(SimpleParserError::UnexpectedToken {
            unexpected: tokens.next().unwrap_or(SimpleToken::Eof),
            expected: vec![
                SimpleToken::Reference("_".to_string()),
                SimpleToken::While("".to_string()),
                SimpleToken::If("".to_string()),
            ],
            near_tokens: tokens.take(6).collect(),
        }),
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

fn while_statement(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: &mut u32,
) -> Result<Node, SimpleParserError> {
    let variable = match tokens.next() {
        Some(SimpleToken::While(name)) => name,
        _ => return Err(SimpleParserError::UnexpectedEndOfInput),
    };
    let while_line = *line;
    *line += 1;

    let statements = Box::new(statement_list(tokens, line)?);

    Ok(Node::While {
        line: while_line,
        variable,
        statements,
    })
}

fn if_statement(
    tokens: &mut Peekable<impl Iterator<Item = SimpleToken>>,
    line: &mut u32,
) -> Result<Node, SimpleParserError> {
    let variable = match tokens.next() {
        Some(SimpleToken::If(name)) => name,
        _ => return Err(SimpleParserError::UnexpectedEndOfInput),
    };
    let if_line = *line;
    *line += 1;

    let if_statements = Box::new(statement_list(tokens, line)?);

    expect_token(tokens, SimpleToken::Else)?;

    let else_statements = Box::new(statement_list(tokens, line)?);

    Ok(Node::If {
        line: if_line,
        variable,
        if_statements,
        else_statements,
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
        _ => Err(SimpleParserError::UnexpectedToken {
            unexpected: token,
            expected: vec![
                SimpleToken::Number(0),
                SimpleToken::LeftParenthesis,
                SimpleToken::Reference("_".to_string()),
            ],
            near_tokens: tokens.take(6).collect(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    struct TestLexer {
        tokens: Vec<SimpleToken>,
    }
    impl Lexer<SimpleToken, SimpleLexerError> for TestLexer {
        fn tokenize(&self, _input: &str) -> Result<Vec<SimpleToken>, SimpleLexerError> {
            Ok(self.tokens.clone())
        }
    }

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
        let test_lexer: Box<dyn Lexer<SimpleToken, SimpleLexerError>> = Box::new(TestLexer {
            tokens: tokens.to_vec(),
        });

        let parser = SimpleParser::new(test_lexer);

        let ast = parser.parse_expression("")?;

        assert_eq!(ast, expected);

        Ok(())
    }

    #[rstest]
    #[case::empty_program(
        &[SimpleToken::Eof],
        Node::Program { procedures: vec![] }
    )]
    #[case::single_procedure(
        &[
            SimpleToken::Procedure("main".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::RightBrace,
            SimpleToken::Eof
        ],
        Node::Program {
            procedures: vec![Box::new(Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList { statements: vec![] }),
            })]
        }
    )]
    #[case::multiple_procedures(
        &[
            SimpleToken::Procedure("main".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::RightBrace,
            SimpleToken::Procedure("foo".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::RightBrace,
            SimpleToken::Eof
        ],
        Node::Program {
            procedures: vec![
                Box::new(Node::Procedure {
                    name: "main".to_string(),
                    body: Box::new(Node::StatementList { statements: vec![] }),
                }),
                Box::new(Node::Procedure {
                    name: "foo".to_string(),
                    body: Box::new(Node::StatementList { statements: vec![] }),
                }),
            ]
        }
    )]
    #[case::procedure_with_assignment(
        &[
            SimpleToken::Procedure("main".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::Reference("x".to_string()),
            SimpleToken::Equal,
            SimpleToken::Number(1),
            SimpleToken::SemiColon,
            SimpleToken::RightBrace,
            SimpleToken::Eof
        ],
        Node::Program {
            procedures: vec![Box::new(Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![
                        Box::new(Node::Assign {
                            line: 1,
                            variable: "x".to_string(),
                            expression: Box::new(Node::Constant { value: 1 }),
                        }),
                    ]
                }),
            })]
        }
    )]
    #[case::procedure_with_while_with_assigment(
        &[
            SimpleToken::Procedure("main".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::While("x".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::Reference("x".to_string()),
            SimpleToken::Equal,
            SimpleToken::Number(1),
            SimpleToken::SemiColon,
            SimpleToken::RightBrace,
            SimpleToken::RightBrace,
            SimpleToken::Eof
        ],
        Node::Program {
            procedures: vec![Box::new(Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![
                        Box::new(Node::While {
                            line: 1,
                            variable: "x".to_string(),
                            statements: Box::new(Node::StatementList { statements: vec![
                                Box::new(Node::Assign {
                                    line: 2,
                                    variable: "x".to_string(),
                                    expression: Box::new(Node::Constant { value: 1 }),
                                }),
                            ] }),
                        }),
                    ]
                }),
            })]
        }
    )]
    #[case::procedure_with_if_assignment(
        &[
            SimpleToken::Procedure("main".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::If("x".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::Reference("x".to_string()),
            SimpleToken::Equal,
            SimpleToken::Number(1),
            SimpleToken::SemiColon,
            SimpleToken::RightBrace,
            SimpleToken::Else,
            SimpleToken::LeftBrace,
            SimpleToken::Reference("x".to_string()),
            SimpleToken::Equal,
            SimpleToken::Number(2),
            SimpleToken::SemiColon,
            SimpleToken::RightBrace,
            SimpleToken::RightBrace,
            SimpleToken::Eof
        ],
        Node::Program {
            procedures: vec![Box::new(Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![
                        Box::new(Node::If {
                            line: 1,
                            variable: "x".to_string(),
                            if_statements: Box::new(Node::StatementList { statements: vec![
                                Box::new(Node::Assign {
                                    line: 2,
                                    variable: "x".to_string(),
                                    expression: Box::new(Node::Constant { value: 1 }),
                                }),
                            ] }),
                            else_statements: Box::new(Node::StatementList { statements: vec![
                                Box::new(Node::Assign {
                                    line: 3,
                                    variable: "x".to_string(),
                                    expression: Box::new(Node::Constant { value: 2 }),
                                }),
                            ] }),
                        }),
                    ]
                }),
            })]
        }
    )]
    #[case::procedure_with_while_with_if_with_assignment(
        &[
            SimpleToken::Procedure("main".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::While("x".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::If("y".to_string()),
            SimpleToken::LeftBrace,
            SimpleToken::Reference("y".to_string()),
            SimpleToken::Equal,
            SimpleToken::Number(1),
            SimpleToken::SemiColon,
            SimpleToken::RightBrace,
            SimpleToken::Else,
            SimpleToken::LeftBrace,
            SimpleToken::Reference("y".to_string()),
            SimpleToken::Equal,
            SimpleToken::Number(2),
            SimpleToken::SemiColon,
            SimpleToken::RightBrace,
            SimpleToken::RightBrace,
            SimpleToken::RightBrace,
            SimpleToken::Eof
        ],
        Node::Program {
            procedures: vec![Box::new(Node::Procedure {
                name: "main".to_string(),
                body: Box::new(Node::StatementList {
                    statements: vec![
                        Box::new(Node::While {
                            line: 1,
                            variable: "x".to_string(),
                            statements: Box::new(Node::StatementList { statements: vec![
                                Box::new(Node::If {
                                    line: 2,
                                    variable: "y".to_string(),
                                    if_statements: Box::new(Node::StatementList { statements: vec![
                                        Box::new(Node::Assign {
                                            line: 3,
                                            variable: "y".to_string(),
                                            expression: Box::new(Node::Constant { value: 1 }),
                                        }),
                                    ] }),
                                    else_statements: Box::new(Node::StatementList { statements: vec![
                                        Box::new(Node::Assign {
                                            line: 4,
                                            variable: "y".to_string(),
                                            expression: Box::new(Node::Constant { value: 2 }),
                                        }),
                                    ] }),
                                }),
                            ] }),
                        }),
                    ]
                }),
            })]
        }
    )]
    fn test_parse_program(
        #[case] tokens: &[SimpleToken],
        #[case] expected: Node,
    ) -> Result<(), SimpleParserError> {
        let lexer = Box::new(TestLexer {
            tokens: tokens.to_vec(),
        });
        let parser = SimpleParser::new(lexer);

        let ast = parser.parse_program("")?;

        assert_eq!(ast, expected);

        Ok(())
    }
}
