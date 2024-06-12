pub trait Lexer<TToken, TError> {
    fn tokenize(&self, input: &str) -> Result<Vec<TToken>, TError>;
}

pub mod basic_lexer {
    use super::Lexer;

    #[derive(Default)]
    pub struct BasicLexer;

    #[derive(Debug, PartialEq)]
    pub enum Token {
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
        Word(String),
    }

    #[derive(Debug)]
    pub enum LexerError {
        UnexpectedCharacter(char),
    }

    impl Lexer<Token, LexerError> for BasicLexer {
        fn tokenize(&self, input: &str) -> Result<Vec<Token>, LexerError> {
            let mut iter = input.chars().peekable();
            let mut tokens = Vec::new();

            while let Some(ch) = iter.next() {
                match ch {
                    '+' => tokens.push(Token::Plus),
                    '-' => tokens.push(Token::Minus),
                    '*' => tokens.push(Token::Multiply),
                    '(' => tokens.push(Token::LeftParenthesis),
                    ')' => tokens.push(Token::RightParenthesis),
                    '{' => tokens.push(Token::LeftBrace),
                    '}' => tokens.push(Token::RightBrace),
                    '=' => tokens.push(Token::Equal),
                    ';' => tokens.push(Token::SemiColon),
                    '0'..='9' => {
                        let mut number = ch.to_digit(10).unwrap() as i32;
                        while let Some(ch) = iter.peek() {
                            if let Some(digit) = ch.to_digit(10) {
                                number = number * 10 + digit as i32;
                            } else {
                                break;
                            }

                            iter.next();
                        }
                        tokens.push(Token::Number(number));
                    }
                    _ => {
                        if ch.is_whitespace() {
                            continue;
                        }

                        if ch.is_alphabetic() {
                            let mut word = ch.to_string();

                            while let Some(ch) = iter.peek() {
                                if ch.is_alphabetic() || ch.is_numeric() {
                                    word.push(*ch);
                                    iter.next();
                                } else {
                                    break;
                                }
                            }

                            tokens.push(Token::Word(word));

                            continue;
                        }

                        return Err(LexerError::UnexpectedCharacter(ch));
                    }
                }
            }

            tokens.push(Token::Eof);

            Ok(tokens)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::basic_lexer::{BasicLexer, Token};
    use super::Lexer;
    use rstest::rstest;

    #[rstest]
    #[case::addition("1 + 2", &[Token::Number(1), Token::Plus, Token::Number(2), Token::Eof])]
    #[case::subtraction("1 - 2", &[Token::Number(1), Token::Minus, Token::Number(2), Token::Eof])]
    #[case::multiplication("1 * 2", &[Token::Number(1), Token::Multiply, Token::Number(2), Token::Eof])]
    #[case::parenthesis("1 * (2 + 3)", &[Token::Number(1), Token::Multiply, Token::LeftParenthesis, Token::Number(2), Token::Plus, Token::Number(3), Token::RightParenthesis, Token::Eof])]
    #[case::brace("1 * {2 + 3}", &[Token::Number(1), Token::Multiply, Token::LeftBrace, Token::Number(2), Token::Plus, Token::Number(3), Token::RightBrace, Token::Eof])]
    #[case::equal("1 = 2", &[Token::Number(1), Token::Equal, Token::Number(2), Token::Eof])]
    #[case::semicolon("1; 2", &[Token::Number(1), Token::SemiColon, Token::Number(2), Token::Eof])]
    #[case::word("foo bar", &[Token::Word("foo".to_string()), Token::Word("bar".to_string()), Token::Eof])]
    #[case::expression("1+2", &[Token::Number(1), Token::Plus, Token::Number(2), Token::Eof])]
    #[case::assignment("a = 1", &[Token::Word("a".to_string()), Token::Equal, Token::Number(1), Token::Eof])]
    #[case::assignment("a=1", &[Token::Word("a".to_string()), Token::Equal, Token::Number(1), Token::Eof])]
    #[case::assignment("a =1", &[Token::Word("a".to_string()), Token::Equal, Token::Number(1), Token::Eof])]
    #[case::assignment("a= 1", &[Token::Word("a".to_string()), Token::Equal, Token::Number(1), Token::Eof])]
    fn test_basic_lexer(#[case] input: &'static str, #[case] output: &[Token]) {
        let lexer = BasicLexer::default();
        let tokens = lexer.tokenize(input).unwrap();

        assert_eq!(tokens, output);
    }
}
