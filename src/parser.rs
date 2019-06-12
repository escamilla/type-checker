use crate::tokenizer::{Token, Tokenizer};

#[derive(Debug, PartialEq)]
pub enum Term {
    Application {
        function: Box<Term>,
        argument: Box<Term>,
    },
    FunctionDefinition {
        parameter: Box<Term>,
        body: Box<Term>,
    },
    Identifier(String),
    Integer(i32),
}

pub fn parse(tokens: &Vec<Token>) -> Result<Term, String> {
    match parse_expression(tokens, 0) {
        Ok((term, position)) => Ok(term),
        Err(message) => Err(message),
    }
}

fn parse_expression(tokens: &Vec<Token>, position: usize) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::Identifier(name) => {
                if let Some(next_token) = tokens.get(position + 1) {
                    if is_binary_operator(next_token) {
                        parse_binary_operation(tokens, position)
                    } else {
                        Ok((Term::Identifier(name.clone()), position + 1))
                    }
                } else {
                    Ok((Term::Identifier(name.clone()), position + 1))
                }
            }
            Token::Integer(value) => {
                if let Some(next_token) = tokens.get(position + 1) {
                    if is_binary_operator(next_token) {
                        parse_binary_operation(tokens, position)
                    } else {
                        Ok((Term::Integer(*value), position + 1))
                    }
                } else {
                    Ok((Term::Integer(*value), position + 1))
                }
            }
            Token::KeywordFn => parse_function_definition(tokens, position),
            _ => Err(format!(
                "expected identifier, integer, or `fn` keyword but got {:?}",
                token,
            )),
        }
    } else {
        Err(String::from(
            "expected identifier, integer, or `fn` keyword but got nothing",
        ))
    }
}

fn parse_function_definition(
    tokens: &Vec<Token>,
    position: usize,
) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::KeywordFn => match parse_identifier(tokens, position + 1) {
                Ok((parameter_term, _)) => {
                    if let Some(token) = tokens.get(position + 2) {
                        match token {
                            Token::Arrow => match parse_expression(tokens, position + 3) {
                                Ok((body_term, position)) => Ok((
                                    Term::FunctionDefinition {
                                        parameter: Box::from(parameter_term),
                                        body: Box::from(body_term),
                                    },
                                    position,
                                )),
                                Err(message) => Err(message),
                            },
                            _ => Err(format!(
                                "expected `=>` after `fn` keyword and function parameter but got {:?}",
                                token
                            )),
                        }
                    } else {
                        Err(String::from(
                            "expected `=>` after `fn` keyword and function parameter but got nothing",
                        ))
                    }
                }
                Err(message) => Err(message),
            },
            _ => Err(format!("expected `fn` keyword but got {:?}", token)),
        }
    } else {
        Err(String::from("expected `fn` keyword but got nothing"))
    }
}

fn parse_identifier(tokens: &Vec<Token>, position: usize) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::Identifier(name) => Ok((Term::Identifier(name.clone()), position + 1)),
            _ => Err(format!("expected identifier but got {:?}", token)),
        }
    } else {
        Err(format!("expected identifier but got nothing"))
    }
}

fn is_binary_operator(token: &Token) -> bool {
    match token {
        Token::Plus | Token::Minus | Token::Times | Token::Divide => true,
        _ => false,
    }
}

fn parse_binary_operation(tokens: &Vec<Token>, position: usize) -> Result<(Term, usize), String> {
    match parse_integer_or_identifier(tokens, position) {
        Ok((left_term, position)) => {
            if let Some(middle_token) = tokens.get(position) {
                if is_binary_operator(middle_token) {
                    match parse_integer_or_identifier(tokens, position + 1) {
                        Ok((right_term, position)) => Ok((
                            Term::Application {
                                function: Box::from(Term::Application {
                                    function: Box::from(Term::Identifier(match middle_token {
                                        Token::Plus => String::from("+"),
                                        Token::Minus => String::from("-"),
                                        Token::Times => String::from("*"),
                                        Token::Divide => String::from("/"),
                                        _ => unimplemented!(),
                                    })),
                                    argument: Box::from(left_term),
                                }),
                                argument: Box::from(right_term),
                            },
                            position,
                        )),
                        Err(message) => Err(message),
                    }
                } else {
                    Err(format!(
                        "expected binary operator but got {:?}",
                        middle_token
                    ))
                }
            } else {
                Err(String::from("expected binary operator but got nothing"))
            }
        }
        Err(message) => Err(message),
    }
}

fn parse_integer_or_identifier(
    tokens: &Vec<Token>,
    position: usize,
) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::Integer(value) => Ok((Term::Integer(*value), position + 1)),
            Token::Identifier(name) => Ok((Term::Identifier(name.clone()), position + 1)),
            _ => Err(format!(
                "expected integer or identifier but got {:?}",
                token
            )),
        }
    } else {
        Err(String::from(
            "expected integer or identifier but got nothing",
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{parse, Term};
    use crate::tokenizer::Tokenizer;

    #[test]
    fn test_parse_integer() {
        let mut tokenizer = Tokenizer::new("1");
        let tokens = tokenizer.tokenize();
        assert_eq!(parse(&tokens), Ok(Term::Integer(1)));
    }

    #[test]
    fn test_parse_identifier() {
        let mut tokenizer = Tokenizer::new("x");
        let tokens = tokenizer.tokenize();
        assert_eq!(parse(&tokens), Ok(Term::Identifier(String::from("x"))));
    }

    #[test]
    fn test_parse_addition() {
        let mut tokenizer = Tokenizer::new("x + 1");
        let tokens = tokenizer.tokenize();

        assert_eq!(
            parse(&tokens),
            Ok(Term::Application {
                function: Box::from(Term::Application {
                    function: Box::from(Term::Identifier(String::from("+"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(1))
            })
        );
    }

    #[test]
    fn test_parse_subtraction() {
        let mut tokenizer = Tokenizer::new("x - 1");
        let tokens = tokenizer.tokenize();

        assert_eq!(
            parse(&tokens),
            Ok(Term::Application {
                function: Box::from(Term::Application {
                    function: Box::from(Term::Identifier(String::from("-"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(1))
            })
        );
    }

    #[test]
    fn test_parse_multiplication() {
        let mut tokenizer = Tokenizer::new("x * 2");
        let tokens = tokenizer.tokenize();

        assert_eq!(
            parse(&tokens),
            Ok(Term::Application {
                function: Box::from(Term::Application {
                    function: Box::from(Term::Identifier(String::from("*"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(2))
            })
        );
    }

    #[test]
    fn test_parse_division() {
        let mut tokenizer = Tokenizer::new("x / 2");
        let tokens = tokenizer.tokenize();

        assert_eq!(
            parse(&tokens),
            Ok(Term::Application {
                function: Box::from(Term::Application {
                    function: Box::from(Term::Identifier(String::from("/"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(2))
            })
        );
    }

    #[test]
    fn test_parse_identity_function() {
        let mut tokenizer = Tokenizer::new("fn x => x");
        let tokens = tokenizer.tokenize();

        assert_eq!(
            parse(&tokens),
            Ok(Term::FunctionDefinition {
                parameter: Box::from(Term::Identifier(String::from("x"))),
                body: Box::from(Term::Identifier(String::from("x")))
            })
        );
    }

    #[test]
    fn test_parse_increment_function() {
        let mut tokenizer = Tokenizer::new("fn x => x + 1");
        let tokens = tokenizer.tokenize();

        assert_eq!(
            parse(&tokens),
            Ok(Term::FunctionDefinition {
                parameter: Box::from(Term::Identifier(String::from("x"))),
                body: Box::from(Term::Application {
                    function: Box::from(Term::Application {
                        function: Box::from(Term::Identifier(String::from("+"))),
                        argument: Box::from(Term::Identifier(String::from("x")))
                    }),
                    argument: Box::from(Term::Integer(1))
                })
            })
        );
    }
}
