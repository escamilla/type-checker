use crate::tokenizer::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Boolean(bool),
    FunctionApplication {
        function: Box<Term>,
        argument: Box<Term>,
    },
    FunctionDefinition {
        parameter: Box<Term>,
        body: Box<Term>,
    },
    Identifier(String),
    IfExpression {
        condition: Box<Term>,
        true_branch: Box<Term>,
        false_branch: Box<Term>,
    },
    Integer(i32),
    LetExpression {
        declaration_name: Box<Term>,
        declaration_value: Box<Term>,
        expression: Box<Term>,
    },
}

pub fn parse(tokens: &Vec<Token>) -> Result<Term, String> {
    match parse_expression(tokens, 0) {
        Ok((term, _)) => Ok(term),
        Err(message) => Err(message),
    }
}

fn parse_expression(tokens: &Vec<Token>, position: usize) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::Boolean(value) => Ok((Term::Boolean(*value), position + 1)),
            Token::Identifier(name) => {
                if let Some(next_token) = tokens.get(position + 1) {
                    if is_binary_operator(next_token) {
                        parse_binary_operation(tokens, position)
                    } else {
                        match parse_expression(tokens, position + 1) {
                            Ok((argument_term, position)) => Ok((
                                Term::FunctionApplication {
                                    function: Box::from(Term::Identifier(name.clone())),
                                    argument: Box::from(argument_term),
                                },
                                position,
                            )),
                            Err(_) => Ok((Term::Identifier(name.clone()), position + 1)),
                        }
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
            Token::KeywordIf => parse_if_expression(tokens, position),
            Token::KeywordLet => parse_let_expression(tokens, position),
            _ => Err(format!(
                "expected `fn` keyword, `if` keyword, identifier, or integer but got {:?}",
                token,
            )),
        }
    } else {
        Err(String::from(
            "expected `fn` keyword, `if` keyword, identifier, or integer but got nothing",
        ))
    }
}

fn parse_declaration_clause(
    tokens: &Vec<Token>,
    position: usize,
) -> Result<(Term, Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::KeywordVal => match parse_identifier(tokens, position + 1) {
                Ok((val_name_term, position)) => {
                    if let Some(token) = tokens.get(position) {
                        match token {
                            Token::Equals => match parse_expression(tokens, position + 1) {
                                Ok((val_value_term, position)) => {
                                    Ok((val_name_term, val_value_term, position))
                                }
                                Err(message) => Err(message),
                            },
                            _ => Err(format!("expected `=` but got {:?}", token)),
                        }
                    } else {
                        Err(String::from("expected `=` but got nothing"))
                    }
                }
                Err(message) => Err(message),
            },
            _ => Err(format!("expected `val` keyword but got {:?}", token)),
        }
    } else {
        Err(String::from("expected `val` keyword but got nothing"))
    }
}

fn parse_let_expression(tokens: &Vec<Token>, position: usize) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::KeywordLet => match parse_declaration_clause(tokens, position + 1) {
                Ok((declaration_name_term, declaration_value_term, position)) => {
                    if let Some(token) = tokens.get(position) {
                        match token {
                            Token::KeywordIn => match parse_expression(tokens, position + 1) {
                                Ok((expression_term, position)) => {
                                    if let Some(token) = tokens.get(position) {
                                        match token {
                                            Token::KeywordEnd => Ok((
                                                Term::LetExpression {
                                                    declaration_name: Box::from(
                                                        declaration_name_term,
                                                    ),
                                                    declaration_value: Box::from(
                                                        declaration_value_term,
                                                    ),
                                                    expression: Box::from(expression_term),
                                                },
                                                position,
                                            )),
                                            _ => Err(format!(
                                                "expected `end` keyword but got {:?}",
                                                token
                                            )),
                                        }
                                    } else {
                                        Err(String::from("expected `end` keyword but got nothing"))
                                    }
                                }
                                Err(message) => Err(message),
                            },
                            _ => Err(format!("expected `in` keyword but got {:?}", token)),
                        }
                    } else {
                        Err(String::from("expected `in` keyword but got nothing"))
                    }
                }
                Err(message) => Err(message),
            },
            _ => Err(format!("expected `let` keyword but got {:?}", token)),
        }
    } else {
        Err(String::from("expected `let` keyword but got nothing"))
    }
}

fn parse_function_definition(
    tokens: &Vec<Token>,
    position: usize,
) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::KeywordFn => match parse_identifier(tokens, position + 1) {
                Ok((parameter_term, position)) => {
                    if let Some(token) = tokens.get(position) {
                        match token {
                            Token::Arrow => match parse_expression(tokens, position + 1) {
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

fn parse_if_expression(tokens: &Vec<Token>, position: usize) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::KeywordIf => match parse_expression(tokens, position + 1) {
                Ok((test_condition_term, position)) => {
                    if let Some(token) = tokens.get(position) {
                        match token {
                            Token::KeywordThen => match parse_expression(tokens, position + 1) {
                                Ok((true_branch_term, position)) => {
                                    if let Some(token) = tokens.get(position) {
                                        match token {
                                            Token::KeywordElse => {
                                                match parse_expression(tokens, position + 1) {
                                                    Ok((false_branch_term, position)) => Ok((
                                                        Term::IfExpression {
                                                            condition: Box::from(
                                                                test_condition_term,
                                                            ),
                                                            true_branch: Box::from(
                                                                true_branch_term,
                                                            ),
                                                            false_branch: Box::from(
                                                                false_branch_term,
                                                            ),
                                                        },
                                                        position,
                                                    )),
                                                    Err(message) => Err(message),
                                                }
                                            }
                                            _ => Err(format!(
                                                "expected `else` keyword but got {:?}",
                                                token
                                            )),
                                        }
                                    } else {
                                        Err(String::from("expected `else` keyword but got nothing"))
                                    }
                                }
                                Err(message) => Err(message),
                            },
                            _ => Err(format!("expected `then` keyword but got {:?}", token)),
                        }
                    } else {
                        Err(String::from("expected `then` keyword but got nothing"))
                    }
                }
                Err(message) => Err(message),
            },
            _ => Err(format!("expected `if` keyword but got {:?}", token)),
        }
    } else {
        Err(String::from("expected `if` keyword but got nothing"))
    }
}

fn parse_identifier(tokens: &Vec<Token>, position: usize) -> Result<(Term, usize), String> {
    if let Some(token) = tokens.get(position) {
        match token {
            Token::Identifier(name) => Ok((Term::Identifier(name.clone()), position + 1)),
            _ => Err(format!("expected identifier but got {:?}", token)),
        }
    } else {
        Err(String::from("expected identifier but got nothing"))
    }
}

fn is_binary_operator(token: &Token) -> bool {
    match token {
        Token::Plus | Token::Minus | Token::Times | Token::Divide | Token::Equals => true,
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
                            Term::FunctionApplication {
                                function: Box::from(Term::FunctionApplication {
                                    function: Box::from(Term::Identifier(match middle_token {
                                        Token::Plus => String::from("+"),
                                        Token::Minus => String::from("-"),
                                        Token::Times => String::from("*"),
                                        Token::Divide => String::from("/"),
                                        Token::Equals => String::from("="),
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
    use crate::parser::{parse, parse_declaration_clause, Term};
    use crate::tokenizer::tokenize;

    #[test]
    fn test_parse_integer() {
        let tokens = tokenize("1");
        assert_eq!(parse(&tokens), Ok(Term::Integer(1)));
    }

    #[test]
    fn test_parse_identifier() {
        let tokens = tokenize("x");
        assert_eq!(parse(&tokens), Ok(Term::Identifier(String::from("x"))));
    }

    #[test]
    fn test_parse_boolean_true() {
        let tokens = tokenize("true");
        assert_eq!(parse(&tokens), Ok(Term::Boolean(true)));
    }

    #[test]
    fn test_parse_boolean_false() {
        let tokens = tokenize("false");
        assert_eq!(parse(&tokens), Ok(Term::Boolean(false)));
    }

    #[test]
    fn test_parse_addition() {
        let tokens = tokenize("x + 1");
        assert_eq!(
            parse(&tokens),
            Ok(Term::FunctionApplication {
                function: Box::from(Term::FunctionApplication {
                    function: Box::from(Term::Identifier(String::from("+"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(1))
            })
        );
    }

    #[test]
    fn test_parse_subtraction() {
        let tokens = tokenize("x - 1");
        assert_eq!(
            parse(&tokens),
            Ok(Term::FunctionApplication {
                function: Box::from(Term::FunctionApplication {
                    function: Box::from(Term::Identifier(String::from("-"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(1))
            })
        );
    }

    #[test]
    fn test_parse_multiplication() {
        let tokens = tokenize("x * 2");
        assert_eq!(
            parse(&tokens),
            Ok(Term::FunctionApplication {
                function: Box::from(Term::FunctionApplication {
                    function: Box::from(Term::Identifier(String::from("*"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(2))
            })
        );
    }

    #[test]
    fn test_parse_division() {
        let tokens = tokenize("x / 2");
        assert_eq!(
            parse(&tokens),
            Ok(Term::FunctionApplication {
                function: Box::from(Term::FunctionApplication {
                    function: Box::from(Term::Identifier(String::from("/"))),
                    argument: Box::from(Term::Identifier(String::from("x")))
                }),
                argument: Box::from(Term::Integer(2))
            })
        );
    }

    #[test]
    fn test_parse_identity_function() {
        let tokens = tokenize("fn x => x");
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
        let tokens = tokenize("fn x => x + 1");
        assert_eq!(
            parse(&tokens),
            Ok(Term::FunctionDefinition {
                parameter: Box::from(Term::Identifier(String::from("x"))),
                body: Box::from(Term::FunctionApplication {
                    function: Box::from(Term::FunctionApplication {
                        function: Box::from(Term::Identifier(String::from("+"))),
                        argument: Box::from(Term::Identifier(String::from("x")))
                    }),
                    argument: Box::from(Term::Integer(1))
                })
            })
        );
    }

    #[test]
    fn test_parse_if_expression() {
        let tokens = tokenize("if x = y then 0 else 1");
        assert_eq!(
            parse(&tokens),
            Ok(Term::IfExpression {
                condition: Box::from(Term::FunctionApplication {
                    function: Box::from(Term::FunctionApplication {
                        function: Box::from(Term::Identifier(String::from("="))),
                        argument: Box::from(Term::Identifier(String::from("x")))
                    }),
                    argument: Box::from(Term::Identifier(String::from("y")))
                }),
                true_branch: Box::from(Term::Integer(0)),
                false_branch: Box::from(Term::Integer(1)),
            })
        );
    }

    #[test]
    fn test_parse_declaration_clause() {
        let tokens = tokenize("val inc = fn x => x + 1");
        assert_eq!(
            parse_declaration_clause(&tokens, 0),
            Ok((
                Term::Identifier(String::from("inc")),
                Term::FunctionDefinition {
                    parameter: Box::from(Term::Identifier(String::from("x"))),
                    body: Box::from(Term::FunctionApplication {
                        function: Box::from(Term::FunctionApplication {
                            function: Box::from(Term::Identifier(String::from("+"))),
                            argument: Box::from(Term::Identifier(String::from("x")))
                        }),
                        argument: Box::from(Term::Integer(1))
                    }),
                },
                tokens.len()
            )),
        );
    }

    #[test]
    fn test_parse_let_expression() {
        let tokens = tokenize("let val inc = fn x => x + 1 in inc 42 end");
        assert_eq!(
            parse(&tokens),
            Ok(Term::LetExpression {
                declaration_name: Box::from(Term::Identifier(String::from("inc"))),
                declaration_value: Box::from(Term::FunctionDefinition {
                    parameter: Box::from(Term::Identifier(String::from("x"))),
                    body: Box::from(Term::FunctionApplication {
                        function: Box::from(Term::FunctionApplication {
                            function: Box::from(Term::Identifier(String::from("+"))),
                            argument: Box::from(Term::Identifier(String::from("x")))
                        }),
                        argument: Box::from(Term::Integer(1))
                    }),
                }),
                expression: Box::from(Term::FunctionApplication {
                    function: Box::from(Term::Identifier(String::from("inc"))),
                    argument: Box::from(Term::Integer(42))
                })
            })
        );
    }
}
