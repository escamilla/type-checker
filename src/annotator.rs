use crate::parser::Term;
use std::collections::HashMap;
use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Type {
    Boolean,
    Function {
        parameter_type: Box<Type>,
        return_type: Box<Type>,
    },
    Integer,
    Placeholder(u32),
}

#[derive(Debug, PartialEq)]
pub struct TypedTerm {
    pub ty: Type,
    pub kind: TypedTermKind,
}

#[derive(Debug, PartialEq)]
pub enum TypedTermKind {
    Boolean(bool),
    FunctionApplication {
        function: Box<TypedTerm>,
        argument: Box<TypedTerm>,
    },
    FunctionDefinition {
        parameter: Box<TypedTerm>,
        body: Box<TypedTerm>,
    },
    Identifier(String),
    IfExpression {
        condition: Box<TypedTerm>,
        true_branch: Box<TypedTerm>,
        false_branch: Box<TypedTerm>,
    },
    Integer(i32),
    LetExpression {
        declaration_name: Box<TypedTerm>,
        declaration_value: Box<TypedTerm>,
        expression: Box<TypedTerm>,
    },
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            Type::Boolean => write!(f, "bool"),
            Type::Function {
                parameter_type,
                return_type,
            } => write!(f, "{} => {}", parameter_type, return_type),
            Type::Integer => write!(f, "int"),
            Type::Placeholder(counter) => write!(f, "t{}", counter),
        }
    }
}

impl Display for TypedTerm {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<{}>{}</{}>", self.ty, self.kind, self.ty)
    }
}

impl Display for TypedTermKind {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            TypedTermKind::Boolean(value) => write!(f, "{}", value),
            TypedTermKind::FunctionApplication { function, argument } => {
                write!(f, "{}({})", function, argument)
            }
            TypedTermKind::FunctionDefinition { parameter, body } => {
                write!(f, "fn {} => {}", parameter, body)
            }
            TypedTermKind::Identifier(name) => write!(f, "{}", name),
            TypedTermKind::IfExpression {
                condition,
                true_branch,
                false_branch,
            } => write!(
                f,
                "if {} then {} else {}",
                condition, true_branch, false_branch
            ),
            TypedTermKind::Integer(value) => write!(f, "{}", value),
            TypedTermKind::LetExpression {
                declaration_name,
                declaration_value,
                expression,
            } => write!(
                f,
                "let val {} = {} in {} end",
                declaration_name, declaration_value, expression
            ),
        }
    }
}

pub fn annotate(term: &Term) -> Result<TypedTerm, String> {
    let (annotated_term, _) = annotate_term(term, 1, &HashMap::new())?;
    Ok(annotated_term)
}

fn annotate_term(
    term: &Term,
    counter: u32,
    env: &HashMap<String, Type>,
) -> Result<(TypedTerm, u32), String> {
    match term {
        Term::Boolean(value) => Ok((
            TypedTerm {
                ty: Type::Placeholder(counter),
                kind: TypedTermKind::Boolean(*value),
            },
            counter,
        )),
        Term::FunctionApplication { function, argument } => {
            let (typed_function, typed_function_counter) =
                annotate_term(function, counter + 1, env)?;
            let (typed_argument, typed_argument_counter) =
                annotate_term(argument, typed_function_counter + 1, env)?;
            Ok((
                TypedTerm {
                    ty: Type::Placeholder(counter),
                    kind: TypedTermKind::FunctionApplication {
                        function: Box::from(typed_function),
                        argument: Box::from(typed_argument),
                    },
                },
                typed_argument_counter,
            ))
        }
        Term::FunctionDefinition { parameter, body } => {
            let mut extended_env = env.clone();
            if let Term::Identifier(name) = *parameter.clone() {
                extended_env.insert(name.clone(), Type::Placeholder(counter + 1));
            }
            let (typed_parameter, typed_parameter_counter) =
                annotate_term(parameter, counter + 2, &extended_env)?;
            let (typed_body, typed_body_counter) =
                annotate_term(body, typed_parameter_counter + 1, &extended_env)?;
            Ok((
                TypedTerm {
                    ty: Type::Placeholder(counter),
                    kind: TypedTermKind::FunctionDefinition {
                        parameter: Box::from(typed_parameter),
                        body: Box::from(typed_body),
                    },
                },
                typed_body_counter,
            ))
        }
        Term::Identifier(name) => match env.get(name) {
            Some(existing_ty) => Ok((
                TypedTerm {
                    ty: existing_ty.clone(),
                    kind: TypedTermKind::Identifier(name.clone()),
                },
                counter - 1,
            )),
            None => match name.as_ref() {
                "+" | "-" | "*" | "/" => Ok((
                    TypedTerm {
                        ty: Type::Placeholder(counter),
                        kind: TypedTermKind::Identifier(name.clone()),
                    },
                    counter,
                )),
                _ => Err(format!("unbound identifier: {}", name)),
            },
        },
        Term::IfExpression {
            condition,
            true_branch,
            false_branch,
        } => {
            let (typed_condition, typed_condition_counter) =
                annotate_term(condition, counter + 1, env)?;
            let (typed_true_branch, typed_true_branch_counter) =
                annotate_term(true_branch, typed_condition_counter + 1, env)?;
            let (typed_false_branch, typed_false_branch_counter) =
                annotate_term(false_branch, typed_true_branch_counter + 1, env)?;
            Ok((
                TypedTerm {
                    ty: Type::Placeholder(counter),
                    kind: TypedTermKind::IfExpression {
                        condition: Box::from(typed_condition),
                        true_branch: Box::from(typed_true_branch),
                        false_branch: Box::from(typed_false_branch),
                    },
                },
                typed_false_branch_counter,
            ))
        }
        Term::Integer(value) => Ok((
            TypedTerm {
                ty: Type::Placeholder(counter),
                kind: TypedTermKind::Integer(*value),
            },
            counter,
        )),
        Term::LetExpression {
            declaration_name,
            declaration_value,
            expression,
        } => {
            let mut extended_env = env.clone();
            if let Term::Identifier(name) = *declaration_name.clone() {
                extended_env.insert(name.clone(), Type::Placeholder(counter + 1));
            }
            let (typed_declaration_name, typed_declaration_name_counter) =
                annotate_term(declaration_name, counter + 2, &extended_env)?;
            let (typed_declaration_value, typed_declaration_value_counter) =
                annotate_term(declaration_value, typed_declaration_name_counter + 1, env)?;
            let (typed_expression, typed_expression_counter) = annotate_term(
                expression,
                typed_declaration_value_counter + 1,
                &extended_env,
            )?;
            Ok((
                TypedTerm {
                    ty: Type::Placeholder(counter),
                    kind: TypedTermKind::LetExpression {
                        declaration_name: Box::from(typed_declaration_name),
                        declaration_value: Box::from(typed_declaration_value),
                        expression: Box::from(typed_expression),
                    },
                },
                typed_expression_counter,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::annotator::{annotate, Type, TypedTerm, TypedTermKind};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_annotate_integer() -> Result<(), String> {
        let tokens = tokenize("42")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert_eq!(
            typed_term,
            Ok(TypedTerm {
                ty: Type::Placeholder(1),
                kind: TypedTermKind::Integer(42)
            })
        );
        Ok(())
    }

    #[test]
    fn test_annotate_boolean_true() -> Result<(), String> {
        let tokens = tokenize("true")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert_eq!(
            typed_term,
            Ok(TypedTerm {
                ty: Type::Placeholder(1),
                kind: TypedTermKind::Boolean(true)
            })
        );
        Ok(())
    }

    #[test]
    fn test_annotate_boolean_false() -> Result<(), String> {
        let tokens = tokenize("false")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert_eq!(
            typed_term,
            Ok(TypedTerm {
                ty: Type::Placeholder(1),
                kind: TypedTermKind::Boolean(false)
            })
        );
        Ok(())
    }

    #[test]
    fn test_annotate_unbound_identifier() -> Result<(), String> {
        let tokens = tokenize("x")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert!(typed_term.is_err());
        Ok(())
    }

    #[test]
    fn test_annotate_function_definition() -> Result<(), String> {
        let tokens = tokenize("fn x => x")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert_eq!(
            typed_term,
            Ok(TypedTerm {
                ty: Type::Placeholder(1),
                kind: TypedTermKind::FunctionDefinition {
                    parameter: Box::from(TypedTerm {
                        ty: Type::Placeholder(2),
                        kind: TypedTermKind::Identifier(String::from("x"))
                    }),
                    body: Box::from(TypedTerm {
                        ty: Type::Placeholder(2),
                        kind: TypedTermKind::Identifier(String::from("x"))
                    }),
                }
            })
        );
        Ok(())
    }

    #[test]
    fn test_annotate_if_expression() -> Result<(), String> {
        let tokens = tokenize("if true then 0 else 1")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert_eq!(
            typed_term,
            Ok(TypedTerm {
                ty: Type::Placeholder(1),
                kind: TypedTermKind::IfExpression {
                    condition: Box::from(TypedTerm {
                        ty: Type::Placeholder(2),
                        kind: TypedTermKind::Boolean(true)
                    }),
                    true_branch: Box::from(TypedTerm {
                        ty: Type::Placeholder(3),
                        kind: TypedTermKind::Integer(0)
                    }),
                    false_branch: Box::from(TypedTerm {
                        ty: Type::Placeholder(4),
                        kind: TypedTermKind::Integer(1)
                    }),
                }
            })
        );
        Ok(())
    }

    #[test]
    fn test_annotate_let_expression() -> Result<(), String> {
        let tokens = tokenize("let val inc = fn x => x + 1 in inc(42) end")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert_eq!(
            typed_term,
            Ok(TypedTerm {
                ty: Type::Placeholder(1),
                kind: TypedTermKind::LetExpression {
                    declaration_name: Box::from(TypedTerm {
                        ty: Type::Placeholder(2),
                        kind: TypedTermKind::Identifier(String::from("inc"))
                    }),
                    declaration_value: Box::from(TypedTerm {
                        ty: Type::Placeholder(3),
                        kind: TypedTermKind::FunctionDefinition {
                            parameter: Box::from(TypedTerm {
                                ty: Type::Placeholder(4),
                                kind: TypedTermKind::Identifier(String::from("x"))
                            }),
                            body: Box::from(TypedTerm {
                                ty: Type::Placeholder(5),
                                kind: TypedTermKind::FunctionApplication {
                                    function: Box::from(TypedTerm {
                                        ty: Type::Placeholder(6),
                                        kind: TypedTermKind::FunctionApplication {
                                            function: Box::from(TypedTerm {
                                                ty: Type::Placeholder(7),
                                                kind: TypedTermKind::Identifier(String::from("+"))
                                            }),
                                            argument: Box::from(TypedTerm {
                                                ty: Type::Placeholder(4),
                                                kind: TypedTermKind::Identifier(String::from("x"))
                                            })
                                        }
                                    }),
                                    argument: Box::from(TypedTerm {
                                        ty: Type::Placeholder(8),
                                        kind: TypedTermKind::Integer(1)
                                    })
                                }
                            }),
                        }
                    }),
                    expression: Box::from(TypedTerm {
                        ty: Type::Placeholder(9),
                        kind: TypedTermKind::FunctionApplication {
                            function: Box::from(TypedTerm {
                                ty: Type::Placeholder(2),
                                kind: TypedTermKind::Identifier(String::from("inc"))
                            }),
                            argument: Box::from(TypedTerm {
                                ty: Type::Placeholder(10),
                                kind: TypedTermKind::Integer(42)
                            })
                        }
                    })
                }
            })
        );
        Ok(())
    }
}
