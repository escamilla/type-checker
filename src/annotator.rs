use crate::parser::Term;
use std::collections::HashMap;

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum Type {
    Boolean,
    Function {
        parameter_type: Box<Type>,
        return_type: Box<Type>,
    },
    Integer,
    Variable(u32),
}

#[derive(Debug, PartialEq)]
pub struct TypedTerm {
    pub ty: Type,
    pub kind: TypedTermKind,
}

#[derive(Debug, PartialEq)]
pub enum TypedTermKind {
    FunctionApplication {
        function: Box<TypedTerm>,
        argument: Box<TypedTerm>,
    },
    FunctionDefinition {
        parameter: Box<TypedTerm>,
        body: Box<TypedTerm>,
    },
    Identifier {
        name: String,
    },
    IfExpression {
        condition: Box<TypedTerm>,
        true_branch: Box<TypedTerm>,
        false_branch: Box<TypedTerm>,
    },
    Integer {
        value: i32,
    },
    LetExpression {
        declaration_name: Box<TypedTerm>,
        declaration_value: Box<TypedTerm>,
        expression: Box<TypedTerm>,
    },
}

pub fn annotate(term: &Term) -> TypedTerm {
    let (annotated_term, _) = annotate_term(term, 1, &HashMap::new());
    annotated_term
}

fn annotate_term(term: &Term, order: u32, env: &HashMap<String, Type>) -> (TypedTerm, u32) {
    match term {
        Term::FunctionApplication { function, argument } => {
            let (typed_function, typed_function_order) = annotate_term(function, order + 1, env);
            let (typed_argument, typed_argument_order) =
                annotate_term(argument, typed_function_order + 1, env);
            (
                TypedTerm {
                    ty: Type::Variable(order),
                    kind: TypedTermKind::FunctionApplication {
                        function: Box::from(typed_function),
                        argument: Box::from(typed_argument),
                    },
                },
                typed_argument_order,
            )
        }
        Term::FunctionDefinition { parameter, body } => {
            let (typed_parameter, typed_parameter_order) = annotate_term(parameter, order + 1, env);
            let mut extended_env = env.clone();
            match &typed_parameter.kind {
                TypedTermKind::Identifier { name } => {
                    extended_env.insert(name.clone(), typed_parameter.ty.clone());
                }
                _ => unimplemented!(),
            }
            let (typed_body, typed_body_order) =
                annotate_term(body, typed_parameter_order + 1, &extended_env);
            (
                TypedTerm {
                    ty: Type::Variable(order),
                    kind: TypedTermKind::FunctionDefinition {
                        parameter: Box::from(typed_parameter),
                        body: Box::from(typed_body),
                    },
                },
                typed_body_order,
            )
        }
        Term::Identifier(name) => match env.get(name) {
            Some(existing_ty) => (
                TypedTerm {
                    ty: existing_ty.clone(),
                    kind: TypedTermKind::Identifier { name: name.clone() },
                },
                order - 1,
            ),
            None => (
                TypedTerm {
                    ty: Type::Variable(order),
                    kind: TypedTermKind::Identifier { name: name.clone() },
                },
                order,
            ),
        },
        Term::IfExpression {
            condition,
            true_branch,
            false_branch,
        } => {
            let (typed_condition, typed_condition_order) = annotate_term(condition, order + 1, env);
            let (typed_true_branch, typed_true_branch_order) =
                annotate_term(true_branch, typed_condition_order + 1, env);
            let (typed_false_branch, typed_false_branch_order) =
                annotate_term(false_branch, typed_true_branch_order + 1, env);
            (
                TypedTerm {
                    ty: Type::Variable(order),
                    kind: TypedTermKind::IfExpression {
                        condition: Box::from(typed_condition),
                        true_branch: Box::from(typed_true_branch),
                        false_branch: Box::from(typed_false_branch),
                    },
                },
                typed_false_branch_order,
            )
        }
        Term::Integer(value) => (
            TypedTerm {
                ty: Type::Variable(order),
                kind: TypedTermKind::Integer { value: *value },
            },
            order,
        ),
        Term::LetExpression {
            declaration_name,
            declaration_value,
            expression,
        } => {
            let (typed_declaration_name, typed_declaration_name_order) =
                annotate_term(declaration_name, order + 1, env);
            let (typed_declaration_value, typed_declaration_value_order) =
                annotate_term(declaration_value, typed_declaration_name_order + 1, env);
            let (typed_expression, typed_expression_order) =
                annotate_term(expression, typed_declaration_value_order + 1, env);
            (
                TypedTerm {
                    ty: Type::Variable(order),
                    kind: TypedTermKind::LetExpression {
                        declaration_name: Box::from(typed_declaration_name),
                        declaration_value: Box::from(typed_declaration_value),
                        expression: Box::from(typed_expression),
                    },
                },
                typed_expression_order,
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::annotator::{annotate, Type, TypedTerm, TypedTermKind};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_annotate_integer() {
        let tokens = tokenize("42");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        assert_eq!(
            typed_term,
            TypedTerm {
                ty: Type::Variable(1),
                kind: TypedTermKind::Integer { value: 42 }
            }
        );
    }

    #[test]
    fn test_annotate_identifier() {
        let tokens = tokenize("x");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        assert_eq!(
            typed_term,
            TypedTerm {
                ty: Type::Variable(1),
                kind: TypedTermKind::Identifier {
                    name: String::from("x"),
                }
            }
        );
    }

    #[test]
    fn test_annotate_function_definition() {
        let tokens = tokenize("fn x => x");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        assert_eq!(
            typed_term,
            TypedTerm {
                ty: Type::Variable(1),
                kind: TypedTermKind::FunctionDefinition {
                    parameter: Box::from(TypedTerm {
                        ty: Type::Variable(2),
                        kind: {
                            TypedTermKind::Identifier {
                                name: String::from("x"),
                            }
                        }
                    }),
                    body: Box::from(TypedTerm {
                        ty: Type::Variable(2),
                        kind: {
                            TypedTermKind::Identifier {
                                name: String::from("x"),
                            }
                        }
                    }),
                }
            }
        );
    }

    #[test]
    fn test_annotate_simple_if_expression() {
        let tokens = tokenize("if x then 0 else 1");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        assert_eq!(
            typed_term,
            TypedTerm {
                ty: Type::Variable(1),
                kind: TypedTermKind::IfExpression {
                    condition: Box::from(TypedTerm {
                        ty: Type::Variable(2),
                        kind: {
                            TypedTermKind::Identifier {
                                name: String::from("x"),
                            }
                        }
                    }),
                    true_branch: Box::from(TypedTerm {
                        ty: Type::Variable(3),
                        kind: { TypedTermKind::Integer { value: 0 } }
                    }),
                    false_branch: Box::from(TypedTerm {
                        ty: Type::Variable(4),
                        kind: { TypedTermKind::Integer { value: 1 } }
                    }),
                }
            }
        );
    }

    #[test]
    fn test_annotate_let_expression() {
        let tokens = tokenize("let val inc = fn x => x + 1 in inc 42 end");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        assert_eq!(
            typed_term,
            TypedTerm {
                ty: Type::Variable(1),
                kind: TypedTermKind::LetExpression {
                    declaration_name: Box::from(TypedTerm {
                        ty: Type::Variable(2),
                        kind: TypedTermKind::Identifier {
                            name: String::from("inc"),
                        }
                    }),
                    declaration_value: Box::from(TypedTerm {
                        ty: Type::Variable(3),
                        kind: TypedTermKind::FunctionDefinition {
                            parameter: Box::from(TypedTerm {
                                ty: Type::Variable(4),
                                kind: TypedTermKind::Identifier {
                                    name: String::from("x"),
                                }
                            }),
                            body: Box::from(TypedTerm {
                                ty: Type::Variable(5),
                                kind: TypedTermKind::FunctionApplication {
                                    function: Box::from(TypedTerm {
                                        ty: Type::Variable(6),
                                        kind: TypedTermKind::FunctionApplication {
                                            function: Box::from(TypedTerm {
                                                ty: Type::Variable(7),
                                                kind: TypedTermKind::Identifier {
                                                    name: String::from("+"),
                                                }
                                            }),
                                            argument: Box::from(TypedTerm {
                                                ty: Type::Variable(4),
                                                kind: TypedTermKind::Identifier {
                                                    name: String::from("x"),
                                                }
                                            })
                                        }
                                    }),
                                    argument: Box::from(TypedTerm {
                                        ty: Type::Variable(8),
                                        kind: { TypedTermKind::Integer { value: 1 } }
                                    })
                                }
                            }),
                        }
                    }),
                    expression: Box::from(TypedTerm {
                        ty: Type::Variable(9),
                        kind: TypedTermKind::FunctionApplication {
                            function: Box::from(TypedTerm {
                                ty: Type::Variable(10),
                                kind: TypedTermKind::Identifier {
                                    name: String::from("inc"),
                                }
                            }),
                            argument: Box::from(TypedTerm {
                                ty: Type::Variable(11),
                                kind: { TypedTermKind::Integer { value: 42 } }
                            })
                        }
                    })
                }
            }
        );
    }
}
