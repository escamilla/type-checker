use crate::parser::Term;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Variable(u32),
}

#[derive(Debug, PartialEq)]
pub enum TypedTerm {
    FunctionApplication {
        ty: Type,
        function: Box<TypedTerm>,
        argument: Box<TypedTerm>,
    },
    FunctionDefinition {
        ty: Type,
        parameter: Box<TypedTerm>,
        body: Box<TypedTerm>,
    },
    Identifier {
        ty: Type,
        name: String,
    },
    IfExpression {
        ty: Type,
        condition: Box<TypedTerm>,
        true_branch: Box<TypedTerm>,
        false_branch: Box<TypedTerm>,
    },
    Integer {
        ty: Type,
        value: i32,
    },
    LetExpression {
        ty: Type,
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
                TypedTerm::FunctionApplication {
                    ty: Type::Variable(order),
                    function: Box::from(typed_function),
                    argument: Box::from(typed_argument),
                },
                typed_argument_order,
            )
        }
        Term::FunctionDefinition { parameter, body } => {
            let (typed_parameter, typed_parameter_order) = annotate_term(parameter, order + 1, env);
            let mut extended_env = env.clone();
            match &typed_parameter {
                TypedTerm::Identifier { ty, name } => {
                    extended_env.insert(name.clone(), ty.clone());
                }
                _ => unimplemented!(),
            }
            let (typed_body, typed_body_order) =
                annotate_term(body, typed_parameter_order + 1, &extended_env);
            (
                TypedTerm::FunctionDefinition {
                    ty: Type::Variable(order),
                    parameter: Box::from(typed_parameter),
                    body: Box::from(typed_body),
                },
                typed_body_order,
            )
        }
        Term::Identifier(name) => match env.get(name) {
            Some(existing_ty) => (
                TypedTerm::Identifier {
                    ty: existing_ty.clone(),
                    name: name.clone(),
                },
                order - 1,
            ),
            None => (
                TypedTerm::Identifier {
                    ty: Type::Variable(order),
                    name: name.clone(),
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
                TypedTerm::IfExpression {
                    ty: Type::Variable(order),
                    condition: Box::from(typed_condition),
                    true_branch: Box::from(typed_true_branch),
                    false_branch: Box::from(typed_false_branch),
                },
                typed_false_branch_order,
            )
        }
        Term::Integer(value) => (
            TypedTerm::Integer {
                ty: Type::Variable(order),
                value: *value,
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
                TypedTerm::LetExpression {
                    ty: Type::Variable(order),
                    declaration_name: Box::from(typed_declaration_name),
                    declaration_value: Box::from(typed_declaration_value),
                    expression: Box::from(typed_expression),
                },
                typed_expression_order,
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::annotator::{annotate, Type, TypedTerm};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;

    #[test]
    fn test_annotate_integer() {
        let tokens = tokenize("42");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        assert_eq!(
            typed_term,
            TypedTerm::Integer {
                ty: Type::Variable(1),
                value: 42,
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
            TypedTerm::Identifier {
                ty: Type::Variable(1),
                name: String::from("x"),
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
            TypedTerm::FunctionDefinition {
                ty: Type::Variable(1),
                parameter: Box::from(TypedTerm::Identifier {
                    ty: Type::Variable(2),
                    name: String::from("x"),
                }),
                body: Box::from(TypedTerm::Identifier {
                    ty: Type::Variable(2),
                    name: String::from("x"),
                }),
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
            TypedTerm::IfExpression {
                ty: Type::Variable(1),
                condition: Box::from(TypedTerm::Identifier {
                    ty: Type::Variable(2),
                    name: String::from("x"),
                }),
                true_branch: Box::from(TypedTerm::Integer {
                    ty: Type::Variable(3),
                    value: 0,
                }),
                false_branch: Box::from(TypedTerm::Integer {
                    ty: Type::Variable(4),
                    value: 1
                }),
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
            TypedTerm::LetExpression {
                ty: Type::Variable(1),
                declaration_name: Box::from(TypedTerm::Identifier {
                    ty: Type::Variable(2),
                    name: String::from("inc"),
                }),
                declaration_value: Box::from(TypedTerm::FunctionDefinition {
                    ty: Type::Variable(3),
                    parameter: Box::from(TypedTerm::Identifier {
                        ty: Type::Variable(4),
                        name: String::from("x"),
                    }),
                    body: Box::from(TypedTerm::FunctionApplication {
                        ty: Type::Variable(5),
                        function: Box::from(TypedTerm::FunctionApplication {
                            ty: Type::Variable(6),
                            function: Box::from(TypedTerm::Identifier {
                                ty: Type::Variable(7),
                                name: String::from("+")
                            }),
                            argument: Box::from(TypedTerm::Identifier {
                                ty: Type::Variable(4),
                                name: String::from("x")
                            })
                        }),
                        argument: Box::from(TypedTerm::Integer {
                            ty: Type::Variable(8),
                            value: 1
                        })
                    }),
                }),
                expression: Box::from(TypedTerm::FunctionApplication {
                    ty: Type::Variable(9),
                    function: Box::from(TypedTerm::Identifier {
                        ty: Type::Variable(10),
                        name: String::from("inc")
                    }),
                    argument: Box::from(TypedTerm::Integer {
                        ty: Type::Variable(11),
                        value: 42,
                    })
                })
            }
        );
    }
}
