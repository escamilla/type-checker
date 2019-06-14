use crate::annotator::{Type, TypedTerm, TypedTermKind};

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Constraint {
    type1: Type,
    type2: Type,
}

pub fn collect_constraints(term: &TypedTerm) -> Vec<Constraint> {
    match &term.kind {
        TypedTermKind::FunctionApplication { function, argument } => {
            let mut constraints = vec![Constraint {
                type1: function.ty.clone(),
                type2: Type::Function {
                    parameter_type: Box::from(argument.ty.clone()),
                    return_type: Box::from(term.ty.clone()),
                },
            }];
            constraints.extend(collect_constraints(function));
            constraints.extend(collect_constraints(argument));
            constraints
        }
        TypedTermKind::FunctionDefinition { parameter, body } => {
            let mut constraints = vec![Constraint {
                type1: term.ty.clone(),
                type2: Type::Function {
                    parameter_type: Box::from(parameter.ty.clone()),
                    return_type: Box::from(body.ty.clone()),
                },
            }];
            constraints.extend(collect_constraints(body));
            constraints
        }
        TypedTermKind::Identifier { name: _ } => vec![],
        TypedTermKind::IfExpression {
            condition,
            true_branch,
            false_branch,
        } => {
            let mut constraints = vec![
                Constraint {
                    type1: term.ty.clone(),
                    type2: true_branch.ty.clone(),
                },
                Constraint {
                    type1: term.ty.clone(),
                    type2: false_branch.ty.clone(),
                },
                Constraint {
                    type1: condition.ty.clone(),
                    type2: Type::Boolean,
                },
                Constraint {
                    type1: true_branch.ty.clone(),
                    type2: false_branch.ty.clone(),
                },
            ];
            constraints.extend(collect_constraints(condition));
            constraints.extend(collect_constraints(true_branch));
            constraints.extend(collect_constraints(false_branch));
            constraints
        }
        TypedTermKind::Integer { value: _ } => vec![Constraint {
            type1: term.ty.clone(),
            type2: Type::Integer,
        }],
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::annotator::{annotate, Type};
    use crate::constraint::{collect_constraints, Constraint};
    use crate::parser::parse;
    use crate::tokenizer::tokenize;
    use std::collections::HashSet;
    use std::iter::FromIterator;

    #[test]
    fn test_collect_constraints_for_identifier() {
        let tokens = tokenize("x");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        let constraints = collect_constraints(&typed_term);
        assert_eq!(constraints, vec![]);
    }

    #[test]
    fn test_collect_constraints_for_integer() {
        let tokens = tokenize("42");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        let constraints = collect_constraints(&typed_term);
        assert_eq!(
            constraints,
            vec![Constraint {
                // type(42) === integer
                type1: Type::Variable(1),
                type2: Type::Integer,
            }]
        );
    }

    #[test]
    fn test_collect_constraints_for_if_expression() {
        let tokens = tokenize("if x then 1 else 0");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(if x then 1 else 0) === type(1)
                Constraint {
                    type1: Type::Variable(1),
                    type2: Type::Variable(3),
                },
                // type(if x then 1 else 0) === type(0)
                Constraint {
                    type1: Type::Variable(1),
                    type2: Type::Variable(4),
                },
                // type(x) === boolean
                Constraint {
                    type1: Type::Variable(2),
                    type2: Type::Boolean,
                },
                // type(1) === type(0)
                Constraint {
                    type1: Type::Variable(3),
                    type2: Type::Variable(4),
                },
                // type(1) === integer
                Constraint {
                    type1: Type::Variable(3),
                    type2: Type::Integer,
                },
                // type(0) === integer
                Constraint {
                    type1: Type::Variable(4),
                    type2: Type::Integer,
                },
            ])
        );
    }

    #[test]
    fn test_collect_constraints_for_function_definition() {
        let tokens = tokenize("fn x => x");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(fn x => x) === type(x) -> type(x)
                Constraint {
                    type1: Type::Variable(1),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Variable(2)),
                        return_type: Box::from(Type::Variable(2))
                    }
                },
            ])
        );
    }

    #[test]
    fn test_collect_constraints_for_function_application() {
        let tokens = tokenize("inc x");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(inc) === type(x) -> type(inc x)
                Constraint {
                    type1: Type::Variable(2),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Variable(3)),
                        return_type: Box::from(Type::Variable(1))
                    }
                },
            ])
        );
    }

    #[test]
    fn test_collect_constraints_for_function_definition_with_function_application() {
        let tokens = tokenize("fn x => x + 1");
        let term = parse(&tokens);
        let typed_term = annotate(&term.unwrap());
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(fn x => x + 1) === type(x) -> type(x + 1)
                Constraint {
                    type1: Type::Variable(1),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Variable(2)),
                        return_type: Box::from(Type::Variable(3))
                    }
                },
                // type(+ x) === type(1) -> type(+ x 1)
                Constraint {
                    type1: Type::Variable(4),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Variable(6)),
                        return_type: Box::from(Type::Variable(3))
                    }
                },
                // type(+) === type(x) -> type(+ x)
                Constraint {
                    type1: Type::Variable(5),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Variable(2)),
                        return_type: Box::from(Type::Variable(4))
                    }
                },
                // type(1) === integer
                Constraint {
                    type1: Type::Variable(6),
                    type2: Type::Integer,
                }
            ])
        );
    }
}
