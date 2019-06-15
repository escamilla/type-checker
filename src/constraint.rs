use crate::annotator::{Type, TypedTerm, TypedTermKind};
use std::collections::HashMap;
use std::fmt::{Display, Error, Formatter};

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct Constraint {
    type1: Type,
    type2: Type,
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{} = {}", self.type1, self.type2)
    }
}

pub fn collect_constraints(term: &TypedTerm) -> Vec<Constraint> {
    let mut bindings = HashMap::new();
    bindings.insert(
        String::from("+"),
        Type::Function {
            parameter_type: Box::from(Type::Integer),
            return_type: Box::from(Type::Integer),
        },
    );
    bindings.insert(
        String::from("-"),
        Type::Function {
            parameter_type: Box::from(Type::Integer),
            return_type: Box::from(Type::Integer),
        },
    );
    bindings.insert(
        String::from("*"),
        Type::Function {
            parameter_type: Box::from(Type::Integer),
            return_type: Box::from(Type::Integer),
        },
    );
    bindings.insert(
        String::from("/"),
        Type::Function {
            parameter_type: Box::from(Type::Integer),
            return_type: Box::from(Type::Integer),
        },
    );
    bindings.insert(
        String::from("="),
        Type::Function {
            parameter_type: Box::from(Type::Integer),
            return_type: Box::from(Type::Integer),
        },
    );
    collect_constraints_with_bindings(term, &bindings)
}

fn collect_constraints_with_bindings(
    term: &TypedTerm,
    bindings: &HashMap<String, Type>,
) -> Vec<Constraint> {
    match &term.kind {
        TypedTermKind::Boolean(_) => vec![Constraint {
            type1: term.ty.clone(),
            type2: Type::Boolean,
        }],
        TypedTermKind::FunctionApplication { function, argument } => {
            let mut constraints = vec![Constraint {
                type1: function.ty.clone(),
                type2: Type::Function {
                    parameter_type: Box::from(argument.ty.clone()),
                    return_type: Box::from(term.ty.clone()),
                },
            }];
            constraints.extend(collect_constraints_with_bindings(function, bindings));
            constraints.extend(collect_constraints_with_bindings(argument, bindings));
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
            constraints.extend(collect_constraints_with_bindings(body, bindings));
            constraints
        }
        TypedTermKind::Identifier(name) => match bindings.get(name) {
            Some(ty) => vec![Constraint {
                type1: term.ty.clone(),
                type2: ty.clone(),
            }],
            None => vec![],
        },
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
            constraints.extend(collect_constraints_with_bindings(condition, bindings));
            constraints.extend(collect_constraints_with_bindings(true_branch, bindings));
            constraints.extend(collect_constraints_with_bindings(false_branch, bindings));
            constraints
        }
        TypedTermKind::Integer { .. } => vec![Constraint {
            type1: term.ty.clone(),
            type2: Type::Integer,
        }],
        TypedTermKind::LetExpression {
            declaration_name,
            declaration_value,
            expression,
        } => {
            let mut constraints = vec![
                Constraint {
                    type1: term.ty.clone(),
                    type2: expression.ty.clone(),
                },
                Constraint {
                    type1: declaration_name.ty.clone(),
                    type2: declaration_value.ty.clone(),
                },
            ];
            constraints.extend(collect_constraints_with_bindings(
                declaration_value,
                bindings,
            ));
            constraints.extend(collect_constraints_with_bindings(expression, bindings));
            constraints
        }
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
    fn test_collect_constraints_for_identifier() -> Result<(), String> {
        let tokens = tokenize("x")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term);
        assert!(typed_term.is_err());
        Ok(())
    }

    #[test]
    fn test_collect_constraints_for_integer() -> Result<(), String> {
        let tokens = tokenize("42")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term)?;
        let constraints = collect_constraints(&typed_term);
        assert_eq!(
            constraints,
            vec![Constraint {
                // type(42) === integer
                type1: Type::Placeholder(1),
                type2: Type::Integer,
            }]
        );
        Ok(())
    }

    #[test]
    fn test_collect_constraints_for_if_expression() -> Result<(), String> {
        let tokens = tokenize("if true then 1 else 0")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term)?;
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(if x then 1 else 0) === type(1)
                Constraint {
                    type1: Type::Placeholder(1),
                    type2: Type::Placeholder(3),
                },
                // type(if x then 1 else 0) === type(0)
                Constraint {
                    type1: Type::Placeholder(1),
                    type2: Type::Placeholder(4),
                },
                // type(x) === boolean
                Constraint {
                    type1: Type::Placeholder(2),
                    type2: Type::Boolean,
                },
                // type(1) === type(0)
                Constraint {
                    type1: Type::Placeholder(3),
                    type2: Type::Placeholder(4),
                },
                // type(1) === integer
                Constraint {
                    type1: Type::Placeholder(3),
                    type2: Type::Integer,
                },
                // type(0) === integer
                Constraint {
                    type1: Type::Placeholder(4),
                    type2: Type::Integer,
                },
            ])
        );
        Ok(())
    }

    #[test]
    fn test_collect_constraints_for_function_definition() -> Result<(), String> {
        let tokens = tokenize("fn x => x")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term)?;
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(fn x => x) === type(x) -> type(x)
                Constraint {
                    type1: Type::Placeholder(1),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(2)),
                        return_type: Box::from(Type::Placeholder(2))
                    }
                },
            ])
        );
        Ok(())
    }

    #[test]
    fn test_collect_constraints_for_function_definition_with_function_application(
    ) -> Result<(), String> {
        let tokens = tokenize("fn x => x + 1")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term)?;
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(fn x => x + 1) === type(x) -> type(x + 1)
                Constraint {
                    type1: Type::Placeholder(1),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(2)),
                        return_type: Box::from(Type::Placeholder(3))
                    }
                },
                // type(+ x) === type(1) -> type(+ x 1)
                Constraint {
                    type1: Type::Placeholder(4),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(6)),
                        return_type: Box::from(Type::Placeholder(3))
                    }
                },
                // type(+) === type(x) -> type(+ x)
                Constraint {
                    type1: Type::Placeholder(5),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(2)),
                        return_type: Box::from(Type::Placeholder(4))
                    }
                },
                // type(+) === int -> int
                Constraint {
                    type1: Type::Placeholder(5),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Integer),
                        return_type: Box::from(Type::Integer)
                    }
                },
                // type(1) === integer
                Constraint {
                    type1: Type::Placeholder(6),
                    type2: Type::Integer,
                }
            ])
        );
        Ok(())
    }

    #[test]
    fn test_collect_constraints_for_let_expression() -> Result<(), String> {
        let tokens = tokenize("let val inc = fn x => x + 1 in inc 42 end")?;
        let term = parse(&tokens)?;
        let typed_term = annotate(&term)?;
        let constraints: HashSet<Constraint> = HashSet::from_iter(collect_constraints(&typed_term));
        assert_eq!(
            constraints,
            HashSet::from_iter(vec![
                // type(let...end) === type(inc 42)
                Constraint {
                    type1: Type::Placeholder(1),
                    type2: Type::Placeholder(9),
                },
                // type(inc) === type(fn x => x + 1)
                Constraint {
                    type1: Type::Placeholder(2),
                    type2: Type::Placeholder(3),
                },
                // type(fn x => x + 1) === type(x) -> type(+ x 1)
                Constraint {
                    type1: Type::Placeholder(3),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(4)),
                        return_type: Box::from(Type::Placeholder(5))
                    }
                },
                // type(+ x) === type(1) -> type(+ x 1)
                Constraint {
                    type1: Type::Placeholder(6),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(8)),
                        return_type: Box::from(Type::Placeholder(5))
                    }
                },
                // type(+) === type(x) -> type(+ x)
                Constraint {
                    type1: Type::Placeholder(7),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(4)),
                        return_type: Box::from(Type::Placeholder(6))
                    }
                },
                // type(+) === int -> int
                Constraint {
                    type1: Type::Placeholder(7),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Integer),
                        return_type: Box::from(Type::Integer)
                    }
                },
                // type(1) === integer
                Constraint {
                    type1: Type::Placeholder(8),
                    type2: Type::Integer,
                },
                // type(inc) === type(x) -> type(inc x)
                Constraint {
                    type1: Type::Placeholder(2),
                    type2: Type::Function {
                        parameter_type: Box::from(Type::Placeholder(10)),
                        return_type: Box::from(Type::Placeholder(9))
                    }
                },
                // type(42) === integer
                Constraint {
                    type1: Type::Placeholder(10),
                    type2: Type::Integer,
                }
            ])
        );
        Ok(())
    }
}
