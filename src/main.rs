use type_checker::annotator::annotate;
use type_checker::constraint::collect_constraints;
use type_checker::parser::parse;
use type_checker::tokenizer::tokenize;

fn main() -> Result<(), String> {
    let tokens = tokenize("fn x => x + 1");
    let term = parse(&tokens);
    let typed_term = annotate(&term.unwrap())?;
    let constraints = collect_constraints(&typed_term);
    println!("{}", typed_term);
    for constraint in constraints {
        println!("{}", constraint);
    }
    Ok(())
}
