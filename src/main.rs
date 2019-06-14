use type_checker::annotator::annotate;
use type_checker::constraint::collect_constraints;
use type_checker::parser::parse;
use type_checker::tokenizer::tokenize;

fn main() {
    //    let tokens = tokenize("let val inc = fn x => x + 1 in inc 42 end");
    let tokens = tokenize("fn isZero => if isZero 1 then 2 else 3");
    let term = parse(&tokens);
    let typed_term = annotate(&term.unwrap());
    let constraints = collect_constraints(&typed_term);
    println!("{}", typed_term);
    for constraint in constraints {
        println!("{}", constraint);
    }
}

/*
type1 = type2 => type3
type3 = type6
type3 = type7
type4 = bool
type6 = type7
type2 = type5 => type4
type5 = int
type6 = int
type7 = int
*/
