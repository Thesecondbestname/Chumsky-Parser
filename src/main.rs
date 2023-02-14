use crate::ast::{ExpressionType, Instruction, Number::*, Type, Value};
use ast::Number;
use chumsky::prelude::*;
mod ast;

fn main() {
    let input_verified = r#"use io::print
    x = xöla
    y = 69 / (56 - 0.45)
    print(Works?)
    enum Foo:
        baz
    ;
    struct baz:
        lmao: int,
        lmao2: int
    ;
    int add (int: x, int: y) (
        x + y
    // 
    add (4,5).sqrt
    
    if x == 4  (
        print ("oooops!")
    ) else (
        print ("phew")
    )"#;
    let temp_input = "5";
    println!("{:?}", parser().parse(temp_input));
}
fn parser() -> impl Parser<char, Instruction, Error = Simple<char>> {
    let int = text::int(10)
        .map(|s: String| {
            Instruction::from_expr(
                Value::Number(Number::Int(s.parse().unwrap())).to_Expression(Type::Int), // Parses integers into expressions. This is temporary. TODO!
            )
        })
        .padded();
    let atom = int;
    let operator = |char| just(char).padded();
    let unary = operator('-').repeated().then(atom);
    int.then_ignore(end())
}
