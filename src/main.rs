use crate::ast::{Type, Value};
use ast::{Expression, Number, MathExpression, ExpressionType};
use chumsky::prelude::*;
mod ast;
use std::io;

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
    let mut temp_input = String::new();
    io::stdin()
        .read_line(&mut temp_input)
        .expect("Failed to read line");
    println!("{:?}", parser().parse(temp_input));
}
fn parser() -> impl Parser<char, Expression, Error = Simple<char>> {
    let int = text::int(10)
        .map(|s: String| {
            Value::Number(Number::Int(s.parse().unwrap())).to_Expression(Type::Int)
            // Parses integers into expressions. This is temporary. TODO!
        })
        .padded();
    let atom = int;
    let operator = |char| just(char).padded();
    let unary = operator('-')
        .repeated()
        .then(atom)
        .foldr(|_operator, rhs| rhs.to_UnaryMathExpression());
    let product = unary.clone()
        .then(operator('*').to(ExpressionType::Mul as fn(_, _) -> ExpressionType)
            .or(operator('/').to(ExpressionType::Div as fn(_, _) -> ExpressionType))
            .then(unary)
            .repeated())
        .foldl(|lhs, (op, rhs)| -> Expression {op(Box::new(lhs), Box::new(rhs)).to_Expression(Type::Int)});
    let addition = unary.clone()
        .then(operator('-').to(ExpressionType::Sub as fn(_, _) -> ExpressionType)
            .or(operator('+').to(ExpressionType::Add as fn(_, _) -> ExpressionType))
            .then(product)
            .repeated())
        .foldl(|lhs, (op, rhs)| -> Expression {op(Box::new(lhs), Box::new(rhs)).to_Expression(Type::Int)});

    addition.then_ignore(end())
    
}
