use crate::ast::{Type, Value};
use crate::Number::Int;
use crate::Type::Float as Float_type;
use crate::Type::Int as Int_type;
use ast::{i32_to_f32, Expression, ExpressionType, Number, Variable, ToExpression, Instruction, Statement};
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
    let quit = false;
    while !quit {
        let mut temp_input = String::new();
        io::stdin()
            .read_line(&mut temp_input)
            .expect("Failed to read line");
        if temp_input == "q" {
            return;
        }
        let parsed = parser().parse(temp_input);
        println!("{:?}", parsed)
    }
}
fn parser() -> impl Parser<char, Expression, Error = Simple<char>> {
    let ident = text::ident()
                .padded();
    let expression = recursive(|expression| {
        
        let int = text::int(10)
            .map(|s: String| Value::Number(Int(s.parse().unwrap())).to_Expression(Int_type))
            .padded();
        
        let float = text::int(10)
            .separated_by(just('.'))
            .allow_leading()
            .allow_trailing()
            .at_most(2)
            .at_least(1)
            .map(|float| -> Expression{
                let nums: _ = float
                    .to_owned()
                    .iter()
                    .map(|num| -> i32 {if let Ok(n) = num.parse() { n } else { 0 }})
                    .collect::<Vec<i32>>();
                
                    Value::Number(Number::Float(i32_to_f32(nums[0], nums[1])))
                        .to_Expression(Float_type)
                });

        let number = float.or(int);
        
        let atom = number.or(expression.delimited_by(just("("), just(")")))
            .or(ident.map(|var| -> Expression { ExpressionType::Variable(var).to_Expression(Type::Inferred) }));
        let operator = |char| just(char).padded();
        let unary = operator('-')
            .repeated()
            .then(atom)
            .foldr(|_operator, rhs| rhs.to_UnaryMathExpression());

        let product = unary
            .clone()
            .then(
                operator('*')
                    .to(ExpressionType::Mul as fn(_, _) -> ExpressionType)
                    .or(operator('/').to(ExpressionType::Div as fn(_, _) -> ExpressionType))
                    .then(unary.clone())
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| -> Expression {
                op(Box::new(lhs), Box::new(rhs)).to_Expression(Type::Int)
            });

        let addition = product
            .clone()
            .then(
                operator('-')
                    .to(ExpressionType::Sub as fn(_, _) -> ExpressionType)
                    .or(operator('+').to(ExpressionType::Add as fn(_, _) -> ExpressionType))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| -> Expression {
                op(Box::new(lhs), Box::new(rhs)).to_Expression(Type::Int)
            });
        addition
    });
    let decl = recursive(|_decl| {
        let assignment = ident
            .then_ignore(just('='))
            .then(expression.clone())
            .then_ignore(text::newline())
            .map(|(name, rhs)|{
                let var = Statement::VariableDeclaration(Variable { 
                    name, 
                    value: Box::new(rhs.clone()), 
                    r#type: rhs.return_type.clone()
                });
                var.to_Expression(Type::Inferred)
            });
        assignment.or(expression).padded()
    });
    decl.then_ignore(end())
}

#[cfg(test)]
mod tests;
