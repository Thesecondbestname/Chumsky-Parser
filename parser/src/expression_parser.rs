#[allow(clippy::too_many_lines)]
pub mod expressions {
    use crate::ast::{BinaryOp, ComparisonOp, Expression, MathOp, Number, Value};
    use crate::convenience_parsers::separator;
    use crate::convenience_types::{Error, ParserInput, Spanned};
    use crate::util_parsers::extra_delimited;
    use crate::Token;
    use chumsky::prelude::*;
    pub fn expression_parser<'tokens, 'src: 'tokens, T>(
        block: T,
    ) -> (impl Parser<
        'tokens,
        ParserInput<'tokens, 'src>, // Input
        Spanned<Expression>,        // Output
        Error<'tokens>,             // Error Type
    > + Clone)
    where
        T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>>
            + Clone
            + 'tokens,
    {
        let ident = select! { Token::Ident(ident) => ident }.labelled("Identifier/ Name");
        let int = select! { Token::Integer(v) => v }.labelled("Whole AAh integer");
        let float = select! { Token::Float(v) => v }.labelled("Floating point");
        let number = int
            .map(|int| Expression::Value(Value::Number(Number::Int(int))))
            .or(float.map(|float| Expression::Value(Value::Number(Number::Float(float)))));
        let bool = select! {Token::True => Expression::Value(Value::Bool(true)),
        Token::False => Expression::Value(Value::Bool(false))}
        .labelled("Boolean");
        let string = select! {Token::LiteralString(s) => Expression::Value(Value::String(s))}
            .labelled("String");
        let span = select! {Token::Span(s) => Expression::Value(Value::Span(s.start, s.end))};

        // The recursive expression Part
        recursive(|expression| {
            let inline_expression = {
                // Atom which is the smallest expression.
                let atom = choice((ident.map(Expression::Ident), number, bool, string, span))
                    .map_with(|expr, span| (expr, span.span()))
                    // Atoms can also just be normal expressions, but surrounded with parentheses
                    .or(extra_delimited::<_, Spanned<Expression>>(
                        expression.clone(),
                    ))
                    // Attempt to recover anything that looks like a parenthesised expression but contains errors
                    .recover_with(via_parser(nested_delimiters(
                        Token::Lparen,
                        Token::Rparen,
                        [(Token::Lbracket, Token::Rbracket)],
                        |span| (Expression::ParserError, span),
                    )))
                    .labelled("Atom")
                    .as_context();
                // A list of expressions
                let items = expression
                    .clone()
                    .then_ignore(separator())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .labelled("a list of expressions");

                // A list of expressions delimited by ()
                let list = items
                    .clone()
                    .delimited_by(just(Token::Lparen), just(Token::Rparen))
                    .recover_with(via_parser(nested_delimiters(
                        Token::Lparen,
                        Token::Rparen,
                        [(Token::Lbracket, Token::Rbracket)],
                        |span| vec![(Expression::ParserError, span)],
                    )))
                    .labelled("list of expressions")
                    .as_context();

                // Function calls have very high precedence so we prioritise them
                let call = atom
                    .clone()
                    .foldl(
                        list.clone()
                            .map_with(|expr, span| (expr, span.span()))
                            .repeated(),
                        |func, args| {
                            let span = SimpleSpan::new(func.1.start, args.1.end);
                            (Expression::FunctionCall(Box::new(func), args.0), span)
                        },
                    )
                    .labelled("Function call");
                // TODO: Fix this garbage with atom and call...
                let method_call = choice((atom, call.clone()))
                    .then_ignore(separator())
                    .then_ignore(just(Token::Period))
                    .then(ident)
                    .then(list.clone().or_not())
                    .map_with(|((called_on, name), args), ctx| {
                        (
                            Expression::MethodCall(
                                Box::new(called_on),
                                name,
                                args.map_or_else(std::vec::Vec::new, |arguments| arguments),
                            ),
                            ctx.span(),
                        )
                    })
                    .labelled("method call");

                // Product ops (multiply and divide) have equal precedence
                let op = just(Token::Mul)
                    .to(MathOp::Mul)
                    .or(just(Token::Div).to(MathOp::Div));
                let product = choice((method_call.clone(), call.clone()))
                    .clone()
                    .foldl(op.then(call).repeated(), |a, (op, b)| {
                        let span = a.1.start..b.1.end;
                        (
                            Expression::MathOp(Box::new(a), op, Box::new(b)),
                            span.into(),
                        )
                    })
                    .labelled("product");

                // Sum ops (add and subtract) have equal precedence
                let op = just(Token::Add)
                    .to(MathOp::Add)
                    .or(just(Token::Sub).to(MathOp::Sub));
                let sum = product
                    .clone()
                    .foldl(op.then(product).repeated(), |a, (op, b)| {
                        let span = a.1.start..b.1.end;
                        (
                            Expression::MathOp(Box::new(a), op, Box::new(b)),
                            span.into(),
                        )
                    })
                    .labelled("sum");

                let logical = {
                    let op = select! {
                        Token::And => BinaryOp::And,
                        Token::Or => BinaryOp::Or,
                        Token::Xor => BinaryOp::Xor
                    };
                    sum.clone().foldl(
                        op.then(sum).repeated(),
                        |lhs: Spanned<Expression>, (op, rhs): (_, Spanned<Expression>)| {
                            let span = SimpleSpan::new(lhs.1.start, rhs.1.end);
                            (Expression::Binary(Box::new(lhs), op, Box::new(rhs)), span)
                        },
                    )
                };

                let comp = {
                    let op = select! {
                        Token::Eq => ComparisonOp::Eq,
                        Token::Neq => ComparisonOp::Neq,
                        Token::Gt => ComparisonOp::Gt,
                        Token::Lt => ComparisonOp::Lt,
                    };
                    logical.clone().foldl(
                        op.then(logical).repeated(),
                        |lhs: Spanned<Expression>, (op, rhs): (_, Spanned<Expression>)| {
                            let span = SimpleSpan::new(lhs.1.start, rhs.1.end);
                            (
                                Expression::Comparison(Box::new(lhs), op, Box::new(rhs)),
                                span,
                            )
                        },
                    )
                }; // Comparison ops (equal, not-equal) have equal precedence
                comp.labelled("expression").as_context().boxed()
            };

            let if_else = just(Token::If)
                .ignore_then(block.clone())
                .then(block.clone())
                .then(just(Token::Else).ignore_then(block.clone()).or_not())
                .map(|((cond, then_branch), else_branch)| {
                    Expression::IfElse(
                        Box::new(cond),
                        Box::new(then_branch),
                        else_branch.map_or_else(|| None, |else_| Some(Box::new(else_))),
                    )
                })
                .labelled("if else");
            choice((inline_expression.clone(), block.clone()))
        })
    }
}
// mod pratt {
//     let basic = {
//             use chumsky::pratt::{infix, left, postfix, prefix, right};

//             let binary = |associativity, token, op| {
//                 infix(associativity, just(token), move |l, r| {
//                     Expr::Binary(Box::new(l), op, Box::new(r))
//                 })
//             };

//             // https://doc.rust-lang.org/stable/reference/expressions.html#expression-precedence
//             atom.clone().pratt((
//                 // field ::= atom "." ident
//                 postfix(
//                     8,
//                     just(Token::Dot).ignore_then(ident_parser().map(String::from)),
//                     |l, field| Expr::FieldAccess(Box::new(l), field),
//                 ),
//                 // subscript ::= field "." "[" expr "]"
//                 postfix(
//                     7,
//                     just(Token::Dot).ignore_then(
//                         expr.clone()
//                             .delimited_by(just(Token::BracketOpen), just(Token::BracketClose)),
//                     ),
//                     |l, index| Expr::Subscript(Box::new(l), Box::new(index)),
//                 ),
//                 // call ::= field "(" (expr ("," expr)*)? ")"
//                 postfix(
//                     7,
//                     expr.clone()
//                         .separated_by(just(Token::Comma))
//                         .collect::<Vec<Expr>>()
//                         .delimited_by(just(Token::ParenOpen), just(Token::ParenClose)),
//                     |l, args| Expr::Call(Box::new(l), args),
//                 ),
//                 // unary ::= ("-" | "not") call
//                 prefix(6, just(Token::Not), |r| {
//                     Expr::Unary(UnaryOp::Not, Box::new(r))
//                 }),
//                 prefix(6, just(Token::Minus), |r| {
//                     Expr::Unary(UnaryOp::Neg, Box::new(r))
//                 }),
//                 // mul_div ::= unary ("*" | "/") unary
//                 binary(left(5), Token::Star, BinaryOp::Mul),
//                 binary(left(5), Token::Slash, BinaryOp::Div),
//                 // add_sub ::= mul_div ("+" | "-") mul_div
//                 binary(left(4), Token::Plus, BinaryOp::Add),
//                 binary(left(4), Token::Minus, BinaryOp::Sub),
//                 // TODO Require parentheses
//                 // compare ::= add_sub ("==" | "!=" | "<" | "<=" | ">" | ">=") add_sub
//                 binary(left(3), Token::Eq, BinaryOp::Eq),
//                 binary(left(3), Token::NotEq, BinaryOp::NotEq),
//                 binary(left(3), Token::LessThan, BinaryOp::LessThan),
//                 binary(left(3), Token::LessThanEq, BinaryOp::LessThanEq),
//                 binary(left(3), Token::GreaterThan, BinaryOp::GreaterThan),
//                 binary(left(3), Token::GreaterThanEq, BinaryOp::GreaterThanEq),
//                 // assign ::= basic "=" basic
//                 binary(right(1), Token::Assign, BinaryOp::Assign),
//                 // break ::= "break" basic
//                 prefix(0, just(Token::Break), |r| Expr::Break(Box::new(r))),
//                 // return_value ::= "return" basic
//                 prefix(0, just(Token::Return), |r| Expr::Return(Box::new(r))),
//             ))
//         };

// }
