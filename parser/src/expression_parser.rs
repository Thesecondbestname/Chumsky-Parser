#[allow(clippy::too_many_lines)]
pub mod expressions {
    use crate::ast::{BinaryOp, ComparisonOp, Expression, MathOp, Number, Value};
    use crate::convenience_types::{Error, ParserInput, Spanned};
    use crate::util_parsers::extra_delimited;
    use crate::Token;
    use chumsky::pratt::{infix, left, postfix, prefix};
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
                    .or(block.clone().labelled("ExpressionBlock").as_context())
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
                    .separated_by(just(Token::Comma)
 // .delimited_by(just(Token::Newline).or_not().ignored(),just(Token::Newline).or_not().ignored())
                    )                     
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
                let method_call = call
                    .clone()
                    .foldl(
                        just(Token::Period)
                            .ignore_then(ident)
                            .then(list.clone().or_not())
                            .repeated(),
                        |func: Spanned<Expression>, (name, args): (String, Option<Vec<Spanned<Expression>>>)| {
                            let spanend = 
                                if args.clone().is_some_and(|x| x.last().is_some()) {
                                    args.clone().unwrap().last().unwrap().1.end
                                } else {
                                    func.1.end
                                };

                            let span = func.1.start..spanend;
                            (
                                Expression::MethodCall(
                                    Box::new(func),
                                    name,
                                    args.map_or_else(std::vec::Vec::new, |arguments| arguments),
                                ),
                                span.into(),
                            )
                        },
                    )
                    .labelled("method call");

                // Product ops (multiply and divide) have equal precedence
                let op = just(Token::Mul)
                    .to(MathOp::Mul)
                    .or(just(Token::Div).to(MathOp::Div));
                let product = method_call
                    .clone()
                    .foldl(op.then(method_call).repeated(), |a, (op, b)| {
                        let span = a.1.start..b.1.end;
                        (
                            Expression::MathOp(Box::new(a), op, Box::new(b)),
                            span.into(),
                        )
                    })
                    .labelled("product");

                // Sum ops (add and subtract) have equal precedence
                let op = just(Token::Plus)
                    .to(MathOp::Add)
                    .or(just(Token::Minus).to(MathOp::Sub));
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
                        Token::Gte => ComparisonOp::Gt,
                        Token::Lte => ComparisonOp::Lt,
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
                comp.labelled("Atom").as_context().boxed()
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
            inline_expression.clone()
        })
    }
    fn atom_parser<'tokens, 'src: 'tokens, T>(
        atom: T,
        expr: T,
    ) -> impl Parser<
        'tokens,
        ParserInput<'tokens, 'src>, // Input
        Spanned<Expression>,        // Output
        Error<'tokens>,             // Error Type
    > + Clone
    where
        T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>>
            + Clone
            + 'tokens,
    {
        let binary_math = |associativity, token, op| {
            infix::<_, _, MathOp, Spanned<Expression>>(
                associativity,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(token),
                move |l: Spanned<Expression>, r: Spanned<Expression>| -> Spanned<Expression> {
                    (
                        Expression::MathOp(Box::new(l.clone()), op, Box::new(r.clone())),
                        (l.start()..r.end()).into(),
                    )
                },
            )
        };
        let binary_comp = |associativity, token, op| {
            infix::<_, _, ComparisonOp, Spanned<Expression>>(
                associativity,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(token),
                move |l: Spanned<Expression>, r: Spanned<Expression>| {
                    (
                        Expression::Comparison(Box::new(l.clone()), op, Box::new(r.clone())),
                        (l.start()..r.end()),
                    )
                },
            )
        };
        let binary = |associativity, token: Token, op| {
            infix::<_, _, BinaryOp, Spanned<Expression>>(
                associativity,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(token),
                move |l, r| -> Expression { Expression::Binary(Box::new(l), op, Box::new(r)) },
            )
        };
        let atom2 = atom.clone();
        // https://doc.rust-lang.org/stable/reference/expressions.html#expression-precedence
        let atom = atom.clone().pratt((
            // field ::= atom "." ident
            postfix::<_, _, Token, Spanned<Expression>>(
                8,
                just(Token::Period)
                    .ignore_then(crate::util_parsers::ident_parser().map(String::from))
                    .map_with(|field, ctx| (field, ctx.span())),
                |l: Spanned<Expression>, field: Spanned<String>| {
                    (
                        Expression::FieldAccess(Box::new(l.clone()), field.0.clone()),
                        l.start()..field.end(),
                    )
                },
            ),
            // call ::= field "(" (expr ("," expr)*)? ")"
            postfix::<_, _, Token, Spanned<Expression>>(
                7,
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<Spanned<Expression>>>()
                    .map_with(|args, ctx| (args, ctx.span()))
                    .delimited_by(just(Token::Lparen), just(Token::Rparen)),
                |l: Spanned<Expression>, args: Spanned<Vec<Spanned<Expression>>>| {
                    (
                        Expression::FunctionCall(Box::new(l.clone()), args.0.clone()),
                        l.start()..args.end(),
                    )
                },
            ),
            // unary ::= ("-" | "not") call
            prefix::<_, _, Token, Spanned<Expression>>(
                6,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(Token::Bang).to_span(),
                |(span, r): (SimpleSpan, Spanned<Expression>)| {
                    (
                        Expression::UnaryBool(Box::new(r.clone())),
                        span.start()..r.end(),
                    )
                },
            ),
            prefix::<_, _, Token, Spanned<Expression>>(
                6,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(Token::Minus).to_span(),
                |(span, r): (SimpleSpan, Spanned<Expression>)| {
                    (
                        Expression::UnaryMath(Box::new(r.clone())),
                        span.start()..r.end(),
                    )
                },
            ),
            // mul_div ::= unary ("*" | "/") unary
            binary_math(left(5), Token::Mul, MathOp::Mul),
            binary_math(left(5), Token::Div, MathOp::Div),
            // add_sub ::= mul_div ("+" | "-") mul_div
            binary_math(left(4), Token::Plus, MathOp::Add),
            binary_math(left(4), Token::Minus, MathOp::Sub),
            // TODO Require parentheses
            // compare ::= add_sub ("==" | "!=" | "<" | "<=" | ">" | ">=") add_sub
            binary_comp(left(3), Token::Eq, ComparisonOp::Eq),
            binary_comp(left(3), Token::Neq, ComparisonOp::Neq),
            binary_comp(left(3), Token::Lt, ComparisonOp::Lt),
            binary_comp(left(3), Token::Lte, ComparisonOp::Lte),
            binary_comp(left(3), Token::Gt, ComparisonOp::Gt),
            binary_comp(left(3), Token::Gte, ComparisonOp::Gte),
            // additional comparisons
            binary(left(2), Token::And, BinaryOp::And),
            binary(left(2), Token::Or, BinaryOp::Or),
            binary(left(2), Token::Xor, BinaryOp::Xor),
            // // break ::= "break" basic
            // prefix(0, just(Token::Break), |r| Expression::Break(Box::new(r))),
            // // return_value ::= "return" basic
            // prefix(0, just(Token::Return), |r| Expression::Return(Box::new(r))),
        ));
        atom2
    }
}

// mod pratt {

// }
