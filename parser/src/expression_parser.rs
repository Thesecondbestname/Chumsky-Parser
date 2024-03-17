#[allow(clippy::too_many_lines)]
pub mod expressions {
    use crate::ast::{BinaryOp, ComparisonOp, Expression, If, MathOp, Number, Value};
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
        let delim_block = extra_delimited(block.clone());

        // The recursive expression Part
        recursive(|expression| {
            let inline_expression = {
                // Atom which is the smallest expression.
                let atom = choice((ident.map(Expression::Ident), number, bool, string, span))
                    .map_with(|expr, span| (expr, span.span()))
                    // Atoms can also just be normal expressions, but surrounded with parentheses
                    .or(delim_block.clone().labelled("ExpressionBlock").as_context())
                    // .or(extra_delimited::<_, Spanned<Expression>>(
                    //     expression.clone(),
                    // ))
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
                            .map_with(|expr, span| {
                                (expr, span.span())
                            })
                            .repeated(),
                        |func, args| {
                            let span = SimpleSpan::new(func.1.start, args.1.end);
                            (Expression::FunctionCall(Box::new(func), args.0), span)
                        },
                    )
                    .labelled("Function call");
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

                let else_expression = sum.clone().foldl(
                    just(Token::Else)
                    .ignore_then(block.clone()).repeated(),
                    |expr, else_branch|{
                        let span = expr.1.start()..else_branch.1.end();
                        (
                            Expression::Else(
                                Box::new(expr),
                                Box::new(else_branch),
                            ), 
                            span.into()
                        )
                    }
                )
                    .labelled("if else");

                let logical = {
                    let op = select! {
                        Token::And => BinaryOp::And,
                        Token::Or => BinaryOp::Or,
                        Token::Xor => BinaryOp::Xor
                    };
                    else_expression.clone().foldl(
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
            // if => "if" expr block
            let if_ = just(Token::If)
                .ignore_then(expression.clone())
                .recover_with(via_parser(nested_delimiters(
                    Token::If,
                    Token::Lparen,
                    [
                        (Token::Lparen, Token::Rparen),
                        (Token::Lbracket, Token::Rbracket),
                        (Token::Lparen, Token::Semicolon),
                    ],
                    |span| (Expression::ParserError, span),
                )))
                .then(block.clone())
                .map_with(|(condition, code_block), ctx| {
                    (Expression::If(
                        Box::new(If {
                            condition: Box::new(condition),
                            code_block,
                        }),
                    ),
                        ctx.span())
                })
                .labelled("if *expression*")
                .as_context();
            choice((inline_expression, if_, extra_delimited(expression)))
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
            infix::<_, _, Token, Spanned<Expression>>(
                associativity,
                just::<Token, ParserInput, Error>(token).boxed(),
                move |l, r,ctx: &'static mut chumsky::input::MapExtra<'_, '_, ParserInput, Error>| -> Spanned<Expression> {
                    (
                        Expression::MathOp(Box::new(l), op, Box::new(r)),
                        ctx.span()
                    )
                },
            )
        };
        let binary_comp = |associativity, token: Token, op| {
            infix::<_, _, ComparisonOp, Spanned<Expression>>(
                associativity,
                just::<_, ParserInput, Error>(token),
                move |l, r, ctx: &'static mut chumsky::input::MapExtra<'_, '_, ParserInput, Error>| -> Spanned<Expression >{
                    (
                        Expression::Comparison(Box::new(l), op, Box::new(r)),
                        ctx.span()
                    )
                },
            )
        };
        let binary = |associativity, token: Token, op: BinaryOp| {
            infix::<_, _, BinaryOp, Spanned<Expression>>(
                associativity,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(token).boxed(),
                move |l, r, ctx: &'static mut chumsky::input::MapExtra<'_, '_, ParserInput, Error>| -> Spanned<Expression >
                { 
                    (
                        Expression::Binary(Box::new(l), op, Box::new(r)),
                        ctx.span()
                    ) 
                },
            )
        };
        // https://doc.rust-lang.org/stable/reference/expressions.html#expression-precedence
        let atom2 = Parser::boxed(atom.clone()).pratt((
            // field ::= atom "." ident
            postfix::<_, _, Token, Spanned<Expression>>(
                8,
                just(Token::Period)
                    .ignore_then(crate::util_parsers::ident_parser().map(String::from))
                    .map_with(|field, ctx| (field, ctx.span())).boxed(),
                |l , field: Spanned<String>, ctx: &'static mut chumsky::input::MapExtra<'_, '_, ParserInput, Error> | {
                    (
                        Expression::FieldAccess(Box::new(l), field),
                        ctx.span()
                    )
                },
            ),
            // call ::= field "(" (expr ("," expr)*)? ")"
            postfix::<_, _, Token, Spanned<Expression>>(
                7,
                expr
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<Spanned<Expression>>>()
                    .map_with(|args, ctx| (args, ctx.span()))
                    .delimited_by(just(Token::Lparen), just(Token::Rparen)),
                |l, args: Spanned<Vec<Spanned<Expression>>>, ctx: &'static mut chumsky::input::MapExtra<'_, '_, ParserInput, Error> | {
                    (
                        Expression::FunctionCall(Box::new(l), args.0),
                        ctx.span()
                    )
                },
            ),
            // unary ::= ("!") call
            prefix::<_, _, Token, Spanned<Expression>>(
                6,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(Token::Bang).ignored(),
                | r, ctx: &'static mut chumsky::input::MapExtra<'_, '_, ParserInput, Error> | {
                    (
                        Expression::UnaryBool(Box::new(r)),
                        ctx.span()
                    )
                },
            ),
            // unary ::= ("-") call
            prefix::<_, _, Token, Spanned<Expression>>(
                6,
                just::<Token, ParserInput<'tokens, 'src>, Error<'tokens>>(Token::Minus).ignored(),
                |r, ctx: &'static mut chumsky::input::MapExtra<'_, '_, ParserInput, Error> | {
                    (
                        Expression::UnaryMath(Box::new(r)),
                        ctx.span()
                    )
                },
            ),
            // mul_div ::= unary ("*" | "/") unary
            binary_math(left(5), Token::Mul, MathOp::Mul),
            binary_math(left(5), Token::Div, MathOp::Div),
            // add_sub ::= mul_div ("+" | "-") mul_div
            binary_math(left(4), Token::Plus, MathOp::Add),
            binary_math(left(4), Token::Minus, MathOp::Sub),
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
        atom
    }
}

