#[allow(clippy::too_many_lines)]
pub mod expressions {
    use crate::ast::{BinaryOp, ComparisonOp, Expression, If, MathOp, Number, Value};
    use crate::convenience_types::{Error, ParserInput, Spanned};
    use crate::util_parsers::{extra_delimited, ident_parser, name_parser};
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
        let ident = ident_parser();
        let int = select! { Token::Integer(v) => v }.labelled("Whole AAh integer");
        let float = select! { Token::Float(v) => v }.labelled("Floating point");
        let number =
            int.map_with(|int, ctx| Expression::Value(Value::Number(Number::Int(int))))
                .or(float
                    .map_with(|float, ctx| Expression::Value(Value::Number(Number::Float(float)))));
        let bool = select! {
            Token::True=> Expression::Value(Value::Bool(true)),
            Token::False => Expression::Value(Value::Bool(false))
        }
        .labelled("Boolean");
        let string = select! {Token::LiteralString(s) => Expression::Value(Value::String(s))}
            .labelled("String");
        let span = select! {Token::Span(s) => Expression::Value(Value::Span(s.start, s.end))};
        let delim_block = extra_delimited(block.clone());

        // The recursive expression Part
        recursive(|expression| {
            let inline_expression = {
                // Atom which is the smallest expression.
                let atom = choice((
                    ident.clone().map(Expression::Ident),
                    number,
                    bool,
                    string,
                    span,
                ))
                .labelled("Atom")
                .as_context()
                .map_with(|expr, span| (expr, span.span()))
                .or(
                    delim_block
                        .clone()
                        // Attempt to recover anything that looks like a parenthesised expression but contains errors
                        .recover_with(via_parser(nested_delimiters(
                            Token::Lparen,
                            Token::Rparen,
                            [(Token::Lbracket, Token::Rbracket)],
                            |span| (Expression::ParserError, span),
                        )))
                        .labelled("Expression Block")
                        .as_context(), // Atoms can also just be normal expressions, but surrounded with parentheses
                );
                // A list of expressions
                let items = expression
                    .clone()
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
                let method_call = call
                    .clone()
                    .foldl(
                        just(Token::Period)
                            .ignore_then(ident.clone())
                            .then(list.clone().or_not())
                            .repeated(),
                        |func: Spanned<Expression>, (name, args)| {
                            let spanend = if let Some(arg) = args.clone() {
                                arg.last()
                                    .expect("[INTERNAL ERROR] I method args span is empty")
                                    .1
                                    .end
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
                    .or(just(Token::Slash).to(MathOp::Div));
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

                let else_expression = sum
                    .clone()
                    .foldl(
                        just(Token::Else).ignore_then(expression.clone()).repeated(),
                        |expr, else_branch| {
                            let span = expr.1.start()..else_branch.1.end();
                            (
                                Expression::Else(Box::new(expr), Box::new(else_branch)),
                                span.into(),
                            )
                        },
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
                };
                // if => "if" expr block
                let if_ = just(Token::If)
                    .ignore_then(comp.clone())
                    .recover_with(via_parser(nested_delimiters(
                        Token::If,
                        Token::Lparen,
                        [
                            (Token::Lbracket, Token::Rbracket),
                            (Token::Lparen, Token::Semicolon),
                        ],
                        |span| (Expression::ParserError, span),
                    )))
                    .labelled("Condition")
                    .as_context()
                    .then(
                        expression
                            .clone()
                            .labelled("If block")
                            .as_context()
                            .recover_with(via_parser(nested_delimiters(
                                Token::Lparen,
                                Token::Rparen,
                                [(Token::Lbracket, Token::Rbracket)],
                                |span| (Expression::ParserError, span),
                            ))),
                    )
                    .map_with(|(condition, code_block), ctx| {
                        (
                            Expression::If(Box::new(If {
                                condition: Box::new(condition),
                                code_block,
                            })),
                            ctx.span(),
                        )
                    })
                    .labelled("if *expression*")
                    .as_context();
                // Comparison ops (equal, not-equal) have equal precedence
                comp.labelled("line expression").as_context().boxed()
            };
            let obj_construction = ident
                .clone()
                .map_with(|a, ctx| (a, ctx.span()))
                .then(
                    name_parser()
                        .map_with(|a, ctx| (a, ctx.span()))
                        .then_ignore(just(Token::Eq))
                        .then(expression.clone())
                        .separated_by(just(Token::Colon))
                        .collect::<Vec<_>>()
                        .map_with(|a, ctx| (a, ctx.span())),
                )
                .delimited_by(just(Token::Lbracket), just(Token::Rbracket))
                .map_with(|(name, args), ctx| {
                    (
                        Expression::Value(Value::Object { name, fields: args }),
                        ctx.span(),
                    )
                });
            choice((inline_expression, obj_construction))
        })
    }
    // pub fn pattern<'tokens, 'src: 'tokens, T>(
    // ) -> impl Parser<
    //     'tokens,
    //     ParserInput<'tokens, 'src>, // Input
    //     Spanned<Expression>,        // Output
    //     Error<'tokens>,             // Error Type
    // > + Clone
    // {

    // }
}
