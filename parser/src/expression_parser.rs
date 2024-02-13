#[allow(clippy::too_many_lines)]
pub mod expressions {
    use crate::ast::{BinaryOp, ComparisonOp, Expression, MathOp, Number, Value};
    use crate::convenience_parsers::separator;
    use crate::convenience_types::{Error, ParserInput, Spanned};
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

        // Blocks are expressions but delimited with parentheses
        let block = block
            .clone()
            .delimited_by(
                just(Token::Lparen).delimited_by(separator(), separator()),
                just(Token::Rparen).delimited_by(separator(), separator()),
            )
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(via_parser(nested_delimiters(
                Token::Lparen,
                Token::Rparen,
                [(Token::Lbracket, Token::Rbracket)],
                |span| (Expression::ParserError, span),
            )));
        // The recursive expression Part
        recursive(|expression| {
            let inline_expression = {
                // Atom which is the smallest expression.
                let atom = choice((ident.map(Expression::Ident), number, bool, string, span))
                    .map_with_span(|expr, span: SimpleSpan| (expr, span))
                    // Atoms can also just be normal expressions, but surrounded with parentheses
                    .or(expression
                        .clone()
                        .delimited_by(just(Token::Lparen), just(Token::Rparen)))
                    // Attempt to recover anything that looks like a parenthesised expression but contains errors
                    .recover_with(via_parser(nested_delimiters(
                        Token::Lparen,
                        Token::Rparen,
                        [(Token::Lbracket, Token::Rbracket)],
                        |span| (Expression::ParserError, span),
                    )))
                    // Attempt to recover anything that looks like a list but contains errors
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
                            .map_with_span(|expr, span: SimpleSpan| (expr, span))
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
                    .map_with_span(|((called_on, name), args), span| {
                        (
                            Expression::MethodCall(
                                Box::new(called_on),
                                name,
                                args.map_or_else(std::vec::Vec::new, |arguments| arguments),
                            ),
                            span,
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
                comp.labelled("expression").as_context()
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
