use crate::ast::{Expression, Statement, Value};
use crate::convenience_parsers::name_parser;
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::lexer::Token;
use crate::util_parsers::newline;
use chumsky::prelude::*;

pub fn statement_parser<'tokens, 'src: 'tokens, T>(
    expr: T,
) -> (impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Statement>, Error<'tokens>> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>>
        + Clone
        + 'tokens,
{
    // let colon = just(Token::Colon).labelled(":");
    // let comma = just(Token::Comma).labelled(",");
    let statement = {
        // continue => "continue"
        let continue_ = just(Token::Continue)
            .ignored()
            .map(|()| -> Statement { Statement::Continue })
            .labelled("Continue")
            .as_context();

        // break => "break" expr
        let break_ = just(Token::Break)
            .ignore_then(expr.clone())
            .map(|expr| -> Statement { Statement::Break(Box::new(expr)) })
            .labelled("Break")
            .as_context();

        // return => "return" expr
        let return_ = just(Token::Return)
            .ignore_then(expr.clone().or_not())
            .map(|expr| -> Statement {
                expr.map_or_else(
                    || {
                        Statement::Return(Box::new((
                            Expression::Value(Value::Tuple(vec![])),
                            SimpleSpan::new(0, 0),
                        )))
                    },
                    |exp| Statement::Return(Box::new(exp)),
                )
            })
            .labelled("Return")
            .as_context();

        // loop => "loop" expr block
        let loop_ = just(Token::Loop)
            .ignore_then(expr.clone())
            .map(|expr| -> Statement { Statement::Loop(expr) })
            .labelled("loop statement")
            .as_context();
        choice((
            expr.clone()
                .then_ignore(just(Token::StmtCast))
                .map(|(expr, span)| (Statement::Expression(expr), span)),
            loop_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
            continue_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
            break_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
            return_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
            // expr.clone()
            //     .map(|(expr, span)| (Statement::Expression(expr), span)),
            // TODO: Add recovery here in case user forgets the stmt cast
        ))
    };
    statement
}
