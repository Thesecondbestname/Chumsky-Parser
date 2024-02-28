use std::string::ParseError;

use crate::ast::{Expression, If, Statement, Value};
use crate::convenience_parsers::ident_parser;
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
    let statement =
        {
            // continue => "continue"
            let continue_ = just(Token::Continue)
                .ignored()
                .then_ignore(newline())
                .map(|()| -> Statement { Statement::Continue })
                .labelled("Continue");

            // break => "break" expr
            let break_ = just(Token::Break)
                .ignore_then(expr.clone())
                .then_ignore(newline())
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
                .then_ignore(newline())
                .labelled("Return")
                .as_context();

            // loop => "loop" expr block
            let loop_ = just(Token::Loop)
                .ignore_then(expr.clone())
                .then_ignore(newline())
                .map(|expr| -> Statement { Statement::Loop(expr) })
                .labelled("loop statement")
                .as_context();

            // assignment => ident "=" expr
            let assignment = ident_parser()
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(newline())
                .map_with(|(name, val), ctx| -> (Statement, SimpleSpan) {
                    (
                        Statement::VariableDeclaration(name, Box::new(val)),
                        ctx.span(),
                    )
                })
                .labelled("assignment")
                .as_context();
            // if => "if" expr "then" expr ("else" expr)?
            let if_ = just(Token::If)
                .ignore_then(expr.clone())
                .recover_with(via_parser(nested_delimiters(
                    Token::If,
                    Token::Lparen,
                    [
                        (Token::Lparen, Token::Rparen),
                        (Token::Lbracket, Token::Rbracket),
                    ],
                    |span| (Expression::ParserError, span),
                )))
                .then(expr.clone())
                .clone()
                .map_with(|(condition, code_block), ctx| -> Statement {
                    Statement::If((
                        If {
                            condition: Box::new(condition),
                            code_block,
                        },
                        ctx.span(),
                    ))
                })
                .labelled("if statement")
                .as_context();
            let if_else =
                if_.clone()
                    .separated_by(just(Token::Else))
                    .collect::<Vec<_>>()
                    .map(|if_| -> Statement {
                        Statement::IfElse(if_.into_iter().map(|if_| {
                    if let Statement::If(if_) = if_ {
                        if_
                    } else {
                        panic!("[INTERNAL ERROR] missmatched statement parsed in If block")
                    }
                }).collect::<Vec<_>>())
                    });
            choice((
                loop_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
                continue_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
                break_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
                return_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
                if_.map_with(|stmnt: Statement, ctx| (stmnt, ctx.span())),
                assignment,
                // TODO: Add recovery here in case user forgets the stmt cast
                expr.then_ignore(just(Token::StmtCast))
                    .map(|(expr, span)| (Statement::Expression(expr), span)),
            ))
        };
    statement
}
