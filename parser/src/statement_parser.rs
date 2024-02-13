use crate::ast::{Expression, If, Statement, Value};
use crate::convenience_parsers::{ident_parser, separator};
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::lexer::Token;
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
            // continue => "continue" expr
            let continue_ = just(Token::Continue)
                .ignored()
                .map(|()| -> Statement { Statement::Continue })
                .labelled("Continue");

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

            // assignment => ident "=" expr
            let assignment = ident_parser()
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .map_with_span(|(name, val), span| -> (Statement, SimpleSpan) {
                    (Statement::VariableDeclaration(name, Box::new(val)), span)
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
                .map_with_span(|(condition, code_block), span| -> Statement {
                    Statement::If((
                        If {
                            condition: Box::new(condition),
                            code_block,
                        },
                        span,
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
                loop_.map_with_span(|stmnt: Statement, span: SimpleSpan| (stmnt, span)),
                continue_.map_with_span(|stmnt: Statement, span: SimpleSpan| (stmnt, span)),
                break_.map_with_span(|stmnt: Statement, span: SimpleSpan| (stmnt, span)),
                return_.map_with_span(|stmnt: Statement, span: SimpleSpan| (stmnt, span)),
                if_.map_with_span(|stmnt: Statement, span: SimpleSpan| (stmnt, span)),
                assignment,
                expr.then_ignore(just(Token::StmtCast))
                    .map(|(expr, span)| (Statement::Expression(expr), span)),
            ))
        };
    statement
}

// block => ( [statement separator]* )
// let block = statement
//     .clone()
//     .map(|stmt: Spanned<Statement>| (stmt.0.to_instruction(), stmt.1))
//     .separated_by(separator().ignored())
//     .collect()
//     .delimited_by(
//         just(Token::Lparen).padded_by(separator()),
//         just(Token::Rparen).padded_by(separator()),
//     )
//     .map_with_span(|block, span| (CodeBlock(block), span));
// block_parser = Some(block.clone());
