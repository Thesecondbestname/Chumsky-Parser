use crate::ast::*;
use crate::convenience_parsers::ident_parser;
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::lexer::Token;
use crate::parsers::expression_parser;
use crate::util_parsers::separator;
use chumsky::prelude::*;

pub fn statement_parser<'tokens, 'src: 'tokens, T>(
    expr: T,
) -> (
    impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Statement>, Error<'tokens>> + Clone, // Statement
)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>>
        + Clone
        + 'tokens,
{
    // let colon = just(Token::Colon).labelled(":");
    // let comma = just(Token::Comma).labelled(",");
    let mut r#loop = None;
    let statement =
        {
            // continue => "continue" expr
            let continue_ = just(Token::Continue)
                .ignored()
                .map(|_| -> Statement { Statement::Continue })
                .labelled("Continue");

            // break => "break" expr
            let break_ = just(Token::Break)
                .ignore_then(expr.clone())
                .map(|expr| -> Statement { Statement::Break(Box::new(expr)) })
                .labelled("Break");

            // return => "return" expr
            let return_ = just(Token::Return)
                .ignore_then(expr.clone().or_not())
                .map(|expr| -> Statement {
                    if let Some(exp) = expr {
                        Statement::Return(Box::new(exp))
                    } else {
                        Statement::Return(Box::new((
                            Expression::Value(Value::Tuple(vec![])),
                            SimpleSpan::new(0, 0),
                        )))
                    }
                })
                .labelled("Return");

            // loop => "loop" expr block
            let loop_ = just(Token::Loop)
                .then_ignore(separator())
                .ignore_then(expr.clone()) // TODO Patch Expression parser to parse blocks.
                .labelled("block")
                .map(|expr| -> Statement {
                    println!("Loop: {:#?}", expr);
                    Statement::Loop(expr)
                })
                .labelled("loop statement");
            r#loop = Some(loop_.clone());

            // assignment => ident "=" expr
            let assignment = ident_parser()
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .map_with_span(|(name, val), span| -> (Statement, SimpleSpan) {
                    (Statement::VariableDeclaration(name, Box::new(val)), span)
                })
                .labelled("assignment");
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
                .delimited_by(just(Token::Lparen), just(Token::Rparen))
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
                .labelled("if statement");
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
                expr.map(|(expr, span)| (Statement::Expression(expr), span)),
            ))
        };
    (statement,)
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
