use crate::ast::*;
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::item_parser::item_parser;
use crate::item_parsers::{fn_parser, import_parser};
use crate::parsers::expression_parser;
use crate::parsers::statement_parser;
use crate::Token;
use chumsky::prelude::*;

pub(super) fn programm_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Expression>,        // Output
    Error<'tokens>,             // Error Type
> + Clone {
    // let mut _code = None;
    // let programm_parser = recursive(|line_expr| {
    let code = statement_parser((expression_parser()))
        .0
        .separated_by(just(Token::Newline))
        .allow_trailing()
        .collect::<Vec<_>>()
        .map_with_span(|expr, span| (Expression::Block(expr), span));
    let block = code
        .clone()
        .delimited_by(just(Token::Lparen), just(Token::Rparen));
    // _code = Some(code);
    code
    // });
    // programm_parser
}
fn block_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Statement>,         // Output
    Error<'tokens>,             // Error Type
> + Clone {
    // import, function, statement, scope
    let scope = recursive(|block| {
        choice((
            item_parser(block),
            statement_parser(block),
            expression_parser().delimited_by(just(Token::Lparen), just(Token::Rparen)),
        ))
        .repeated()
        .collect::<Vec<_>>()
    });
}
pub fn parse_from_lex(
    input: &Vec<(Token, SimpleSpan)>,
) -> ParseResult<Spanned<Expression>, Rich<Token>> {
    programm_parser().parse(input.as_slice().spanned((input.len()..input.len()).into()))
}
pub fn range_into_span(input: &Vec<(Token, std::ops::Range<usize>)>) -> Vec<(Token, SimpleSpan)> {
    input
        .iter()
        .map(|token| -> (Token, SimpleSpan) {
            (token.0.clone(), SimpleSpan::from(token.1.clone()))
        })
        .collect::<Vec<_>>()
}
