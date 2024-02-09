use crate::ast::*;
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::item_parser::item_parser;
use crate::parsers::expression_parser;
use crate::parsers::statement_parser;
use crate::Token;
use chumsky::prelude::*;

// pub(super) fn programm_parser<'tokens, 'src: 'tokens>() -> impl Parser<
//     'tokens,
//     ParserInput<'tokens, 'src>, // Input
//     Spanned<Expression>,        // Output
//     Error<'tokens>,             // Error Type
// > + Clone {
//     // let code = statement_parser((expression_parser()))
//     //     .0
//     //     .separated_by(just(Token::Newline))
//     //     .allow_trailing()
//     //     .collect::<Vec<_>>()
//     //     .map_with_span(|expr, span| (Expression::DEPRECATED_BLOCK(expr), span));
//     // let block = code
//     //     .clone()
//     //     .delimited_by(just(Token::Lparen), just(Token::Lparen));
//     // code
//     let programm = item_parser()
// }
fn block_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Expression>,        // Output
    Error<'tokens>,             // Error Type
> + Clone {
    // import, function, statement, scope
    let scope = recursive(|block| {
        let block_element = choice((
            item_parser(block.clone()).map_with_span(|item, span| BlockElement::Item((item, span))),
            statement_parser(expression_parser(block)).map(BlockElement::Statement),
        ));
        let program = block_element
            .map_with_span(|item, span| (item, span))
            .separated_by(just(Token::Newline))
            .collect::<Vec<_>>()
            .map_with_span(|items, span| (Expression::Block(Block(items)), span));
        return program;
    });
    return scope;
}
pub fn parse_from_lex(
    input: &Vec<(Token, SimpleSpan)>,
) -> ParseResult<Spanned<Expression>, Rich<Token>> {
    block_parser().parse(input.as_slice().spanned((input.len()..input.len()).into()))
}
pub fn range_into_span(input: &Vec<(Token, std::ops::Range<usize>)>) -> Vec<(Token, SimpleSpan)> {
    input
        .iter()
        .map(|token| -> (Token, SimpleSpan) {
            (token.0.clone(), SimpleSpan::from(token.1.clone()))
        })
        .collect::<Vec<_>>()
}
