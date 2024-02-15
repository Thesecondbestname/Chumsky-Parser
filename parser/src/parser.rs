use crate::ast::*;
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::item_parser::item_parser;
use crate::parsers::expression_parser;
use crate::parsers::statement_parser;
use crate::util_parsers::extra_delimited;
use crate::Token;
use chumsky::prelude::*;

fn block_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Expression>,        // Output
    Error<'tokens>,             // Error Type
> + Clone {
    // import, function, statement, scope
    let scope = recursive(|block| {
        let delimited_block = extra_delimited::<_, Spanned<Expression>>(block.clone())
            .recover_with(via_parser(nested_delimiters(
                Token::Lparen,
                Token::Rparen,
                [(Token::Lbracket, Token::Rbracket)],
                |span| (Expression::ParserError, span),
            )));

        let block_element = choice((
            item_parser(block.clone()).map_with(|item, ctx| BlockElement::Item((item, ctx.span()))),
            statement_parser(expression_parser(delimited_block)).map(BlockElement::Statement),
        ));
        let program = block_element
            .map_with(|item, ctx| (item, ctx.span()))
            .separated_by(just(Token::Newline))
            .collect::<Vec<_>>()
            .map_with(|items, ctx| (Expression::Block(Block(items)), ctx.span()));
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
