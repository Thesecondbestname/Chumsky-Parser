use crate::ast::{self, Type};
use crate::convenience_types::{Error, ParserInput};
use crate::Token;
use chumsky::prelude::*;

pub fn ident_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    String,                     // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident }.labelled("Identifier/ Name")
}
pub fn separator<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    (),                         // Output
    Error<'tokens>,             // Error Type
> + Clone {
    just(Token::Newline).repeated()
}
pub fn type_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Type,                       // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let int = select! { Token::Integer(v) => v }.labelled("Whole AAh integer");
    recursive(|r#type| {
        let path = ident_parser()
            .map_with_span(|a, b| (a, b))
            .separated_by(just(Token::PathSeperator))
            .collect()
            .map(ast::Path)
            .map_with_span(|a, b| Type::Path((a, b)));
        let primitives = select! {Token::Type(x) => x,}.labelled("primitive type");
        let tuple = r#type
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::Lparen), just(Token::Rparen))
            .map(Type::Tuple)
            .labelled("Tuple");
        let array = r#type
            .clone()
            .then_ignore(just(Token::Comma))
            .then(int)
            .delimited_by(just(Token::Lbracket), just(Token::Rbracket))
            .map(|(r#type, len)| Type::Array(Box::new(r#type), len))
            .labelled("Array");
        choice((tuple, primitives, array, path))
    })
    .labelled("Type")
}
pub fn parameter_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    (Type, String),             // Output
    Error<'tokens>,             // Error Type
> + Clone {
    type_parser()
        .then_ignore(just(Token::Hashtag))
        .then(ident_parser())
}

pub fn extra_delimited<'tokens, 'src: 'tokens, T>(
    idk: T,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    T,                          // Output
    Error<'tokens>,             // Error Type
> + Clone
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, T, Error<'tokens>> + Clone, // Statement
{
    idk.delimited_by(
        just(Token::Lparen).delimited_by(separator(), separator()),
        just(Token::Rparen).delimited_by(separator(), separator()),
    )
}
