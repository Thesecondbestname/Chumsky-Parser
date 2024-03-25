use crate::ast::{self, Type};
use crate::convenience_types::{Error, ParserInput, Spanned};
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
    just(Token::Newline).or_not().ignored()
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
            .map_with(|a, ctx| (a, ctx.span()))
            .separated_by(just(Token::Slash))
            .collect()
            .map(ast::Path)
            .map_with(|a, ctx| Type::Path((a, ctx.span())));
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
    ParserInput<'tokens, 'src>,       // Input
    (Spanned<String>, Spanned<Type>), // Output
    Error<'tokens>,                   // Error Type
> + Clone {
    ident_parser()
        .map_with(|name, ctx| (name, ctx.span()))
        .then_ignore(just(Token::Hashtag))
        .then(type_parser().map_with(|type_, ctx| (type_, ctx.span())))
}

pub fn extra_delimited<'tokens, 'src: 'tokens, T, U>(
    idk: T,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    U,                          // Output
    Error<'tokens>,             // Error Type
> + Clone
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, U, Error<'tokens>> + Clone, // Statement
{
    idk.delimited_by(
        just(Token::Lparen).delimited_by(separator(), separator()),
        separator().then(just(Token::Rparen)),
    )
}
pub fn newline<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    (),                         // Output
    Error<'tokens>,             // Error Type
> + Clone {
    choice((
        just(Token::Newline).ignored(),
        end().labelled(""),
        just(Token::Rparen).rewind().ignored(),
        just(Token::Lparen).rewind().ignored(),
    ))
    .labelled("Separator")
}
