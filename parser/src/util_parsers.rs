use crate::ast::{self, Ident, Name, Pattern, Type};
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::expression::value;
use crate::Token;
use chumsky::label::LabelError;
use chumsky::prelude::*;

pub fn name_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    String,                     // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident }.labelled("Name")
}
pub fn ident_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Ident,                      // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident }
        .map_with(|a, ctx| (a, ctx.span()))
        .separated_by(just(Token::DoubleColon))
        .at_least(1)
        .collect()
        .map(ast::Ident)
        .labelled("Identifier")
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
        let path = ident_parser().map_with(|a, ctx| Type::Path((a, ctx.span())));
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
        choice((
            tuple,
            primitives,
            array,
            path,
            just(Token::Self_).to(Type::Self_),
        ))
    })
    .labelled("Type")
}
pub fn parameter_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,       // Input
    (Spanned<String>, Spanned<Type>), // Output
    Error<'tokens>,                   // Error Type
> + Clone {
    choice((
        just(Token::Self_)
            .map_with(|_, ctx| (("self".to_owned(), ctx.span()), (Type::Self_, ctx.span()))),
        name_parser()
            .map_with(|name, ctx| (name, ctx.span()))
            .then_ignore(just(Token::Hashtag))
            .then(type_parser().map_with(|type_, ctx| (type_, ctx.span()))),
    ))
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
        end().labelled("EOI"),
        just(Token::Rparen).rewind().ignored(),
        just(Token::Lparen).rewind().ignored(),
    ))
    .labelled("Separator")
}
pub fn unexpected_newline<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    (),                         // Output
    Error<'tokens>,             // Error Type
> + Clone {
    none_of(Token::Newline)
        .ignore_then(choice((
            just(Token::Newline).ignored(),
            end().labelled("EOI"),
            just(Token::Rparen).rewind().ignored(),
            just(Token::Lparen).rewind().ignored(),
        )))
        .labelled("Unexpected Separator")
}
pub fn irrefutable_pattern<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Pattern>,           // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let pattern = recursive(|pat| pattern(pat).map_with(|pat, ctx| (pat, ctx.span())));
    pattern.labelled("irrefutable pattern")
}
pub fn refutable_pattern<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Pattern>,           // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let pattern = recursive(|pat| {
        pattern(pat)
            .or(value().map_with(|a, b| (a, b.span())).map(Pattern::Value))
            .map_with(|pat, ctx| (pat, ctx.span()))
    });
    pattern.labelled("labelled pattern")
}
pub fn pattern<'tokens, 'src: 'tokens, T>(
    pattern: T,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Pattern,                    // Output
    Error<'tokens>,             // Error Type
> + Clone
where
    T: Parser<
            'tokens,
            ParserInput<'tokens, 'src>, // Input
            Spanned<Pattern>,           // Output
            Error<'tokens>,             // Error Type
        > + Clone,
{
    let name_pattern = ident_parser().map(|ident| {
        let Ident(s) = ident;
        if "_" == s[0].0 && s.len() == 1 {
            Name::Underscore
        } else {
            Name::Name(s)
        }
    });
    let tuple_destructure = pattern
        .clone()
        .separated_by(just(Token::Comma))
        .collect()
        .delimited_by(just(Token::Lparen), just(Token::Rparen));
    let enum_destructure = ident_parser().then(tuple_destructure.clone());
    let struct_destructure = ident_parser().then(
        name_pattern
            .clone()
            .map_with(|pat, ctx| (pat, ctx.span()))
            .then_ignore(just(Token::Hashtag))
            .then(pattern.clone())
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::Lparen), just(Token::Rparen)),
    );
    let array_destructure = pattern
        .clone()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then_ignore(just(Token::DoubleDot))
        .then(name_pattern.clone())
        .delimited_by(just(Token::Lbucket), just(Token::Rbucket));
    choice((
        struct_destructure
            .map(|(name, b)| Pattern::Struct(name, b))
            .labelled("Struct destructure"),
        enum_destructure
            .map(|(name, pat)| Pattern::Enum(name, pat))
            .labelled("Enum destructure"),
        tuple_destructure
            .map(Pattern::Tuple)
            .labelled("Tuple destructure"),
        array_destructure
            .map(|(pats, end)| Pattern::Array(pats, end))
            .labelled("Array destructure"),
        name_pattern.map(Pattern::Name).labelled("name"),
    ))
}
