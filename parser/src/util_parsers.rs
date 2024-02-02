use crate::ast::Type;
use crate::convenience_types::*;
use crate::Token;
use chumsky::prelude::*;

pub(super) fn ident_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    String,                     // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident.clone().to_string() }.labelled("Identifier/ Name")
}
pub(super) fn separator<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    (),                         // Output
    Error<'tokens>,             // Error Type
> + Clone {
    just(Token::Newline).repeated()
}
pub(super) fn type_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Type,                       // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let int = select! { Token::Integer(v) => {println!("found {:?}",v);v.clone()} }
        .labelled("Whole AAh integer");
    recursive(|r#type| {
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
        choice((tuple, primitives, array, ident_parser().map(Type::Custom)))
    })
    .labelled("Type")
}
