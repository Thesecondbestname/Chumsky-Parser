use crate::ast::{
    EnumDeclaration, EnumVariantDeclaration, Expression, FunctionDeclaration, Import, Item,
    Statement, StructDeclaration, StructField, Type,
};
use crate::convenience_parsers::{ident_parser, separator, type_parser};
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::lexer::Token;

use chumsky::prelude::*;

pub fn item_parser<'tokens, 'src: 'tokens, T>(
    block: T,
) -> (impl Parser<'tokens, ParserInput<'tokens, 'src>, Item, Error<'tokens>> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone, // Statement
{
    choice((
        fn_parser(block).map(Item::Function),
        enum_parser().map(Item::Enum),
        struct_parser().map(Item::Struct),
        import_parser().map(Item::Import),
    ))
}
pub fn fn_parser<'tokens, 'src: 'tokens, T>(
    block: T,
) -> (impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    FunctionDeclaration,        // Output
    Error<'tokens>,             // Error Type
> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone, // Statement
{
    let block = block.delimited_by(just(Token::Lparen), just(Token::Rparen));
    // fn = type name ":" (ident "#" type ,)* block
    let function = type_parser()
        .labelled("return type")
        .map_with_span(|r#type, span| -> (Type, SimpleSpan) { (r#type, span) })
        .then(ident_parser())
        .map_with_span(|(r#type, name), span| (r#type, (name, span)))
        .then_ignore(just(Token::Colon))
        .then(
            ident_parser()
                .map_with_span(|name, span| -> (String, SimpleSpan) { (name, span) })
                .then_ignore(just(Token::Hashtag))
                .then(type_parser())
                .map_with_span(|(name, b), span| ((b, span), name))
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .labelled("arguments"),
        )
        .then(block.clone()) // TODO again using Block parser to parse a block
        .map(
            |(((return_type, name), arguments), block)| FunctionDeclaration {
                name,
                return_type,
                arguments,
                body: block,
            },
        )
        .labelled("function definition");
    return function;
}
pub fn struct_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<StructDeclaration>, // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let struct_field = ident_parser()
        .clone()
        .then_ignore(just(Token::DoubleColon).labelled("::"))
        .then(type_parser())
        .map_with_span(|(name, r#type), span| (StructField { name, r#type }, span))
        .labelled("struct declaration field");

    let r#struct = just(Token::Struct)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Colon))
        .then_ignore(separator())
        .then(
            struct_field
                .separated_by(just(Token::Comma).padded_by(separator()))
                .collect::<Vec<_>>(),
        )
        .then_ignore(separator())
        .then_ignore(just(Token::Semicolon))
        .map_with_span(|(struct_name, fields), span| {
            (
                StructDeclaration {
                    name: struct_name,
                    fields,
                },
                span,
            )
        })
        .labelled("struct declaration");
    r#struct
}

pub fn enum_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<EnumDeclaration>, Error<'tokens>> + Clone
{
    let enum_fields = ident_parser()
        .labelled("Enum field name")
        .then(
            type_parser()
                .delimited_by(just(Token::Lparen), just(Token::Rparen))
                .or_not(),
        )
        .padded_by(separator())
        .map_with_span(|(name, r#type), span| (EnumVariantDeclaration { name, r#type }, span))
        .labelled("Enum field");

    let r#enum = just(Token::Enum)
        .labelled("Enum Token")
        .ignore_then(ident_parser())
        .labelled("Enum name")
        .then_ignore(just(Token::Colon))
        .then_ignore(separator())
        .then(
            enum_fields
                .separated_by(just(Token::Comma).then_ignore(separator()))
                .allow_trailing()
                .collect::<Vec<(EnumVariantDeclaration, SimpleSpan)>>(),
        )
        .then_ignore(just(Token::Semicolon))
        .map_with_span(|(name, variants), span| (EnumDeclaration { name, variants }, span))
        .labelled("Enum declaration");
    r#enum
}

pub fn import_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Import>, Error<'tokens>> + Clone {
    let ident = ident_parser();
    let import = just(Token::Import)
        .ignore_then(
            ident
                .clone()
                .then_ignore(just(Token::PathSeperator))
                .map_with_span(|module, span| (module, span))
                .repeated()
                .collect(),
        )
        .then(ident.clone())
        .map_with_span(|(module, name), span| ((Import(module, (name, span))), span))
        .labelled("Import");
    import
}

pub fn trait_parser() {}
