use crate::ast::{
    EnumDeclaration, EnumVariantDeclaration, Expression, FunctionDeclaration, Import, Item,
    StructDeclaration, StructField, Type,
};
use crate::convenience_parsers::{ident_parser, separator, type_parser};
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::lexer::Token;
use crate::util_parsers::newline;

use chumsky::prelude::*;

pub fn item_parser<'tokens, 'src: 'tokens, T>(
    block: T,
) -> (impl Parser<'tokens, ParserInput<'tokens, 'src>, Item, Error<'tokens>> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone, // Statement
{
    choice((
        fn_parser(block)
            .map(Item::Function)
            .labelled("Function")
            .as_context(),
        enum_parser().map(Item::Enum).labelled("Enum").as_context(),
        struct_parser()
            .map(Item::Struct)
            .labelled("Struct")
            .as_context(),
        import_parser()
            .map(Item::Import)
            .labelled("Import")
            .as_context(),
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
    let arguments = ident_parser()
        .map_with(|name, ctx| -> (String, SimpleSpan) { (name, ctx.span()) })
        .then_ignore(just(Token::Hashtag))
        .then(type_parser())
        .map_with(|(name, b), ctx| ((b, ctx.span()), name))
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .labelled("arguments");
    // fn = type name ":" (ident "#" type ,)* block
    let function = type_parser()
        .labelled("return type")
        .map_with(|r#type, ctx| -> (Type, SimpleSpan) { (r#type, ctx.span()) })
        .then(ident_parser())
        .map_with(|(r#type, name), ctx| (r#type, (name, ctx.span())))
        .then_ignore(just(Token::Colon))
        .then(arguments)
        .then(block.clone())
        .map(
            |(((return_type, name), arguments), block)| FunctionDeclaration {
                name,
                return_type,
                arguments,
                body: block,
            },
        );
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
        .then_ignore(just(Token::Hashtag).labelled("#"))
        .then(type_parser())
        .map_with(|(name, r#type), ctx| (StructField { name, r#type }, ctx.span()))
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
        .map_with(|(struct_name, fields), ctx| {
            (
                StructDeclaration {
                    name: struct_name,
                    fields,
                },
                ctx.span(),
            )
        });
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
        .map_with(|(name, r#type), ctx| (EnumVariantDeclaration { name, r#type }, ctx.span()))
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
        .map_with(|(name, variants), ctx| (EnumDeclaration { name, variants }, ctx.span()));
    r#enum
}

pub fn import_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Import>, Error<'tokens>> + Clone {
    let ident = ident_parser();
    let import = just(Token::Import)
        .ignore_then(
            ident
                .clone()
                // TODO: Use path separator
                .then_ignore(just(Token::Div))
                .map_with(|module, ctx| (module, ctx.span()))
                .repeated()
                .collect(),
        )
        .then(ident.clone())
        .then_ignore(newline())
        .map_with(|(module, name), ctx| ((Import(module, (name, ctx.span()))), ctx.span()));
    import
}
