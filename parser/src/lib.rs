pub(crate) use crate::lexer::Token;
mod ast;
mod expression_parser;
mod item_parser;
mod lexer;
mod parser;
mod statement_parser;
mod util_parsers;

pub use lexer::lex_arrow_program;
pub use parser::parse_from_lex;

mod convenience_types {
    use crate::Token;
    use chumsky::prelude::Rich;
    pub(crate) type Error<'tokens> = chumsky::extra::Err<Rich<'tokens, Token, Span>>;
    pub(crate) type OutputError<'a> = Rich<'a, crate::lexer::Token>;
    pub(crate) type Spanned<T> = (T, Span);
    pub(crate) type ParserInput<'tokens, 'src> =
        chumsky::input::SpannedInput<Token, Span, &'tokens [(Token, Span)]>;
    pub(crate) type Span = chumsky::span::SimpleSpan<usize>;
}
mod parsers {
    pub(crate) use crate::{
        expression_parser::expressions::expression_parser, statement_parser::statement_parser,
    };
}
mod item_parsers {
    pub(crate) use crate::item_parser::enum_parser;
    pub(crate) use crate::item_parser::fn_parser;
    pub(crate) use crate::item_parser::import_parser;
    pub(crate) use crate::item_parser::struct_parser;
    // pub(crate) use crate::item_parser::trait_parser;
}
mod convenience_parsers {
    pub(crate) use super::util_parsers::{ident_parser, separator, type_parser};
}
#[cfg(test)]
mod tests {}
