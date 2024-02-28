#![feature(try_trait_v2)]
use chumsky::prelude::Rich;
use chumsky::span::SimpleSpan;
pub use lexer::Token;
mod ast;
mod expression_parser;
mod item_parser;
mod lexer;
mod parser;
mod statement_parser;
mod util_parsers;

pub use lexer::lex_sketchy_program;
pub use parser::parse_from_lex;
pub use parser::range_into_span;
pub use parser::SketchyParser;
pub type OutputError<'a> = Rich<'a, crate::lexer::Token>;
mod convenience_types {
    use crate::Token;
    use chumsky::prelude::Rich;
    pub(crate) type Error<'tokens> = chumsky::extra::Err<Rich<'tokens, Token, Span>>;
    pub type OutputError<'a> = Rich<'a, crate::lexer::Token>;
    pub(crate) type Spanned<T> = (T, Span);
    pub(crate) type ParserInput<'tokens, 'src> =
        chumsky::input::SpannedInput<Token, Span, &'tokens [(Token, Span)]>;
    pub(crate) type Span = chumsky::span::SimpleSpan<usize>;
}
pub fn empty_span(offset: usize) -> SimpleSpan {
    SimpleSpan::splat(offset)
}
mod parsers {
    pub(crate) use crate::{
        expression_parser::expressions::expression_parser, statement_parser::statement_parser,
    };
}
mod convenience_parsers {
    pub(crate) use super::util_parsers::{ident_parser, separator, type_parser};
}
#[cfg(test)]
mod tests {}
