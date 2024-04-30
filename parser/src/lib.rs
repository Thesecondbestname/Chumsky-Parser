#![allow(clippy::needless_return)]
use chumsky::prelude::Rich;
pub use lexer::Token;
mod ast;

mod expression;
mod item;
mod lexer;
mod parse_error;
mod parser;
mod statement_parser;
mod util_parsers;

pub use parser::SketchyParser;
pub type OutputError<'a> = Rich<'a, crate::lexer::Token>;
pub type OutputType = convenience_types::Spanned<ast::Expression>;
mod convenience_types {
    use crate::Token;
    use chumsky::prelude::Rich;
    pub type Error<'tokens> = chumsky::extra::Err<Rich<'tokens, Token, Span>>;
    pub type Spanned<T> = (T, Span);
    pub type ParserInput<'tokens, 'src> =
        chumsky::input::SpannedInput<Token, Span, &'tokens [(Token, Span)]>;
    pub type Span = chumsky::span::SimpleSpan<usize>;
}
pub mod span_functions {
    use chumsky::span::SimpleSpan;
    pub fn empty_span() -> SimpleSpan {
        SimpleSpan::new(0, 0)
    }
    pub fn span_from(offset: usize) -> SimpleSpan {
        SimpleSpan::splat(offset)
    }
    pub fn range_to_span(range: &std::ops::Range<usize>) -> SimpleSpan {
        SimpleSpan::from(range.clone())
    }
}
mod convenience_parsers {
    pub use super::util_parsers::{name_parser, separator, type_parser};
}
#[cfg(test)]
mod tests {}
