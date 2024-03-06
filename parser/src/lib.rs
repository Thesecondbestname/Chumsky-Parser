#![feature(try_trait_v2)]
#![allow(clippy::needless_return)]
#![allow(needless_return)]
use chumsky::prelude::Rich;
pub use lexer::Token;
mod ast;
mod expression_parser;
mod item_parser;
mod lexer;
mod parser;
mod statement_parser;
mod util_parsers;

pub use parser::SketchyParser;
pub type OutputError<'a> = Rich<'a, crate::lexer::Token>;
pub type OutputType = convenience_types::Spanned<ast::Expression>;
mod convenience_types {
    use crate::Token;
    use chumsky::prelude::Rich;
    pub(crate) type Error<'tokens> = chumsky::extra::Err<Rich<'tokens, Token, Span>>;
    pub(crate) type Spanned<T> = (T, Span);
    pub(crate) type ParserInput<'tokens, 'src> =
        chumsky::input::SpannedInput<Token, Span, &'tokens [(Token, Span)]>;
    pub(crate) type Span = chumsky::span::SimpleSpan<usize>;
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
