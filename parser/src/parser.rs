#![allow(clippy::needless_return)]
use crate::ast::{Block, BlockElement, Expression};
use crate::convenience_types::{Error, ParserInput, Spanned};
use crate::item_parser::item_parser;
use crate::lexer::{lex_sketchy_program, Lex, LexError};
use crate::util_parsers::extra_delimited;
use crate::Token;
use crate::{empty_span, parsers::*};
use chumsky::container::Container;
use chumsky::prelude::*;
use chumsky::span::Span;
use thiserror::Error as DeriveError;

fn block_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Expression>,        // Output
    Error<'tokens>,             // Error Type
> + Clone {
    // import, function, statement, scope
    let scope = recursive(|block| {
        let block_element = choice((
            item_parser(block.clone()).map_with(|item, ctx| BlockElement::Item((item, ctx.span()))),
            statement_parser(expression_parser(block)).map(BlockElement::Statement),
        ));
        let delimited_block = extra_delimited::<_, Spanned<BlockElement>>(
            block_element
                .clone()
                .map_with(|expr, ctx| (expr, ctx.span())),
        )
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|items, ctx| (Expression::Block(Block(items)), ctx.span()));

        let program = block_element
            .map_with(|item, ctx| (item, ctx.span()))
            .repeated()
            .collect::<Vec<_>>()
            .map_with(|items, ctx| (Expression::Block(Block(items)), ctx.span()));
        return delimited_block;
    });
    return scope;
}
// ----- STATES ----
#[derive(Default, Clone)]
pub struct NotInitialized;
#[derive(Default, Clone, Debug)]
pub struct Initialized(String);
#[derive(Default, Clone, Debug)]
pub struct Lexed(Vec<(Token, SimpleSpan)>);
/// Contains a parsed program. Either holds the parsed ast, or the errors with the recovered ast
#[derive(Default, Clone)]
pub struct Parsed(Option<Spanned<Expression>>);
// ---- STATES ----

pub struct SketchyParser {
    input: String,
    parse_result: Spanned<Expression>,
}
#[derive(Default, Debug)]
pub struct SketchyParserBuilder<I, L, P> {
    input: I,
    tokens: Option<L>,
    parse_result: P,
}
impl<I, L: Default, P: Default> SketchyParserBuilder<I, L, P> {
    pub fn from_input(self, inp: impl Into<String>) -> SketchyParserBuilder<Initialized, L, P> {
        SketchyParserBuilder {
            input: Initialized(inp.into()),
            tokens: self.tokens,
            parse_result: self.parse_result,
        }
    }
}
impl<L, P> SketchyParserBuilder<Initialized, L, P> {
    pub fn lex_sketchy_programm(self) -> LexResult<P> {
        LexResult(
            lex_sketchy_program(&format!("({})", self.input.0))
                .to_result()
                .map(|lex| SketchyParserBuilder {
                    input: self.input.clone(),
                    tokens: Some(Lexed(
                        lex.into_iter().map(|(a, span)| (a, span.into())).collect(),
                    )),
                    parse_result: self.parse_result,
                })
                .map_err(|(recovered, err)| LexErr(recovered, err, self.input.0)),
        )
    }
}
impl<P> SketchyParserBuilder<Initialized, Lexed, P> {
    pub fn parse_sketchy_programm<'a>(self) -> ParserResult<'a> {
        let input = &self.tokens.as_ref().unwrap().0;
        let parse =
            block_parser().parse(input.as_slice().spanned((input.len()..input.len()).into()));
        let (ast, errs) = parse.into_output_errors();
        if let Some(ast) = ast {
            if errs.is_empty() {
                return ParserResult(Ok(SketchyParserBuilder {
                    input: self.input.clone(),
                    tokens: None,
                    parse_result: Parsed(Some(ast)),
                }));
            }
            return ParserResult(Err(ParseErr(
                errs.into_iter()
                    .map(chumsky::error::Rich::into_owned)
                    .collect(),
                ast,
                self.input.0,
            )));
        }
        return ParserResult(Err(ParseErr(
            errs.into_iter()
                .map(chumsky::error::Rich::into_owned)
                .collect(),
            (Expression::ParserError, empty_span()),
            self.input.0,
        )));
    }
}
impl SketchyParserBuilder<Initialized, Lexed, Parsed> {
    pub fn build(self) -> SketchyParser {
        SketchyParser {
            input: self.input.0,
            parse_result: self.parse_result.0.expect(
                "[INTERNAL ERROR] Fucked up the typestate. Called build on empty parse_result field",
            ),
        }
    }
}
impl SketchyParser {
    pub fn builder() -> SketchyParserBuilder<NotInitialized, NotInitialized, NotInitialized> {
        SketchyParserBuilder::default()
    }
}
impl SketchyParser {
    pub fn ast(&self) -> &Expression {
        &self.parse_result.0
    }
    pub fn into_ast(self) -> Expression {
        self.parse_result.0
    }
    pub fn span_on_src(&self, span: SimpleSpan) -> String {
        self.input[span.start()..span.end()].to_string()
    }
}
///Parser error type
#[derive(DeriveError, Debug)]
#[error("Error while Parsing")]
pub struct ParseErr<'a>(Vec<Rich<'a, Token>>, Spanned<Expression>, String);
/// Result Type of parse
pub struct ParserResult<'a>(
    anyhow::Result<SketchyParserBuilder<Initialized, Lexed, Parsed>, ParseErr<'a>>,
);
/// Lexer error type
#[derive(DeriveError, Debug)]
#[error("Error while Lexing")]
pub struct LexErr(Lex, LexError, String);
/// Lex result type
#[derive(DeriveError)]
pub struct LexResult<P>(anyhow::Result<SketchyParserBuilder<Initialized, Lexed, P>, LexErr>);
impl<P> LexResult<P> {
    pub fn print_errors(
        self,
        formater: impl Fn(&std::ops::Range<usize>, &Token, &str) -> (),
    ) -> Self {
        let Err(ref errors) = self.0 else {
            return self;
        };
        for (span, err) in &errors.0 {
            formater(&err, &span, &errors.2[err.clone()]);
        }
        self
    }
    pub fn into_result(
        self,
    ) -> anyhow::Result<SketchyParserBuilder<Initialized, Lexed, P>, LexErr> {
        self.0
    }
}
impl<'a> ParserResult<'a> {
    pub fn print_errors(self, formater: fn(&Rich<'a, Token>, &Spanned<Expression>, &str)) -> Self {
        let Err(ref error) = self.0 else {
            return self;
        };
        for err in &error.0 {
            formater(&err, &error.1, &error.2);
        }
        self
    }
    pub fn into_result(
        self,
    ) -> anyhow::Result<SketchyParserBuilder<Initialized, Lexed, Parsed>, ParseErr<'a>> {
        self.0
    }
}
