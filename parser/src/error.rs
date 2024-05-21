mod diagnostic;
use crate::{
    convenience_types::ParserInput,
    span::{SourceId, Span},
    Token,
};
use chumsky::{
    util::{Maybe, MaybeRef},
    ParseResult,
};
pub use diagnostic::{Diagnostic, Level};

use diagnostic::{Pattern, Reason};

type Label = &'static str;

/// A rich default error type that tracks error spans, expected inputs, and the actual input found at an error site.
#[derive(Clone, Debug)]
pub struct ParseError {
    pub reason: Reason,
    span: Span,
    context: Vec<(Label, Span)>,
}

impl ParseError {
    /// Create an error with a custom message and span
    #[inline]
    pub fn custom<M: ToString>(span: Span, msg: M) -> Self {
        ParseError {
            span,
            reason: Reason::Custom(Diagnostic::new(Level::Error, msg.to_string())),
            context: Vec::new(),
        }
    }

    /// Get the span associated with this error.
    pub fn span(&self) -> Span {
        self.span
    }

    /// Get the reason for this error.
    pub fn reason(&self) -> &Reason {
        &self.reason
    }

    /// Get the token found by this error when parsing. `None` implies that the error expected the end of input.
    pub fn found(&self) -> Option<Token> {
        self.reason.found()
    }

    /// Return an iterator over the labelled contexts of this error, from least general to most.
    ///
    /// 'Context' here means parser patterns that the parser was in the process of parsing when the error occurred. To
    /// add labelled contexts, see [`Parser::labelled`].
    pub fn contexts(&self) -> impl Iterator<Item = (&Label, &Span)> {
        self.context.iter().map(|(l, s)| (l, s))
    }

    /// Get an iterator over the expected items associated with this error
    pub fn expected(&self) -> impl ExactSizeIterator<Item = &Pattern> {
        fn push_expected<'a>(reason: &'a Reason, v: &mut Vec<&'a Pattern>) {
            match reason {
                Reason::ExpectedFound { expected, .. } => v.extend(expected.iter()),
                Reason::Custom(_) => {}
            }
        }
        let mut v = Vec::new();
        push_expected(&self.reason, &mut v);
        v.into_iter()
    }
}

impl<'a, 'src> chumsky::error::Error<'a, ParserInput<'a, 'src>> for ParseError {
    #[inline]
    fn expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, Token>>>>(
        expected: E,
        found: Option<Maybe<Token, &'a Token>>,
        span: Span,
    ) -> Self {
        Self {
            span,
            reason: Reason::ExpectedFound {
                expected: expected
                    .into_iter()
                    .map(|tok| {
                        tok.map(|inner| Pattern::Token(inner.into_inner()))
                            .unwrap_or(Pattern::EndOfInput)
                    })
                    .collect(),
                found: found.map(|inner| inner.into_inner()),
            },
            context: Vec::new(),
        }
    }

    #[inline]
    fn merge(mut self, mut other: Self) -> Self {
        let new_reason = self.reason.flat_merge(other.reason);
        Self {
            span: self.span,
            reason: new_reason,
            // TODO Merging contexts correctly?
            context: {
                self.context.append(&mut other.context);
                self.context.dedup_by_key(|(label, _)| *label);
                self.context
            },
        }
    }

    #[inline]
    fn merge_expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, Token>>>>(
        mut self,
        new_expected: E,
        _found: Option<Maybe<Token, &'a Token>>,
        _span: Span,
    ) -> Self {
        match &mut self.reason {
            Reason::ExpectedFound { expected, found: _ } => {
                for new_expected in new_expected {
                    let new_expected = new_expected
                        .map(|inner| Pattern::Token(inner.into_inner()))
                        .unwrap_or(Pattern::EndOfInput);
                    if !expected[..].contains(&new_expected) {
                        expected.push(new_expected);
                    }
                }
            }
            Reason::Custom(_) => todo!(),
        }
        // TOOD: Merge contexts
        self
    }

    #[inline]
    fn replace_expected_found<E: IntoIterator<Item = Option<MaybeRef<'a, Token>>>>(
        mut self,
        new_expected: E,
        new_found: Option<MaybeRef<'a, Token>>,
        span: Span,
    ) -> Self {
        self.span = span;
        match &mut self.reason {
            Reason::ExpectedFound { expected, found } => {
                expected.clear();
                expected.extend(new_expected.into_iter().map(|tok| {
                    tok.map(|inner| Pattern::Token(inner.into_inner()))
                        .unwrap_or(Pattern::EndOfInput)
                }));
                *found = new_found.map(|inner| inner.into_inner());
            }
            _ => {
                self.reason = Reason::ExpectedFound {
                    expected: new_expected
                        .into_iter()
                        .map(|tok| {
                            tok.map(|inner| Pattern::Token(inner.into_inner()))
                                .unwrap_or(Pattern::EndOfInput)
                        })
                        .collect(),
                    found: new_found.map(|inner| inner.into_inner()),
                };
            }
        }
        self.context.clear();
        self
    }
}

impl<'a, 'src> chumsky::label::LabelError<'a, ParserInput<'a, 'src>, Label> for ParseError {
    #[inline]
    fn label_with(&mut self, label: Label) {
        // Opportunistically attempt to reuse allocations if we can
        match &mut self.reason {
            Reason::ExpectedFound { expected, found: _ } => {
                expected.clear();
                expected.push(Pattern::Label(label));
            }
            _ => {
                self.reason = Reason::ExpectedFound {
                    expected: vec![Pattern::Label(label)],
                    found: self.reason.take_found(),
                };
            }
        }
    }

    #[inline]
    fn in_context(&mut self, label: Label, span: Span) {
        if self.context.iter().all(|(l, _)| l != &label) {
            self.context.push((label, span));
        }
    }
}
pub fn errors_to_diagnostics<T: std::fmt::Debug>(
    parse_result: ParseResult<T, ParseError>,
    src_id: SourceId,
) -> (Option<T>, Vec<Diagnostic>) {
    let error_to_diagostic = |err: ParseError| -> Diagnostic {
        use crate::error::Reason;
        let mut span = err.span().with_id(src_id);
        // TODO Why are these spans generated?
        // The input from the tokenizer looks correct...
        if span.end < span.start {
            std::mem::swap(&mut span.start, &mut span.end);
        }

        match err.reason {
            Reason::ExpectedFound { expected, found } => match (expected.is_empty(), found.clone())
            {
                (true, _) => report_unexpected(span, found),
                (false, None) => report_expected(span, expected),
                (false, Some(found)) => report_expected_found(span, expected, found),
            },
            Reason::Custom(diagnostic) => diagnostic,
        }
    };

    let (output, errors) = parse_result.into_output_errors();
    (output, errors.into_iter().map(error_to_diagostic).collect())
}

fn report_expected_found(span: Span, expected: Vec<Pattern>, found: Token) -> Diagnostic {
    let patterns = patterns_to_string(expected);
    Diagnostic::new(
        Level::Error,
        format!("Expected {patterns}, found {}", found),
    )
    .with_child(span, Level::Error, format!("Expected {patterns}"))
}

fn report_expected(span: Span, expected: Vec<Pattern>) -> Diagnostic {
    let message = format!("Expected {}", patterns_to_string(expected));
    Diagnostic::spanned(span, Level::Error, message)
}

fn report_unexpected(span: Span, found: Option<Token>) -> Diagnostic {
    let message = format!(
        "Unexpected {}",
        found
            .map(|token| token.to_string())
            .unwrap_or("end of input".to_owned())
    );
    Diagnostic::spanned(span, Level::Error, message)
}

fn patterns_to_string(patterns: Vec<Pattern>) -> String {
    if patterns.is_empty() {
        return "nothing".into();
    };

    use std::collections::HashSet;
    let mut patterns: HashSet<&Pattern> = HashSet::from_iter(patterns.iter());

    fn replace_subset(
        super_set: &mut HashSet<&Pattern>,
        search: &'static [Pattern],
        replacement: &'static Pattern,
    ) {
        let search_set = HashSet::from_iter(search);
        if search_set.is_subset(super_set) {
            for pattern in search_set {
                super_set.remove(pattern);
            }
            super_set.insert(replacement);
        }
    }

    fn replace_element(
        haystack: &mut HashSet<&Pattern>,
        needle: &'static Pattern,
        replacement: &'static Pattern,
    ) {
        if haystack.remove(needle) {
            haystack.insert(replacement);
        }
    }

    replace_subset(
        &mut patterns,
        &[
            Pattern::Token(Token::Eq),
            Pattern::Token(Token::Neq),
            Pattern::Token(Token::Lt),
            Pattern::Token(Token::Lte),
            Pattern::Token(Token::Gt),
            Pattern::Token(Token::Gte),
        ],
        &Pattern::Label("a comparison operator"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Token(Token::Slash),
            Pattern::Token(Token::Mul),
            Pattern::Token(Token::Plus),
            Pattern::Token(Token::Minus),
        ],
        &Pattern::Label("an arithmetic operator"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Token(Token::Assign),
            // TODO Add others
        ],
        &Pattern::Label("an assignment operator"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Label("a comparison operator"),
            Pattern::Label("an arithmetic operator"),
            Pattern::Label("an assignment operator"),
        ],
        &Pattern::Label("an operator"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Label("literal"),
            Pattern::Token(Token::Lparen),
            Pattern::Token(Token::Lbracket),
            Pattern::Token(Token::Break),
            Pattern::Token(Token::Return),
        ],
        &Pattern::Label("expression"),
    );

    replace_subset(
        &mut patterns,
        &[
            Pattern::Label("expression"),
            Pattern::Token(Token::Bang),
            Pattern::Token(Token::Minus),
        ],
        &Pattern::Label("expression"),
    );

    let mut patterns = patterns.into_iter().collect::<Vec<_>>();
    let (last, start) = patterns.split_last().unwrap();
    format!(
        "{}{}{last}",
        start
            .into_iter()
            .fold(String::new(), |acc, a| format!("{acc}, {a}")),
        if start.is_empty() { "" } else { " or " }
    )
}
