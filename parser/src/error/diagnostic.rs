use crate::convenience_types::Span;
use std::{fmt::Display, sync::mpsc::Sender};

/// An enum representing a diagnostic level.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum Level {
    /// An error.
    Error,
    /// A warning.
    Warning,
    /// A note.
    Note,
    /// A help message.
    Help,
    /// Some debug information.
    Debug,
}

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Level::Error => write!(f, "Error"),
            Level::Warning => write!(f, "Warning"),
            Level::Note => write!(f, "Note"),
            Level::Help => write!(f, "Help"),
            Level::Debug => write!(f, "Debug"),
        }
    }
}
use core::fmt;

type Label = &'static str;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    /// A specific token was expected.
    Token(crate::Token),
    /// A labelled pattern was expected.
    Label(Label),
    /// The end of input was expected.
    EndOfInput,
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{}", token),
            Self::Label(label) => f.write_str(label),
            Self::EndOfInput => f.write_str("EndOfInput"),
        }
    }
}

impl fmt::Debug for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Token(token) => write!(f, "{token:?}"),
            Self::Label(label) => f.write_str(label),
            Self::EndOfInput => f.write_str("EndOfInput"),
        }
    }
}

// TODO: Maybe should make ExpectedFound encapsulated a bit more
/// The reason for a [`Rich`] error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Reason {
    /// An unexpected input was found
    ExpectedFound {
        /// The tokens expected
        expected: Vec<Pattern>,
        /// The tokens found
        found: Option<crate::Token>,
    },
    /// An error with a custom message
    Custom(Diagnostic),
}

impl Reason {
    /// Return the token that was found by this error reason. `None` implies that the end of input was expected.
    pub fn found(&self) -> Option<crate::Token> {
        match self {
            Self::ExpectedFound { found, .. } => found.clone(),
            Self::Custom(_) => None,
        }
    }

    pub fn take_found(&mut self) -> Option<crate::Token> {
        match self {
            Reason::ExpectedFound { found, .. } => found.take(),
            Reason::Custom(_) => None,
        }
    }

    #[inline]
    pub fn flat_merge(self, other: Self) -> Self {
        match (self, other) {
            (
                Reason::ExpectedFound {
                    expected: mut this_expected,
                    found,
                },
                Reason::ExpectedFound {
                    expected: mut other_expected,
                    ..
                },
            ) => {
                // Try to avoid allocations if we possibly can by using the longer vector
                if other_expected.len() > this_expected.len() {
                    core::mem::swap(&mut this_expected, &mut other_expected);
                }
                for expected in other_expected {
                    if !this_expected[..].contains(&expected) {
                        this_expected.push(expected);
                    }
                }
                Reason::ExpectedFound {
                    expected: this_expected,
                    found,
                }
            }
            (Reason::Custom(this), Reason::Custom(other)) => todo!(),
            (this @ Reason::Custom(_), _) => this,
            (_, other @ Reason::Custom(_)) => other,
        }
    }
}

/// A structure representing a diagnostic message and associated children messages.
#[must_use]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    pub span: Option<Span>,
    pub children: Vec<SubDiagnostic>,
}

impl Diagnostic {
    pub fn new(level: Level, message: impl Into<String>) -> Self {
        Diagnostic {
            level,
            message: message.into(),
            span: None,
            children: vec![],
        }
    }

    pub fn spanned(span: Span, level: Level, message: impl Into<String>) -> Self {
        Diagnostic {
            level,
            message: message.into(),
            span: Some(span),
            children: vec![],
        }
    }

    pub fn with_child(
        mut self,
        spans: impl MultiSpan,
        level: Level,
        message: impl Into<String>,
    ) -> Self {
        self.children.push(SubDiagnostic {
            level,
            message: message.into(),
            spans: spans.into_vec(),
        });
        self
    }

    pub fn emit(self, emitter: Sender<Diagnostic>) {
        emitter
            .send(self)
            .expect("Internal error: Could not emit diagnostic")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SubDiagnostic {
    pub level: Level,
    pub message: String,
    pub spans: Vec<Span>,
}

/// Trait implemented by types that can be converted into a set of `Span`s.
pub trait MultiSpan {
    /// Converts `self` into a `Vec<Span>`.
    fn into_vec(self) -> Vec<Span>;
}

impl MultiSpan for Span {
    fn into_vec(self) -> Vec<Span> {
        vec![self]
    }
}

impl MultiSpan for Vec<Span> {
    fn into_vec(self) -> Vec<Span> {
        self
    }
}

impl MultiSpan for &[Span] {
    fn into_vec(self) -> Vec<Span> {
        self.to_vec()
    }
}
