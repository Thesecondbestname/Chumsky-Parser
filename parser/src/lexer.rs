use crate::{ast, impl_display};
use logos::{Lexer, Logos};
pub type Lex = Vec<(Token, std::ops::Range<usize>)>;
pub type LexError = Vec<((), std::ops::Range<usize>, String)>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    pub start: i32,
    pub end: i32,
}
/// A Result type holding the successfully lexed tokens and any eventual errors
pub struct LexResult {
    tokens: Lex,
    errors: LexError,
}
impl LexResult {
    fn new(tokens: Lex, errors: Vec<((), std::ops::Range<usize>, String)>) -> Self {
        Self { tokens, errors }
    }
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
    pub const fn tokens(&self) -> &Vec<(Token, std::ops::Range<usize>)> {
        &self.tokens
    }
    pub const fn errors(&self) -> &Vec<((), std::ops::Range<usize>, String)> {
        &self.errors
    }
    /// Converts the lex to a result, consuming and returning the Lex if no errors exist and a tuple of lex and errors otherwise
    pub fn to_result(self) -> Result<Lex, (Lex, LexError)> {
        if self.is_ok() {
            Ok(self.tokens)
        } else {
            Err((self.tokens, self.errors))
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\f]+|(?://.*\n|/\*[\s\S]*\*/)")]
pub enum Token {
    #[token("+")]
    Plus,
    #[token("\n")]
    Newline,
    #[token("=")]
    Assign,
    #[token("!")]
    Bang,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(",")]
    Comma,
    #[token("#")]
    Hashtag,
    #[token("/")]
    Slash,
    #[token("else")]
    Else,
    #[token("trait")]
    Trait,
    #[token("then")]
    Then,
    #[token("Self")]
    Self_,
    #[token("continue")]
    Continue,
    #[token("impl")]
    Impl,
    #[token("break")]
    Break,
    #[token("return")]
    Return,
    #[token("enum")]
    Enum,
    #[token("==")]
    Eq,
    #[token("match")]
    Match,
    #[token("false")]
    False,
    #[token("<")]
    Gt,
    #[token("<=")]
    Gte,
    #[token("..")]
    DoubleDot,
    #[regex(r"(\d+)\.\.(\d+)", |lex| parse_span(lex.slice().to_string()))]
    Span(Span),
    #[regex("[a-zA-Z_öäü][a-zA-Z0-9_öäü]*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| lex.slice().to_string())]
    LiteralString(String),
    #[token("if")]
    If,
    #[token("use")]
    Import,
    #[token("{")]
    Lbracket,
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap(), priority=2)]
    Integer(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    r#Float(f64),
    #[token("loop")]
    Loop,
    /// (
    #[token("(")]
    Lparen,
    /// >=
    #[token(">=")]
    Lte,
    #[token(">")]
    Lt,
    /// %
    #[token("%")]
    Mod,
    #[token("?")]
    QuestionMark,
    #[token("*")]
    Mul,
    ///!=
    #[token("!=")]
    Neq,
    #[token("or")]
    #[token("||")]
    Or,
    #[token("and")]
    #[token("&&")]
    And,
    #[token("!|")]
    #[token("xor")]
    Xor,
    /// }
    #[token("}")]
    Rbracket,
    /// ]
    #[token("]")]
    Rbucket,
    /// [
    #[token("[")]
    Lbucket,
    /// )
    #[token(")")]
    Rparen,
    #[token(";")]
    Semicolon,
    #[token("struct")]
    Struct,
    #[token("-")]
    Minus,
    #[token("true")]
    True,
    #[regex("bool|float|int|char|string", type_matcher)]
    Type(ast::Type),
    #[token("while")]
    While,
    Nothing,
}

fn type_matcher(lex: &Lexer<Token>) -> ast::Type {
    let slice = lex.slice();
    match slice {
        "bool" => ast::Type::Bool,
        "int" => ast::Type::Int,
        "float" => ast::Type::Float,
        "string" => ast::Type::String,
        "char" => ast::Type::Char,
        _ => unreachable!(),
    }
}

#[derive(Logos, Clone, Debug, PartialEq, PartialOrd)]
pub enum Number {
    #[regex(".*")]
    None,
}
impl Eq for Number {}

/// # Panics
///
/// Panics if I misused unwrap here.
///
/// # Errors
///
/// This function will return an error if Lexing was unsuccessful.
#[must_use]
pub fn lex_sketchy_program(inp: &String) -> LexResult {
    let parse = Token::lexer(&inp[..]);
    let (token, err): (Vec<_>, Vec<_>) = parse.clone().spanned().partition(|token| token.0.is_ok());
    let result = LexResult::new(
        token
            .into_iter()
            .map(|token| (token.0.unwrap(), token.1))
            .collect(),
        err.iter()
            .map(|(error, span)| {
                (
                    error.clone().err().unwrap(),
                    span.clone(),
                    parse.slice().to_owned(),
                )
            })
            .collect::<Vec<((), std::ops::Range<usize>, String)>>(),
    );
    return result;
}
fn parse_span(inp: String) -> Span {
    let nums = inp
        .split_once("..")
        .expect("Lexer Error: Span regex fucked");
    Span {
        start: nums.0.parse().expect("Lexer Error: Span regex fucked"),
        end: nums.1.parse().expect("Lexer Error: Span regex fucked"),
    }
}

impl_display!(Token, |s: &Token| {
    match s {
        Token::Plus => "+".to_owned(),
        Token::Newline => "Newline".to_owned(),
        Token::Assign => "=".to_owned(),
        Token::Bang => "!".to_owned(),
        Token::Dot => ".".to_owned(),
        Token::Colon => ":".to_owned(),
        Token::DoubleColon => "::".to_owned(),
        Token::Comma => ",".to_owned(),
        Token::Hashtag => "#".to_owned(),
        Token::Slash => "/".to_owned(),
        Token::Else => "else".to_owned(),
        Token::Continue => "continue".to_owned(),
        Token::Break => "break".to_owned(),
        Token::Return => "return".to_owned(),
        Token::Enum => "enum".to_owned(),
        Token::Eq => "==".to_owned(),
        Token::False => "false".to_owned(),
        Token::Gt => ">".to_owned(),
        Token::Span(Span { start, end }) => format!("{start}..{end}"),
        Token::Ident(ident) => format!("ident: {ident}"),
        Token::LiteralString(string) => format!(r#""{string}""#),
        Token::If => "if".to_owned(),
        Token::Import => "use".to_owned(),
        Token::Lbracket => "{".to_owned(),
        Token::Integer(int) => format!("{int}"),
        Token::Float(float) => format!("{float}"),
        Token::Loop => "loop".to_owned(),
        Token::Lparen => "(".to_owned(),
        Token::Lt => "<".to_owned(),
        Token::Mod => "%".to_owned(),
        Token::QuestionMark => "?".to_owned(),
        Token::Mul => "*".to_owned(),
        Token::Neq => "!=".to_owned(),
        Token::Or => "|| or".to_owned(),
        Token::And => "&& and".to_owned(),
        Token::Xor => "!|".to_owned(),
        Token::Rbracket => "}".to_owned(),
        Token::Rparen => ")".to_owned(),
        Token::Semicolon => ";".to_owned(),
        Token::Struct => "struct".to_owned(),
        Token::Minus => "-".to_owned(),
        Token::True => "true".to_owned(),
        Token::Type(type_) => format!("{type_:?}"),
        Token::While => "while".to_owned(),
        Token::Nothing => "Noting".to_owned(),
        Token::Lte => "<=".to_owned(),
        Token::Gte => ">=".to_owned(),
        Token::Match => "match".to_owned(),
        Token::Then => "then".to_owned(),
        Token::Impl => "impl".to_owned(),
        Token::DoubleDot => "..".to_owned(),
        Token::Rbucket => "]".to_owned(),
        Token::Lbucket => "[".to_owned(),
        Token::Self_ => "self".to_owned(),
        Token::Trait => "trait".to_owned(),
    }
});
