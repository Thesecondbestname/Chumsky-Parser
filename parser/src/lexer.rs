use crate::{ast, impl_display};
use logos::{Lexer, Logos};
pub type Lex = Vec<(Token, std::ops::Range<usize>)>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Span {
    pub start: i32,
    pub end: i32,
}
/// A Result type holding the successfully lexed tokens and any eventual errors
pub struct LexResult {
    tokens: Lex,
    errors: Vec<((), std::ops::Range<usize>, String)>,
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
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\f]+|(?://[^\r\n]*|/\*[\s\S]*\*/)")]
pub enum Token {
    #[token("+")]
    Add,
    #[token("\n")]
    Newline,
    #[token("=")]
    Assign,
    #[token("!")]
    Bang,
    #[token(".")]
    Period,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(",")]
    Comma,
    #[token("#")]
    Hashtag,
    #[token("/")]
    Div,
    #[token("else")]
    Else,
    #[token("continue")]
    Continue,
    #[token("break")]
    Break,
    #[token("return")]
    Return,
    #[token("enum")]
    Enum,
    #[token("type")]
    /// type
    TypeToken,
    #[token("const")]
    Const,
    #[token("==")]
    Eq,
    #[token("false")]
    False,
    #[token("<=")]
    Gt,
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
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse::<i64>().unwrap(), priority=2)]
    Integer(i64),
    #[regex(r"-?[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().unwrap())]
    r#Float(f64),
    #[token("loop")]
    Loop,
    /// (
    #[token("(")]
    Lparen,
    #[token(">=")]
    Lt,
    /// %
    #[token("%")]
    Mod,
    #[token("?")]
    QuestionMark,
    #[token("*")]
    Mul,
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
    #[token("_")]
    PathSeperator,
    #[token("}")]
    Rbracket,
    /// )
    #[token(")")]
    Rparen,
    #[token(";")]
    Semicolon,
    /// :3
    #[token(":3")]
    StmtCast,
    #[token("struct")]
    Struct,
    #[token("-")]
    Sub,
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

/// .
///
/// # Panics
///
/// Panics if .
///
/// # Errors
///
/// This function will return an error if .
#[must_use]
pub fn lex_arrow_program(inp: &String) -> LexResult {
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
fn parse_escaped_ident(inp: String) -> String {
    inp.trim_start_matches('r').trim_matches('#').to_string()
}

fn parse_escaped_string(inp: String) -> String {
    let raw_inp = inp.trim_start_matches('r').trim_matches('#');
    return raw_inp.trim_matches('\"').to_string();
}
impl_display!(Token, |s: &Token| {
    match s {
        Token::Add => "+".to_string(),
        Token::Newline => "Line break".to_string(),
        Token::Assign => "=".to_string(),
        Token::Bang => "!".to_string(),
        Token::Period => ".".to_string(),
        Token::Colon => ":".to_string(),
        Token::DoubleColon => "::".to_string(),
        Token::Comma => ",".to_string(),
        Token::Hashtag => "#".to_string(),
        Token::Div => "/".to_string(),
        Token::Else => "else".to_string(),
        Token::Continue => "continue".to_string(),
        Token::Break => "break".to_string(),
        Token::Return => "return".to_string(),
        Token::Enum => "enum".to_string(),
        Token::TypeToken => "type".to_string(),
        Token::Const => "const".to_string(),
        Token::Eq => "==".to_string(),
        Token::False => "false".to_string(),
        Token::Gt => ">".to_string(),
        Token::Span(Span { start, end }) => format!("{start}..{end}"),
        Token::Ident(ident) => format!("ident: {ident}"),
        Token::LiteralString(string) => format!(r#""{string}""#),
        Token::If => "if".to_string(),
        Token::Import => "use".to_string(),
        Token::Lbracket => "[".to_string(),
        Token::Integer(int) => format!("{int}"),
        Token::Float(float) => format!("{float}"),
        Token::Loop => "loop".to_string(),
        Token::Lparen => "(".to_string(),
        Token::Lt => "<".to_string(),
        Token::Mod => "%".to_string(),
        Token::QuestionMark => "?".to_string(),
        Token::Mul => "*".to_string(),
        Token::Neq => "!=".to_string(),
        Token::Or => "|| or".to_string(),
        Token::And => "&& and".to_string(),
        Token::Xor => "!|".to_string(),
        Token::PathSeperator => "_".to_string(),
        Token::Rbracket => "]".to_string(),
        Token::Rparen => ")".to_string(),
        Token::Semicolon => ";".to_string(),
        Token::StmtCast => ":3".to_string(),
        Token::Struct => "struct".to_string(),
        Token::Sub => "-".to_string(),
        Token::True => "true".to_string(),
        Token::Type(type_) => format!("{type_:?}"),
        Token::While => "while".to_string(),
        Token::Nothing => "Noting".to_string(),
    }
});
