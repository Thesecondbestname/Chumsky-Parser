use crate::ast;
use logos::{Lexer, Logos};
pub type Lex = Vec<(Token, std::ops::Range<usize>)>;

#[derive(Debug, PartialEq, Clone)]
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
        LexResult { tokens, errors }
    }
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
    pub fn tokens(&self) -> &Vec<(Token, std::ops::Range<usize>)> {
        &self.tokens
    }
    pub fn errors(&self) -> &Vec<((), std::ops::Range<usize>, String)> {
        &self.errors
    }
}
impl Default for LexResult {
    fn default() -> Self {
        LexResult {
            tokens: vec![],
            errors: vec![],
        }
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
    TypeKey,
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
}

fn type_matcher(lex: &mut Lexer<Token>) -> ast::Type {
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
