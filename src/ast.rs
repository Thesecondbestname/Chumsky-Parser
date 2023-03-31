#![allow(dead_code)]
#![allow(unused_variables)]
use core::{f32, fmt};

use crate::Spanned;
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum Token<'source> {
    Op(&'source str),
    PathSeperator,
    Import,
    Delimiter(char),
    Num(i32),
    String(&'source str),
    Struct,
    Enum,
    If,
    Else,
    Fn,
    Loop,
    While,
    Colon,
    Hashtag,
    Bool(bool),
    Ident(&'source str),
    Type(Type),
}

#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    Statement(Box<Statement<'a>>),
    Expression(Box<Expression<'a>>),
}
#[derive(Debug, Clone)]
pub struct Block<'a> {
    instructions: Vec<Instruction<'a>>,
}

#[derive(Clone)]
pub enum Statement<'a> {
    /// Obviously for importing stuff 
    Import(Vec<String>, String),
    /// Whhere Variables are declared! Order is Name,Value and optional Type
    VariableDeclaration(String, Box<Expression<'a>>, Option<Type>),
    /// Where the enum is declared and not where it is constructed
    EnumDeclaration {
        name: String,
        variants: Vec<EnumVariantDeclaration>,
    },
    /// The one where the struct is firstly defined
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
    },
    /// Here is where a function is defined
    FunctionDeclaration {
        name: String,
        return_type: String,
        arguments: Vec<(Type, String)>,
        body: Block<'a>,
    },

    /// This is the one actually constructed
    Struct {
        name: String,
        fields: Vec<(String, Expression<'a>)>,
    },
    /// This is for when the enum is actually constructed
    Enum {
        name: String,
        field: String,
        value: Option<Expression<'a>>,
    },
    /// Obvious If statement. This is a statement as opposed to the IfElse Expression because that
    /// returns something
    IfStatement(Box<Spanned<Expression<'a>>>, Box<Spanned<Expression<'a>>>),
    Loop(Box<Block<'a>>),
    WhileLoop(Expression<'a>, Box<Block<'a>>),
}
#[derive(Debug, Clone)]
pub struct EnumVariantDeclaration {
    name: String,
    value: Option<Type>,
}
#[derive(Debug, Clone)]
pub struct StructField {
    pub(crate) name: String,
    pub(crate) r#type: Type,
}
#[derive(Debug, Clone)]
pub struct Variable<'a> {
    pub name: String,
    pub value: Box<Expression<'a>>,
}
#[derive(Clone)]
pub enum Expression<'a> {
    ParserError,
    Ident(&'a str),
    List(Vec<Expression<'a>>),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Variable(String),
    FunctionCall(Box<Spanned<Self>>, Spanned<Vec<Self>>),
    Block(Block<'a>),
    Value(Value),
    IfElse(Box<Spanned<Self>>, Box<Spanned<Self>>, Box<Spanned<Self>>),
    UnaryBool(Box<Spanned<Self>>),
    UnaryMath(Box<Spanned<Self>>),
    MathOp(Box<Spanned<Self>>, MathOp, Box<Spanned<Self>>),
    MathtComp(Box<Self>, MathtoBinOp, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, BinaryOp, Box<Expression<'a>>),
}

#[derive(Clone)]
/// An enum of all possible values.
pub enum Value {
    String(String),
    Number(Number),
    Array(i64, Vec<Type>),
    Tuple(Vec<Value>),
    Char(char),
    Bool(bool),
    Span(i32, i32),
}
#[derive(Clone)]
pub enum MathOp {
    Add,
    Sub,
    Pow,
    Div,
    Mul,
    AbsDiv,
}
#[derive(Clone)]
pub enum MathtoBinOp {
    Lt,
    Gt,
    Lte,
    Gte,
}
#[derive(Clone)]
pub enum BinaryOp {
    Neq,
    Eq,
    And,
    Or,
    Nand,
    Xor,
}
/// All the type primitives. These do not contain values, rather they denote that a given type
/// belongs here.
#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord)]
pub enum Type {
    Int,
    Bool,
    Float,
    String,
    Array(Box<Type>,i32),
    Tuple(Vec<Type>),
    Char,
    Span,
    Inferred,
}
#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}
#[derive(Debug, Clone)]
pub struct Span {
    pub(crate) begin: Number,
    pub(crate) end: Number,
}
crate::impl_debug!(Statement, |s: &Statement| {
    match s {
        Statement::VariableDeclaration(n, b, _) => format!("(Variable: {:?} = {:?})", n, b),
        _ => "Not implemented...".to_string(),
    }
});
crate::impl_debug_nolifetime!(MathOp, |s: &MathOp| match s {
    MathOp::Add => format!("+"),
    MathOp::Sub => format!("-"),
    MathOp::Pow => format!("**"),
    MathOp::Div => format!("/"),
    MathOp::Mul => format!("*"),
    MathOp::AbsDiv => format!("%"),
});
crate::impl_debug!(Expression, |s: &Expression| {
    match s {
        Expression::MathOp(a, b, c) => {
            format!("(Mathematical Operation: {:?} {:?} {:?})", a, b, c)
        }
        Expression::Value(a) => format!("{:?}", a),
        _ => format!("Not implemented..."),
    }
});
crate::impl_debug_nolifetime!(Value, |s: &Value| {
    match s {
        Self::String(arg0) => arg0.to_string(),
        Self::Number(arg0) => match arg0 {
            Number::Int(n) => format!("{}", n),
            Number::Float(n) => format!("{}", n),
        },
        Self::Array(arg0, arg1) => format!(r#""{:?}", length {:?}"#, arg1, arg0),
        Self::Tuple(arg0) => format!("{:?}", arg0),
        Self::Char(arg0) => arg0.to_string(),
        Self::Bool(arg0) => arg0.to_string(),
        Self::Span(begin, end) => format!(r#"from {:?} to {:?}"#, begin, end),
    }
});

impl Number {
    pub fn from_i32(num: i32) -> Number {
        Number::Int(num.into())
    }
    pub fn from_f32(num: f32) -> Number {
        Number::Float(num.into())
    }
}
impl<'inp> Expression<'inp> {
    /// Converts an Expression To an Instruction
    pub fn to_instruction(self) -> Instruction<'inp> {
        Instruction::Expression(Box::new(self))
    }
}
impl<'inp> Statement<'inp> {
    pub fn to_instruction(self) -> Instruction<'inp> {
        Instruction::Statement(Box::new(self))
    }
}
#[macro_export]
macro_rules! impl_debug {
    ($struct_name:ident, $write_calls:expr) => {
        impl std::fmt::Debug for $struct_name<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", &$write_calls(&self))
            }
        }
    };
}
/// used to implement the to_Expression trait.
#[macro_export]
macro_rules! impl_to_Instruction {
    ($name: ident, $closure :expr) => {
        impl<'Inp> ToInstruction<'Inp> for $name<'Inp> {
            fn to_Instruction(&self) -> Instruction<'Inp> {
                $closure(self.clone()) 
            }
        }
    };
}
#[macro_export]
macro_rules! impl_to_Instruction_nolife {
    ($name: ident, $closure :expr) => {
        impl ToInstruction for $name {
            fn to_Instruction(&self) -> Instruction {
                $closure(self.clone())
            }
        }
    };
}
#[macro_export]
macro_rules! impl_debug_nolifetime {
    ($struct_name:ident, $write_calls:expr) => {
        impl std::fmt::Debug for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", &$write_calls(&self))
            }
        }
    };
}
/// Trait that turns something arbitrary into an Expression.
pub trait ToInstruction<'inp> {
    fn to_Instruction(&self) -> Instruction<'inp>;
}
pub fn i32_to_f32(num1: i32, num2: i32) -> f64 {
    let result = format!("{}.{}", num1, num2);
    result.parse::<f64>().unwrap()
}
