#![allow(dead_code)]
#![allow(unused_variables)]
use core::{f32, fmt};
pub enum Lexer {
    LParen,
    RParen,
    Semicolon,
    Num(i32),
    Dot,
    String(String),
    Fn,
    Loop,
    While,
    Eq,
    Colon,
    Bang,
    Hashtag,
    Lt,
    Gt,
}
#[derive(Debug, Clone)]
pub enum Instruction {
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}
#[derive(Debug, Clone)]
pub struct Block {
    instructions: Vec<Instruction>,
    return_type: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    /// Various declarations. These should be kept track of.
    Import {
        module: Vec<String>,
        name: String,
    },
    VariableDeclaration(Variable),
    EnumDeclaration {
        name: String,
        variants: Vec<EnumVariantDeclaration>,
    },
    StructDeclaration {
        name: String,
        fields: Vec<StructFieldDeclaration>,
    },
    FunctionDeclaration {
        name: String,
        return_type: String,
        arguments: Vec<(Type, String)>,
        body: Block,
    },

    /// Now the ones that are actually constructed within the code. These should also be kept track of.
    Struct {
        name: String,
        fields: Vec<(String, Expression)>,
    },
    Enum {
        name: String,
        field: String,
        value: Option<Expression>,
    },
    FunctionCall(FunctionCall),
    /// Various control flow statements. These live in the moment and can be forgotten, because the get folded away anyway.
    IfStatement {
        condition: Expression,
        then_branch: Box<Block>,
        else_branch: Option<Box<Block>>,
    },
    Loop {
        block: Box<Block>,
    },
    WhileLoop {
        expression: Expression,
        block: Box<Block>,
    },
}
#[derive(Debug, Clone)]
pub struct EnumVariantDeclaration {
    name: String,
    value: Option<Type>,
}
#[derive(Debug, Clone)]
pub struct StructFieldDeclaration {
    name: String,
    r#type: Type,
}
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub r#type: Type,
    pub value: Box<Expression>,
}
#[derive(Clone)]
pub struct Expression {
    type_of_expression: ExpressionType,
    pub return_type: Type,
}
#[derive(Clone)]
pub enum ExpressionType {
    Statement(Box<Statement>),
    Variable(String),
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
        return_type: Type,
    },
    Block(Block),
    MathExpr(MathExpression),
    Value(Value),
    MathToBool(MathtoBinaryOperation),
    BoolToBool(BinaryBinaryOperation),
    UnaryBool(Box<Expression>),
    UnaryMath(Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    DevEq(Box<Expression>, Box<Expression>),
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
    Span(Span),
}
#[derive(Debug, Clone)]
pub struct FunctionCall {
    name: String,
    arguments: Vec<Expression>,
    return_type: Type,
}
#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Float,
    String,
    Array,
    Tuple,
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
    begin: Number,
    end: Number,
}
/// Enum used to hold mathematical operations.
#[derive(Debug, Clone)]
pub enum MathExpression {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    DevEq(Box<Expression>, Box<Expression>),
}
#[derive(Clone)]
/// Operators taking bools and returning bools. Elsewhere called booleand extenders
enum BinaryBinaryOperation {
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Nand(Box<Expression>, Box<Expression>),
    Xor(Box<Expression>, Box<Expression>),
}
#[derive(Clone)]
/// Operator that returns a boolean but takes anything
enum MathtoBinaryOperation {
    Lt(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Eq(Box<Expression>, Box<Expression>),
    Neq(Box<Expression>, Box<Expression>),
    LtE(Box<Expression>, Box<Expression>),
    GtE(Box<Expression>, Box<Expression>),
}

crate::impl_debug!(ExpressionType, |s: &ExpressionType| {
    match s {
        ExpressionType::Add(a, b) => format!("Add{{{:?} , {:?}}}", a, b),
        ExpressionType::Sub(a, b) => format!("Sub{{{:?} , {:?}}}", a, b),
        ExpressionType::Mul(a, b) => format!("{:?} * {:?}", a, b),
        ExpressionType::Div(a, b) => format!("{:?} / {:?}", a, b),
        ExpressionType::Pow(a, b) => format!("{:?} ^ {:?}", a, b),
        ExpressionType::DevEq(a, b) => format!("{:?} % {:?}", a, b),
        ExpressionType::Value(a) => format!("{:?}", a),
        _ => format!("Not implemented..."),
    }
});
crate::impl_debug!(MathtoBinaryOperation, |s: &MathtoBinaryOperation| {
    match s {
        MathtoBinaryOperation::Lt(lhs, rhs) => "<",
        MathtoBinaryOperation::Gt(lhs, rhs) => ">",
        MathtoBinaryOperation::Eq(lhs, rhs) => "=",
        MathtoBinaryOperation::Neq(lhs, rhs) => "!=",
        MathtoBinaryOperation::LtE(lhs, rhs) => "<=",
        MathtoBinaryOperation::GtE(lhs, rhs) => ">=",
    }
});
crate::impl_debug!(BinaryBinaryOperation, |_| {
    "Boolean operator that takes in a boolean, like && or and"
});
crate::impl_debug!(Value, |s: &Value| {
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
        Self::Span(span) => format!(r#"from {:?} to {:?}"#, span.begin, span.end),
    }
});
crate::impl_debug!(Expression, |s: &Expression| (format!(
    "{:?} -> {:?}",
    s.clone().type_of_expression,
    s.clone().return_type
)));

impl Expression {
    pub fn to_UnaryMathExpression(self) -> Expression {
        ExpressionType::UnaryMath(Box::new(self)).to_Expression(Type::Int)
    }
    /// Converts an Expression To an Instruction
    pub fn to_instruction(self) -> Instruction {
        Instruction::Expression(Box::new(self))
    }
}
impl Statement {
    pub fn to_instruction(self) -> Instruction {
        Instruction::Statement(Box::new(self))
    }
}
crate::impl_to_Expression!(Statement, |s, ret| ExpressionType::Statement(Box::new(s))
    .to_Expression(Type::Inferred));
crate::impl_to_Expression!(Value, |s: Value, ret| ExpressionType::Value(s)
    .to_Expression(ret));
crate::impl_to_Expression!(Variable, |s: Variable, ret| {
    ExpressionType::Variable(s.name).to_Expression(ret)
});
crate::impl_to_Expression!(ExpressionType, |s, ret| {
    Expression {
        type_of_expression: s,
        return_type: ret,
    }
});
#[macro_export]
macro_rules! impl_debug {
    ($struct_name:ident, $write_calls:expr) => {
        impl std::fmt::Debug for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", &$write_calls(&self))
            }
        }
    };
}
/// used to implement the to_Expression trait.
#[macro_export]
macro_rules! impl_to_Expression {
    ($name: ident, $closure :expr) => {
        impl ToExpression for $name {
            fn to_Expression(&self, return_type: Type) -> Expression {
                $closure(self.clone(), return_type)
            }
        }
    };
}
/// Trait that turns something arbitrary into an Expression.
pub trait ToExpression {
    fn to_Expression(&self, return_type: Type) -> Expression;
}
pub fn i32_to_f32(num1: i32, num2: i32) -> f64 {
    let result = format!("{}.{}", num1, num2);
    result.parse::<f64>().unwrap()
}
