#![allow(unused)]
use crate::convenience_types::Spanned;

#[derive(Debug, Clone)]
pub enum Instruction {
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum BlockElement {
    Item(Spanned<Item>),
    Statement(Spanned<Statement>),
    /// An expression cast to a statement with a :3
    SilentExpression(Spanned<Expression>),
}

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Spanned<BlockElement>>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path(pub Vec<Spanned<String>>);

#[derive(Debug, Clone)]
pub enum Item {
    //TODO: Read span here
    Function(FunctionDeclaration),
    Import(Spanned<Import>),
    Enum(Spanned<EnumDeclaration>),
    Struct(Spanned<StructDeclaration>),
}
#[derive(Debug, Clone)]
/// Here is where a function is defined
pub struct FunctionDeclaration {
    pub name: Spanned<String>,
    pub return_type: Spanned<Type>,
    pub arguments: Vec<Spanned<(Spanned<Type>, Spanned<String>)>>,
    pub body: Spanned<Expression>,
}
#[derive(Debug, Clone)]
/// Obviously for importing stuff
pub struct Import(pub Vec<Spanned<String>>, pub Spanned<String>);

#[derive(Debug, Clone)]
/// Where the enum is declared and not where it is constructed
pub struct EnumDeclaration {
    pub name: String,
    pub variants: Vec<Spanned<EnumVariantDeclaration>>,
}
/// A variant of an enum that is currently declared. I.e. an arm.
#[derive(Debug, Clone)]
pub struct EnumVariantDeclaration {
    pub name: String,
    pub r#type: Option<Type>,
}
#[derive(Debug, Clone)]
/// The one where the struct is firstly defined
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<Spanned<StructField>>,
}
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Spanned<String>,
    pub r#type: Spanned<Type>,
}
#[derive(Debug, Clone)]
pub enum Statement {
    ParserError,
    /// Whhere Variables are declared! Order is Name,Value but without optional Type
    VariableDeclaration(String, Box<Spanned<Expression>>),
    /// Control Flow! This shit doesn't return anything, rather the expression inside
    Break(Box<Spanned<Expression>>),
    /// Loop statement, takes only a codeblock. Might consider making this an expression giving the value after return.
    Loop(Spanned<Expression>),
    /// return statements are.... STATEMENTS woo your good at this.
    Return(Box<Spanned<Expression>>),
    // Becomes a bit more obvious that they shouldn't return anything when looking at continue.
    Continue,
    /// This is the one where a struct is actually constructed
    WhileLoop(Spanned<Expression>, Vec<Spanned<Expression>>),
    /// An expression whose return type is ignored is a statement
    Expression(Expression),
}
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub value: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct If {
    pub(crate) condition: Box<Spanned<Expression>>,
    pub(crate) code_block: Spanned<Expression>,
}
#[derive(Clone, Debug)]
pub enum Expression {
    ParserError,
    If(Box<If>),
    Ident(String),
    List(Vec<Self>),
    FunctionCall(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    MethodCall(Box<Spanned<Self>>, String, Vec<Spanned<Self>>),
    Block(Block),
    Value(Value),
    FieldAccess(Box<Spanned<Self>>, Spanned<String>),
    Else(Box<Spanned<Self>>, Box<Spanned<Self>>),
    UnaryBool(Box<Spanned<Self>>),
    UnaryMath(Box<Spanned<Self>>),
    MathOp(Box<Spanned<Self>>, MathOp, Box<Spanned<Self>>),
    Comparison(Box<Spanned<Self>>, ComparisonOp, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Unit,
}

#[derive(Clone, Debug)]
/// An enum of all possible values. A type can have
pub enum Value {
    String(String),
    Number(Number),
    Array(i64, Vec<Type>),
    Tuple(Vec<Value>),
    Char(char),
    Bool(bool),
    Span(i32, i32),
    Option(Box<Expression>),
    Struct {
        name: String,
        fields: Vec<(String, Expression)>,
    },
    /// This is for when the enum is actually constructed
    Enum {
        name: String,
        field: String,
        value: Option<Box<Expression>>,
    },
}
#[derive(Clone, Debug)]
pub enum MathOp {
    Add,
    Sub,
    Div,
    Mul,
}
#[derive(Clone, Debug)]
pub enum ComparisonOp {
    Lt,
    Gt,
    Lte,
    Gte,
    Neq,
    Eq,
}
#[derive(Clone, Debug)]
pub enum BinaryOp {
    And,
    Or,
    Xor,
}
/// All the type primitives. These do not contain values, rather they denote that a given type
/// belongs here.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Float,
    String,
    Array(Box<Type>, i64),
    Tuple(Vec<Type>),
    Char,
    Span,
    Path(Spanned<Path>),
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
crate::impl_display!(Value, |s: &Value| match s {
    Value::String(string) => format!("{string}"),
    Value::Number(Number::Int(int)) => format!("{int}"),
    Value::Number(Number::Float(float)) => format!("{}", float),
    Value::Array(len, vals) => format!("{{len:{} [{:?}]}}", len, vals),
    Value::Tuple(vals) => format!("({:?})", vals),
    Value::Char(char) => format!("'{}'", char),
    Value::Bool(bool) => format!("{}", bool),
    Value::Span(start, end) => format!("{}..{}", start, end),
    Value::Option(val) => format!("{}?", val),
    Value::Struct { name, fields } => format!(
        "{} {{{}}}",
        name,
        fields
            .iter()
            .map(|(name, field)| format!("{}: {}", name, field))
            .collect::<Vec<_>>()
            .join(",")
    ),
    Value::Enum { name, field, value } => format!("{} {{{}}}", name, field),
});
crate::impl_display!(MathOp, |s: &MathOp| match s {
    MathOp::Add => "+".to_string(),
    MathOp::Sub => "-".to_string(),
    MathOp::Div => "/".to_string(),
    MathOp::Mul => "*".to_string(),
});
crate::impl_display!(Expression, |s: &Expression| {
    match s {
        Expression::MathOp(a, b, c) => {
            format!("({:?} {} {})", b, a.0, c.0)
        }
        Expression::Value(a) => format!("{a}"),
        Expression::Ident(a) => format!("<{a}>"),
        Expression::ParserError => "Error".to_string(),
        Expression::FunctionCall(called, args) => format!("call ({} ({args:?})", called.0),
        Expression::MethodCall(on, name, args) => {
            format!("call ({} on {} ({args:?}) )", name, on.0)
        }
        Expression::Block(block) => format!("{block}"),
        Expression::If(if_) => format!("{if_}"),
        Expression::Comparison(lhs, op, rhs) => format!("({} {op:?} {})", lhs.0, rhs.0),
        Expression::Binary(a, op, b) => format!("({:?} {} {})", op, a.0, b.0),
        fuck => format!("Not yet implemented to display {fuck:#?}"),
    }
});

crate::impl_display!(Block, |s: &Block| {
    if s.0.is_empty() {
        return String::new();
    }
    s.0.iter()
        .fold(String::new(), |acc, elem| acc + &format!("{}", elem.0))
});
crate::impl_display!(If, |s: &If| {
    let If {
        condition,
        code_block: blocc,
    } = s;
    format!("if {} then {})", condition.0, blocc.0)
});
crate::impl_display!(BlockElement, |s: &BlockElement| {
    match s {
        BlockElement::Item(item) => format!("{}", item.0),
        // HACK: THIS IS EXTREEEMELY VOLATILE it will overflow the stack if captured
        BlockElement::Statement(stmt) => format!("{}", stmt.0),
        BlockElement::SilentExpression(expr) => format!("{}", expr.0),
        fuck => format!("BlockElement doesn't have a display impl \n{fuck:#?}"),
    }
});
crate::impl_display!(Item, |s: &Item| {
    match s {
        Item::Function(FunctionDeclaration {
            name,
            return_type,
            arguments,
            body,
        }) => format!(
            "{}({arguments:?}) -> {:?} ({})",
            name.0, return_type.0, body.0
        ),
        Item::Import(imp) => format!("import ({:?} {})", imp.0 .0, imp.0 .1 .0),
        Item::Enum(_) => todo!(),
        Item::Struct(_) => todo!(),
    }
});
crate::impl_display!(Statement, |s: &Statement| {
    match s {
        Statement::ParserError => "Error while parsing Statement".to_owned(),
        Statement::Break(val) => format!("Break {}", val.0),
        Statement::Loop(blocc) => format!("loop {}", blocc.0),
        Statement::Return(val) => format!("return {}", val.0),
        Statement::Continue => format!("continue"),
        Statement::WhileLoop(_, _) => todo!(),
        Statement::Expression(e) => format!("{e}"),
        // fuck => format!("Not yet implemented {fuck} :3\n"),
        _ => String::new(),
    }
});
impl Number {
    pub fn from_i32(num: i32) -> Self {
        Number::Int(num.into())
    }
    pub fn from_f32(num: f32) -> Self {
        Number::Float(num.into())
    }
}
impl Expression {
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
#[macro_export]
/// Takes a struct name as first argument and a closure of it's Struct
/// Synopsys: (`struct_name`, |s: &`struct_name`| match s{...})
macro_rules! impl_display {
    ($struct_name:ident, $write_calls:expr) => {
        impl std::fmt::Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", &$write_calls(&self))
            }
        }
    };
}
#[macro_export]
macro_rules! display_inner {
    ($struct_name:ident) => {
        impl std::fmt::Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{:#?}", stringify!($struct_name))
            }
        }
    };
}
#[macro_export]
macro_rules! impl_to_Instruction {
    ($name: ident, $closure :expr) => {
        impl<'Inp> ToInstruction<'Inp> for $name<'Inp> {
            fn into_instruction(self) -> Instruction<'Inp> {
                $closure(self.clone())
            }
        }
    };
}
#[macro_export]
macro_rules! impl_to_Instruction_nolife {
    ($name: ident, $closure :expr) => {
        impl ToInstruction for $name {
            fn into_instruction(self) -> Instruction {
                $closure(self.clone())
            }
        }
    };
}
/// Trait that turns something arbitrary into an Expression.
pub trait ToInstruction {
    fn into_instruction(self) -> Instruction;
}
pub fn i32_to_f32(num1: i32, num2: i32) -> f64 {
    let result = format!("{num1}.{num2}");
    result.parse::<f64>().unwrap()
}
