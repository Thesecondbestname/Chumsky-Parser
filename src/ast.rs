use core::fmt;

#[derive(Debug)]
pub enum Instruction {
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}
#[derive(Debug)]
pub struct Block {
    instructions: Vec<Instruction>,
    return_type: Type,
}

#[derive(Debug)]
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
#[derive(Debug)]
pub struct EnumVariantDeclaration {
    name: String,
    value: Option<Type>,
}
#[derive(Debug)]
pub struct StructFieldDeclaration {
    name: String,
    r#type: Type,
}
#[derive(Debug)]
pub struct Variable {
    name: String,
    r#type: Type,
    value: Box<Expression>,
}
pub struct Expression {
    type_of_expression: ExpressionType,
    return_type: Type,
}
#[derive(Debug)]
pub enum ExpressionType {
    Variable(Variable),
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
#[derive(Debug)]
pub struct FunctionCall {
    name: String,
    arguments: Vec<Expression>,
    return_type: Type,
}
#[derive(Debug)]
pub enum Type {
    Int,
    Bool,
    Float,
    String,
    Array,
    Tuple,
    Char,
    Span,
}
#[derive(Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}
#[derive(Debug)]
pub struct Span {
    begin: Number,
    end: Number,
}
/// Enum used to hold mathematical operations.
#[derive(Debug)]
pub enum MathExpression {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Pow(Box<Expression>, Box<Expression>),
    DevEq(Box<Expression>, Box<Expression>),
}
/// Operators taking bools and returning bools. Elsewhere called booleand extenders
enum BinaryBinaryOperation {
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Nand(Box<Expression>, Box<Expression>),
    Xor(Box<Expression>, Box<Expression>),
}
/// Operator that returns a boolean but takes anything
enum MathtoBinaryOperation {
    Lt(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Eq(Box<Expression>, Box<Expression>),
    Neq(Box<Expression>, Box<Expression>),
    LtE(Box<Expression>, Box<Expression>),
    GtE(Box<Expression>, Box<Expression>),
}

// impl core::fmt::Debug for ExpressionType {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         match self {
//             ExpressionType::Add(a, b) => write!(f, "{:?} + {:?}", a, b),
//             ExpressionType::Sub(a, b) => write!(f, "{:?} - {:?}", a, b),
//             ExpressionType::Mul(a, b) => write!(f, "{:?} * {:?}", a, b),
//             ExpressionType::Div(a, b) => write!(f, "{:?} / {:?}", a, b),
//             ExpressionType::Pow(a, b) => write!(f, "{:?} ^ {:?}", a, b),
//             ExpressionType::DevEq(a, b) => write!(f, "{:?} % {:?}", a, b),
//             ExpressionType::Value(a) => write!(f, "{:?}",a),
//             _ => write!(f, "Not implemented...")
//         }
//     }
// }
impl core::fmt::Debug for MathtoBinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MathtoBinaryOperation::Lt(lhs, rhs) => write!(f, "<"),
            MathtoBinaryOperation::Gt(lhs, rhs) => write!(f, ">"),
            MathtoBinaryOperation::Eq(lhs, rhs) => write!(f, "="),
            MathtoBinaryOperation::Neq(lhs, rhs) => write!(f, "!="),
            MathtoBinaryOperation::LtE(lhs, rhs) => write!(f, "<="),
            MathtoBinaryOperation::GtE(lhs, rhs) => write!(f, ">="),
        }
    }
}
// impl fmt::Debug for BinaryBinaryOperation {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(
//             f,
//             r#"Boolean operator that takes in a boolean, like && or "and" "#
//         )
//     }
// }
crate::ImplDebug!(
    BinaryBinaryOperation,
    "Boolean operator that takes in a boolean, like && or and"
);
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(arg0) => write!(f, r#""{}""#, arg0),
            Self::Number(arg0) => write!(f, r#"{:?}"#, arg0),
            Self::Array(arg0, arg1) => write!(f, r#""{:?}", length {:?}"#, arg1, arg0),
            Self::Tuple(arg0) => write!(f, r#"({:?})"#, arg0),
            Self::Char(arg0) => write!(f, r#"'{}'"#, arg0),
            Self::Bool(arg0) => write!(f, r#"{}"#, arg0),
            Self::Span(span) => write!(f, r#"from {:?} to {:?}"#, span.begin, span.end),
        }
    }
}
impl Instruction {
    /// Generates a new boxed expression over the inputs.
    pub fn from_expr(expr: Expression) -> Instruction {
        Instruction::Expression(Box::new(expr))
    }
}
impl Expression {
    pub fn from_ExpressionType(expr: ExpressionType, ret_type: Type) -> Expression {
        Expression {
            type_of_expression: expr,
            return_type: ret_type,
        }
    }
    pub fn to_UnaryMathExpression(self) -> Expression {
        Self::from_ExpressionType(ExpressionType::UnaryMath(Box::new(self)), Type::Int)
    }
    
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            r#" "{:?}" ({:?})"#,
            self.type_of_expression, self.return_type
        )
    }
}
// crate::ImplDebug!(
//     Expression,
//     (
//         r#" "{:?}"  returns a {:?}"#,
//         self.type_of_expression,
//         self.return_type
//     )
// );
impl Value {
    pub fn to_ExpressionType(self) -> ExpressionType {
        ExpressionType::Value(self)
    }
    /// Converts an ExpressionType to an Expression using an input as return type
    pub fn to_Expression(self, ret_type: Type) -> Expression {
        self.to_ExpressionType().to_Expression(ret_type)
    }
}
impl ExpressionType {
    pub fn to_Expression(self, ret: Type) -> Expression {
        Expression {
            type_of_expression: self,
            return_type: ret,
        }
    }
}
#[macro_export]
macro_rules! ImplDebug {
    ( $struct:ident, $write_macro:expr ) => {
        impl fmt::Debug for $struct {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", $write_macro)
            }
        }
    };
}
