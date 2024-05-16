use crate::convenience_types::Spanned;
use displaydoc::Display;

#[derive(Debug, Clone)]
pub enum Instruction {
    Statement(Box<Statement>),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct Block(pub Vec<Spanned<Item>>);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub Vec<Spanned<String>>);

#[derive(Debug, Clone)]
pub enum Pattern {
    Name(Name),
    Value(Spanned<Expression>),
    Enum(Ident, Vec<Spanned<Pattern>>),
    Struct(Ident, Vec<(Spanned<Name>, Spanned<Pattern>)>),
    Tuple(Vec<Spanned<Pattern>>),
    Array(Vec<Spanned<Pattern>>, Name),
    PatternError,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Name {
    Name(Vec<Spanned<String>>),
    Underscore,
}

#[derive(Debug, Clone)]
pub enum Item {
    //TODO: Read span here
    Function(FunctionDeclaration),
    Import(Spanned<Import>),
    Enum(Spanned<EnumDeclaration>),
    Struct(Spanned<StructDeclaration>),
    Assingment(Spanned<VariableDeclaration>),
    Trait(Spanned<Trait>),
    TopLevelExprError,
}

#[derive(Debug, Clone)]
pub struct Trait(pub String, pub Vec<Spanned<TraitFns>>);
#[derive(Debug, Clone)]
pub struct TraitFns(pub String, pub Vec<Spanned<Type>>, pub Spanned<Type>);
#[derive(Debug, Clone)]
pub struct VariableDeclaration(pub Spanned<Pattern>, pub Spanned<Expression>);
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
pub struct Import(pub Vec<Spanned<String>>);

#[derive(Debug, Clone)]
/// Where the enum is declared and not where it is constructed
pub struct EnumDeclaration {
    pub name: String,
    pub variants: Vec<Spanned<EnumVariantDeclaration>>,
    pub impl_blocks: Vec<(Option<String>, Spanned<FunctionDeclaration>)>,
}
/// A variant of an enum that is currently declared. I.e. an arm.
#[derive(Debug, Clone)]
pub struct EnumVariantDeclaration {
    pub name: String,
    pub fields: Vec<Spanned<Type>>,
}
#[derive(Debug, Clone)]
/// The one where the struct is firstly defined
pub struct StructDeclaration {
    pub name: String,
    pub fields: Vec<Spanned<StructField>>,
    pub impl_blocks: Vec<(Option<String>, Spanned<FunctionDeclaration>)>,
}
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Spanned<String>,
    pub r#type: Spanned<Type>,
}
#[derive(Debug, Clone)]
pub enum Statement {
    ParserError,
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
}
#[derive(Clone, Debug)]
pub enum Expression {
    ParserError,
    TopLvlExpr(Box<Spanned<Expression>>),
    If(Box<If>),
    Match {
        condition: Box<Spanned<Expression>>,
        arms: Vec<(Spanned<Pattern>, Spanned<Expression>)>,
    },
    Ident(Ident),
    List(Vec<Self>),
    FunctionCall(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    MethodCall(Box<Spanned<Self>>, Ident, Vec<Spanned<Self>>),
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

#[derive(Debug, Clone)]
pub struct If {
    pub(crate) condition: Spanned<Expression>,
    pub(crate) code_block: Spanned<Expression>,
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
    Object {
        name: Spanned<Ident>,
        fields: Spanned<Vec<(Spanned<String>, Spanned<Expression>)>>,
    },
}
#[derive(Clone, Debug)]
pub enum MathOp {
    Add,
    Sub,
    Div,
    Mul,
}
#[derive(Clone, Debug, Display)]
pub enum ComparisonOp {
    /// >
    Lt,
    /// <
    Gt,
    /// >=
    Lte,
    /// <=
    Gte,
    /// !=
    Neq,
    /// ==
    Eq,
}
#[derive(Clone, Debug, Display)]
pub enum BinaryOp {
    /// &&
    And,
    /// ||
    Or,
    /// !|
    Xor,
}
/// All the type primitives. These do not contain values, rather they denote that a given type
/// belongs here.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Self_,
    Int,
    Bool,
    Float,
    String,
    Array(Box<Type>, i64),
    Tuple(Vec<Type>),
    Char,
    Span,
    Path(Spanned<Ident>),
}
#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}
crate::impl_display!(Value, |s: &Value| match s {
    Value::String(string) => string.to_string(),
    Value::Number(Number::Int(int)) => format!("{int}"),
    Value::Number(Number::Float(float)) => format!("{float}"),
    Value::Array(len, vals) => format!("{{len:{} [{:?}]}}", len, vals),
    Value::Tuple(vals) => format!("({vals:?})"),
    Value::Char(char) => format!("'{char}'"),
    Value::Bool(bool) => format!("{bool}"),
    Value::Span(start, end) => format!("{}..{}", start, end),
    Value::Option(val) => format!("{val}?"),
    Value::Object { name, fields } => format!(
        "{} {{{}}}",
        name.0,
        fields
            .0
            .iter()
            .map(|(name, field)| format!("{}: {}", name.0, field.0))
            .collect::<Vec<_>>()
            .join(",")
    ),
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
            format!("({} {} {})", a.0, b, c.0)
        }
        Expression::Value(a) => format!("{a}"),
        Expression::Ident(a) => format!("{a}"),
        Expression::ParserError => "Error".to_string(),
        Expression::FunctionCall(called, args) => format!(
            "{{{}({})}}",
            called.0,
            args.into_iter()
                .map(|a| a.0.to_string())
                .reduce(|acc, a| format!("{acc}, {a}"))
                .unwrap_or("".to_string())
        ),
        Expression::MethodCall(on, name, args) => {
            format!(
                "{{{}.{}({})}}",
                on.0,
                name,
                args.iter().fold(String::new(), |acc, a| format!(
                    "{acc} {}",
                    format!("{},", a.0)
                ))
            )
        }
        Expression::Block(block) => format!("{block}"),
        Expression::If(if_) => format!("{if_}"),
        Expression::Comparison(lhs, op, rhs) => format!("({} {op} {})", lhs.0, rhs.0),
        Expression::Binary(a, op, b) => format!("({} {op} {})", a.0, b.0),
        Expression::Else(c, e) => format!("{} else ({})", c.0, e.0),
        Expression::UnaryBool(e) => format!("!{}", e.0),
        Expression::UnaryMath(e) => format!("-{}", e.0),
        Expression::Unit => "Dis weird aah heal".to_owned(),
        Expression::Match { condition, arms } => format!(
            "match {} {{{}}}",
            condition.0,
            arms.iter().fold(String::new(), |acc, it| acc
                + &format!("{} => {},", it.0 .0, it.1 .0))
        ),
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
crate::impl_display!(Ident, |s: &Ident| {
    s.0.iter()
        .map(|x| x.0.clone())
        .collect::<Vec<_>>()
        .join("::")
});
crate::impl_display!(Type, |s: &Type| {
    match s {
        Type::Self_ => "Self".to_owned(),
        Type::Int => "integer".to_owned(),
        Type::Bool => "bool".to_owned(),
        Type::Float => "float".to_owned(),
        Type::String => "String".to_owned(),
        Type::Array(ty, size) => format!("[{ty};{size}]"),
        Type::Tuple(t) => format!("({:?})", t),
        Type::Char => "char".to_owned(),
        Type::Span => "span".to_owned(),
        Type::Path(p) => format!("{}", p.0),
    }
});
crate::impl_display!(StructDeclaration, |s: &StructDeclaration| {
    let fields = s
        .fields
        .iter()
        .map(|x| format!("{}: {}", x.0.name.0.clone(), x.0.r#type.0.clone()))
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "struct {0} {{ {fields}}}{1}",
        s.name,
        s.impl_blocks
            .iter()
            .map(|a| format!(
                "impl {}{}{{{}}}",
                a.0.clone()
                    .map(|a| a.to_owned() + " for ")
                    .unwrap_or("".to_string()),
                s.name,
                a.1 .0
            ))
            .collect::<String>()
    )
});
crate::impl_display!(FunctionDeclaration, |s: &FunctionDeclaration| {
    format!(
        "fn {}({}) -> {} ({})",
        s.name.0,
        s.arguments.iter().fold(String::new(), |acc, a| format!(
            "{acc} {}: {},",
            a.0 .1 .0.clone(),
            a.0 .0 .0.clone()
        )),
        s.return_type.0,
        s.body.0
    )
});
crate::impl_display!(Item, |s: &Item| {
    match s {
        Item::Function(r#fn) => r#fn.to_string(),
        Item::Import((imp, _)) => format!(
            "use {};",
            imp.0
                .iter()
                .fold(String::new(), |acc, (a, _)| format!("{acc}::{a}"))
        ),
        Item::Enum((
            EnumDeclaration {
                name,
                variants,
                impl_blocks,
            },
            _,
        )) => {
            format!(
                "enum {} {{{} {:?}}}",
                name,
                variants.iter().fold(String::new(), |acc, (v, _)| acc
                    + &format!(
                        "{}({}{}),",
                        v.name,
                        v.fields
                            .iter()
                            .next()
                            .map(|x| x.0.to_string())
                            .unwrap_or("".to_string()),
                        v.fields
                            .iter()
                            .skip(1)
                            .fold(String::new(), |acc, b| format!("{acc}, {}", b.0))
                    )),
                impl_blocks
            )
        }
        Item::Struct((struct_, _)) => format!("{struct_}"),
        Item::Assingment((decl, _)) => format!("let {} = {};", decl.0 .0, decl.1 .0),
        Item::Trait((Trait(a, b), _)) => format!(
            "trait {a} {{{}}}",
            b.iter()
                .map(|a| format!(
                    "fn {}({}) -> {}",
                    a.0 .0,
                    a.0 .1
                        .iter()
                        .fold(String::new(), |acc, b| format!("{},{acc}", b.0)),
                    a.0 .2 .0
                ))
                .fold(String::new(), |acc, b| format!("{b},{acc}"))
        ),
        Item::TopLevelExprError => todo!(),
    }
});
crate::impl_display!(Name, |s: &Name| {
    match s {
        Name::Name(name) => format!(
            "{}{}",
            name.iter().next().unwrap().0,
            name.iter()
                .skip(1)
                .fold(String::new(), |acc, a| format!("{acc}::{}", a.0))
        ),
        Name::Underscore => "_".to_string(),
    }
});
crate::impl_display!(Pattern, |s: &Pattern| {
    match s {
        Pattern::Enum(name, pattern) => {
            format!(
                "{} ({})",
                name,
                pattern
                    .iter()
                    .fold(String::new(), |acc, a| acc + &a.0.to_string())
            )
        }
        Pattern::Struct(name, pat) => format!(
            "{name}{{{}{}}}",
            format!(
                "{}:{}",
                pat.iter().next().unwrap().0 .0,
                pat.iter().next().unwrap().1 .0
            ),
            pat.iter()
                .skip(1)
                .fold(String::new(), |acc, (name, pat)| acc
                    + &format!(", {}: {}", name.0, pat.0))
        ),
        Pattern::Tuple(tuple) => format!(
            "({}{})",
            tuple.iter().next().unwrap().0,
            tuple
                .iter()
                .skip(1)
                .fold(String::new(), |acc, a| acc + ", " + &a.0.to_string())
        ),
        Pattern::Array(pats, end) => format!(
            "[{}{}..{end}]",
            pats.iter().next().unwrap().0,
            pats.iter()
                .skip(1)
                .fold(String::new(), |acc, b| format!("{}, {}", acc, b.0))
        ),
        Pattern::Name(name) => name.to_string(),
        Pattern::Value(e) => e.0.to_string(),
        Pattern::PatternError => "Bad pattern(probably expression)".to_owned(),
    }
});
crate::impl_display!(Statement, |s: &Statement| {
    match s {
        Statement::ParserError => "Error while parsing Statement".to_owned(),
        Statement::Break(val) => format!("Break {};", val.0),
        Statement::Loop(blocc) => format!("loop ({});", blocc.0),
        Statement::Return(val) => format!("return {};", val.0),
        Statement::Continue => "continue;".to_string(),
        Statement::WhileLoop(_, _) => todo!(),
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
