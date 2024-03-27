use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use parser::SketchyParser;
#[test]
fn test_basic_lex() -> anyhow::Result<()> {
    let lex = r#"use io/print
    x = xöla
    y = 69 / (56 - 0.45)
    print(Works):3
    enum Foo:
        baz
    ;
    struct baz:
        lmao# int,
        lmao2# int
    ;
    add: x#int, y#int; int (
        x + y
    )
    // Some kinda idk  
    add (4,5). sqrt:3
    
    if x == 4  (
        print ("oooops!"):3
    ) else (
        print ("phew"):3
    )"#;
    test(lex, "test_basic_lex")
}
#[test]
fn test_structs() -> anyhow::Result<()> {
    let input = r"struct baz:
        lmao# int,
        lmao2# int
    ;";
    test(input, "test_structs")
}
#[test]
fn test_method_calls() -> anyhow::Result<()> {
    let input = "500.sqrt:3";
    test(input, "test_method_calls")
}
#[test]
fn test_paths() -> anyhow::Result<()> {
    let input = "std::core::rnd(crate::here::info)";
    test(input, "test_paths")
}
#[test]
fn test_struct_construction() -> anyhow::Result<()> {
    let input = r#"Dude { name= "Kevin", mood= Mood::Sadge}"#;
    test(input, "test_struct_construction")
}
#[test]
fn test_enum_construction() -> anyhow::Result<()> {
    let input = "Some(24)";
    test(input, "test_enum_construction")
}
#[test]
fn test_loops() -> anyhow::Result<()> {
    let input = "loop (
        a = 5
        return 3
        continue
    \n)";
    test(input, "test_loops")
}
#[test]
fn test_return() -> anyhow::Result<()> {
    let input = "return 3";
    test(input, "test_return")
}
#[test]
fn test_continue() -> anyhow::Result<()> {
    let input = "loop (continue)";
    test(input, "test_continue")
}
#[test]
fn test_function_definitions() -> anyhow::Result<()> {
    let input = "draw: 
    state #SnekGame, 
    frame #Canvas, 
    window #Window; int( 
         a-4 *3
    )";
    test(input, "test_function_definitions")
}

#[test]
fn test_span() -> anyhow::Result<()> {
    let input = "0..500:3";
    test(input, "test_span")
}
#[test]
fn test_use() -> anyhow::Result<()> {
    let input = r"use foo/bar/baz";
    test(input, "test_use")
}
#[test]
fn test_separator() -> anyhow::Result<()> {
    let input = r"x = 50
        print(ksjdfo):3 ";
    test(input, "test_separator")
}
#[test]
fn test_angery_case() -> anyhow::Result<()> {
    let input = r"x = 50.sqrt
        y = ksjdfo
        print(Works):3";
    test(input, "test_angery_case")
}
#[test]
fn test_assign() -> anyhow::Result<()> {
    let input = "\nx = 5 + 5 * (69 +420)";
    test(input, "test_assign")
}
#[test]
fn test_else() -> anyhow::Result<()> {
    let input = "24 + 4 else (b)";
    test(input, "test_else")
}
#[test]
fn test_bool_expr() -> anyhow::Result<()> {
    let input = r"4 == 4 and 5 <= (5 + 1):3";
    test(input, "test_bool_expr")
}
#[test]
fn test_call() -> anyhow::Result<()> {
    let input = r"foo.bar(test) :3";
    test(input, "test_call")
}
#[test]
fn test_string() -> anyhow::Result<()> {
    let input = r#""Hi!":3"#;
    test(input, "test_string")
}

#[test]
fn test_multiple_statements() -> anyhow::Result<()> {
    let input = "(
     x = 4+5
        x = 32
    ):3";
    test(input, "test_multiple_expressions")
}
#[test]
fn test_conditions() -> anyhow::Result<()> {
    let input = r"if (4 == 4) (3)";
    test(input, "test_conditions")
}
#[test]
fn test_conditions_inverted_parens() -> anyhow::Result<()> {
    let input = "if 4 == 4 (x = 5)";
    test(input, "test_conditions_inverted_parens")
}
#[test]
fn test_multiple_calls() -> anyhow::Result<()> {
    let input = r"lambda(3)(5).add(helo):3";
    test(input, "test_multiple_calls")
}
#[test]
fn test_call_string() -> anyhow::Result<()> {
    let input = r#"print ("foo"):3"#;
    test(input, "test_call_string")
}
#[test]
fn test_math_operation() -> anyhow::Result<()> {
    let input = r"2+7/(3+4):3";
    test(input, "test_math_operation")
}
fn test(input: &str, name: &'static str) -> anyhow::Result<()> {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let parse = SketchyParser::builder()
        .input(input.trim(), name)
        .lex_sketchy_programm()
        .print_errors(|span, token, input, name| {
            Report::build(ReportKind::Error, name, 12)
                .with_message(format!("Error while lexing test {input}"))
                .with_label(
                    Label::new((name, span.start - 1..span.end - 1.clone()))
                        .with_message(format!("Found unexpected Token {token} at {span:?}"))
                        .with_color(a),
                )
                .finish()
                .eprint((name, Source::from(input)))
                .expect("Falied to build report!");
        })
        .into_result()?
        .remove_duplicate_newline()
        .parse_sketchy_programm()
        .print_errors(crate::print_error)
        .into_result()?
        .finish();
    println!("{}", parse.ast());
    Ok(())
}
