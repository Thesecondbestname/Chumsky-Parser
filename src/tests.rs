use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use parser::SketchyParser;
#[test]
fn test_basic_lex() -> anyhow::Result<()> {
    let lex = r#"use io/print
    x = xöla
    y = 69 / (56 - 0.45)
    _ = print(Works)
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
    _ = add (4,5). sqrt
    
    _ = if x == 4 then (
        _ = print ("oooops!")
    ) else (
        _ = print ("phew")
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
    let input = "x = 500.sqrt";
    test(input, "test_method_calls")
}
#[test]
fn test_paths() -> anyhow::Result<()> {
    let input = "x =std::core::rnd(crate::here::info)";
    test(input, "test_paths")
}
#[test]
fn test_struct_construction() -> anyhow::Result<()> {
    let input = r#"x = Dude { name= "Kevin", mood= Mood::Sadge}"#;
    test(input, "test_struct_construction")
}
#[test]
fn test_enum_construction() -> anyhow::Result<()> {
    let input = "x = Some(24)";
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
    let input = "x = 0..500\n";
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
        g = print(ksjdfo) ";
    test(input, "test_separator")
}
#[test]
fn test_angery_case() -> anyhow::Result<()> {
    let input = r"x = 50.sqrt
        y = ksjdfo
        _ = print(Works)";
    test(input, "test_angery_case")
}
#[test]
fn test_assign() -> anyhow::Result<()> {
    let input = "\nx = 5 + 5 * (69 +420)";
    test(input, "test_assign")
}
#[test]
fn test_else() -> anyhow::Result<()> {
    let input = "x = (24 + 4 else (x = 5))";
    test(input, "test_else")
}
#[test]
fn test_bool_expr() -> anyhow::Result<()> {
    let input = r"y = 4 == 4 and 5 <= (5 + 1)";
    test(input, "test_bool_expr")
}
#[test]
fn test_call() -> anyhow::Result<()> {
    let input = r"x = foo.bar(test)";
    test(input, "test_call")
}
#[test]
fn test_string() -> anyhow::Result<()> {
    let input = r#"g = "Hi!""#;
    test(input, "test_string")
}

#[test]
fn test_multiple_statements() -> anyhow::Result<()> {
    let input = "z = (
     x = 4+5
        x = 32
    )";
    test(input, "test_multiple_expressions")
}
#[test]
fn test_conditions() -> anyhow::Result<()> {
    let input = r"g = if (4 == 4) then (x = 3)";
    test(input, "test_conditions")
}
#[test]
fn test_conditions_inverted_parens() -> anyhow::Result<()> {
    let input = "l = if 4 == 4 then (n = 2.pass)";
    test(input, "test_conditions_inverted_parens")
}
#[test]
fn test_multiple_calls() -> anyhow::Result<()> {
    let input = r"m = lambda(3)(5).add(helo)";
    test(input, "test_multiple_calls")
}
#[test]
fn test_call_string() -> anyhow::Result<()> {
    let input = r#"m = print ("foo")"#;
    test(input, "test_call_string")
}
#[test]
fn test_math_operation() -> anyhow::Result<()> {
    let input = r"x = 2+7/(3+4)";
    test(input, "test_math_operation")
}
fn test(input: &str, name: &'static str) -> anyhow::Result<()> {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let parse = SketchyParser::builder()
        .input(input.trim(), name)
        .dbg_print_input()
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
    println!("\n\t{}", parse.ast());
    Ok(())
}
