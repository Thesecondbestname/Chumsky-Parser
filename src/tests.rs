use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use parser::SketchyParser;
#[test]
fn basic_lex() -> anyhow::Result<()> {
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
        impl Add:
            add: Self; int (
                self.lmao + self.lmao2
            )
        ;
        impl:
            new: 
                window #Window; int ( 
                 a-4 *3
            )   
            draw: 
                state #SnekGame, 
                frame #Canvas, 
                window #Window; int ( 
                 a-4 *3
            )   
        ;
    ;
    add: x#int, y#int; int (
        match x if 
            4 then "four",
            _ then x + y
        ;
    )
    // Some kinda idk  
    _ = add (4,5). sqrt
    
    _ = if x == 4 then (
        _ = print ("oooops!")
    ) else (
        _ = print ("phew")
    )"#;
    test(lex, "basic_lex")
}
#[test]
fn structs() -> anyhow::Result<()> {
    let input = r"struct baz:
        lmao# int,
        lmao2# int
    ;";
    test(input, "structs")
}
#[test]
fn enums() -> anyhow::Result<()> {
    let input = r"enum Baz:
        lmao,
        lmao2(int, bool)
    ;";
    test(input, "enums")
}
#[test]
fn struct_functions() -> anyhow::Result<()> {
    let input = r"struct baz:
        lmao# int,
        lmao2# int,
        impl Add:
            draw: 
                state #SnekGame, 
                frame #Canvas, 
                window #Window; int ( 
                 a-4 *3
            )   
        ;
        impl:
            new: 
                window #Window; int ( 
                 a-4 *3
            )   
        ;

    ;";
    test(input, "structs_with_impl")
}
#[test]
fn method_calls() -> anyhow::Result<()> {
    let input = "x = 500.sqrt";
    test(input, "method_calls")
}
#[test]
fn traits() -> anyhow::Result<()> {
    let input = "trait Add: add:int, int; int;";
    test(input, "traits")
}
#[test]
fn enum_destructuring() -> anyhow::Result<()> {
    let input = "Some((name, _)) = Y";
    test(input, "enum_destructuring")
}
#[test]
fn struct_destructuring() -> anyhow::Result<()> {
    let input = "Person(name# (name, _), pattern# _) = Y";
    test(input, "struct_destructuring")
}
#[test]
fn array_destructuring() -> anyhow::Result<()> {
    let input = "[a,b,c..d] = Y";
    test(input, "array_destructuring")
}
#[test]
fn paths() -> anyhow::Result<()> {
    let input = "x =std::core::rnd(crate::here::info)";
    test(input, "paths")
}
#[test]
fn struct_construction() -> anyhow::Result<()> {
    let input = r#"x = Dude { name= "Kevin", mood= Mood::Sadge}"#;
    test(input, "struct_construction")
}
#[test]
fn enum_construction() -> anyhow::Result<()> {
    let input = "x = Some(24)";
    test(input, "enum_construction")
}
#[test]
fn loops() -> anyhow::Result<()> {
    let input = "loop (
        a = 5
        return 3
        continue
    \n)";
    test(input, "loops")
}
#[test]
fn r#return() -> anyhow::Result<()> {
    let input = "return 3";
    test(input, "return")
}
#[test]
fn r#continue() -> anyhow::Result<()> {
    let input = "loop (continue)";
    test(input, "continue")
}
#[test]
fn function_definitions() -> anyhow::Result<()> {
    let input = "draw: 
    state #SnekGame, 
    frame #Canvas, 
    window #Window; int ( 
         a-4 *3
    )";
    test(input, "function_definitions")
}

#[test]
fn span() -> anyhow::Result<()> {
    let input = "x = 0..500\n";
    test(input, "span")
}
#[test]
fn import() -> anyhow::Result<()> {
    let input = r"use foo/bar/baz";
    test(input, "use")
}
#[test]
fn separator() -> anyhow::Result<()> {
    let input = r"x = 50
        g = print(ksjdfo) ";
    test(input, "separator")
}
#[test]
fn angery_case() -> anyhow::Result<()> {
    let input = r"x = 50.sqrt
        y = ksjdfo
        _ = print(Works)";
    test(input, "angery_case")
}
#[test]
fn assign() -> anyhow::Result<()> {
    let input = "\nx = 5 + 5 * (69 +420)";
    test(input, "assign")
}
#[test]
fn r#else() -> anyhow::Result<()> {
    let input = "x = (24 + 4 else (x = 5))";
    test(input, "else")
}
#[test]
fn bool_expr() -> anyhow::Result<()> {
    let input = r"y = 4 == 4 and 5 <= (5 + 1)";
    test(input, "bool_expr")
}
#[test]
fn call() -> anyhow::Result<()> {
    let input = r"x = foo.bar(test)";
    test(input, "call")
}
#[test]
fn string() -> anyhow::Result<()> {
    let input = r#"g = "Hi!""#;
    test(input, "string")
}
#[test]
fn r#match() -> anyhow::Result<()> {
    let input = "x = match Some(x) if Some(e) then e, None then panic();";
    test(input, "match")
}
#[test]
fn multiple_statements() -> anyhow::Result<()> {
    let input = "z = (
     x = 4+5
        x = 32
    )";
    test(input, "multiple_expressions")
}
#[test]
fn conditions() -> anyhow::Result<()> {
    let input = r"g = if (4 == 4) then (x = 3)";
    test(input, "conditions")
}
#[test]
fn conditions_inverted_parens() -> anyhow::Result<()> {
    let input = "l = if 4 == 4 then (n = 2.pass)";
    test(input, "conditions_inverted_parens")
}
#[test]
fn multiple_calls() -> anyhow::Result<()> {
    let input = r"m = lambda(3)(5).add(helo)";
    test(input, "multiple_calls")
}
#[test]
fn call_multiple_args() -> anyhow::Result<()> {
    let input = r#"m = print ("foo", 5, false)"#;
    test(input, "call_string")
}
#[test]
fn math_operation() -> anyhow::Result<()> {
    let input = r"x = 2+7/(3+4)";
    test(input, "math_operation")
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
        .dbg_print_ast()
        .into_result()?
        .finish();
    println!("\n\t{}", parse.ast());
    Ok(())
}
