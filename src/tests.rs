use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
use colored::Colorize;
use parser::{parse_from_lex, range_into_span, OutputError};
#[test]
fn test_basic_lex() {
    let lex = r#"use io_print
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
    int add: x# int, y# int (
        x + y
    // Some kinda idk  
    add (4,5).sqrt:3
    
    if x == 4  (
        print ("oooops!"):3
    ) else (
        print ("phew"):3
    )"#;
    test(lex);
}
#[test]
fn test_structs() {
    let input = r#"struct baz:
        lmao# int,
        lmao2# int
    ;"#;
    test(input);
}
#[test]
fn test_method_calls() {
    let input = "500.sqrt:3";
    test(input);
}
#[test]
fn test_loops() {
    let input = "loop (
        a = 5
        return 3
        continue
    \n)";
    test(input);
}
#[test]
fn test_return() {
    let input = "return 3";
    test(input);
}
#[test]
fn test_continue() {
    let input = "loop (continue)";
    test(input);
}
#[test]
fn test_function_definitions() {
    let input = "int draw: 
    state #SnekGame, 
    frame #Canvas, 
    window #Window( 
        a-3
    )";
    test(input);
}

#[test]
fn test_span() {
    let input = "0..500:3";
    test(input);
}
#[test]
fn test_use() {
    let input = r#"use foo_bar_baz"#;
    test(input);
}
#[test]
fn test_seperator() {
    let input = r#"x= 50
        print(ksjdfo):3 "#;
    test(input);
}
#[test]
fn test_angery_case() {
    let input = r#"x = 50.sqrt
        y = ksjdfo
        print(Works):3"#;
    test(input);
}
#[test]
fn test_assign() {
    let input = "x = 5 + 5 * (69 +420)";
    test(input);
}
#[test]
fn test_bool_expr() {
    let input = r#"4 == 4 and 5 <= (5 + 1):3"#;
    test(input);
}
#[test]
fn test_call() {
    let input = r#"foo.bar(test) :3"#;
    test(input);
}
#[test]
fn test_string() {
    let input = r#""Hi!":3"#;
    test(input);
}

#[test]
fn test_multiple_expressions() {
    let input = "(
    x = 4+5
        x = 32
    ):3";
    test(input);
}
#[test]
fn test_conditions() {
    let input = r#"if (4 == 4) (3)"#;
    test(input);
}
#[test]
fn test_call_string() {
    let input = r#"print ("foo"):3"#;
    test(input);
}
#[test]
fn test_math_operation() {
    let input = r#"2+7/(3+4):3"#;
    test(input);
}
fn test(input: &str) {
    let input = input.to_string();
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let lex_result = parser::lex_sketchy_program(&input);
    if !lex_result.is_ok() {
        for x in lex_result.errors() {
            // println!("{} at {:#?} with {}", "LEX ERROR".red(), &x.1, &x.2);
            Report::build(ReportKind::Error, "test", 12)
                .with_message(format!("{x:?}"))
                .with_label(
                    Label::new(("test", x.clone().1))
                        .with_message(x.2.to_string())
                        .with_color(a),
                )
                .finish()
                .print(("test", Source::from(input.clone())))
                .expect("Falied to build report!");
        }
        assert!(false);
    }

    let lex = range_into_span(lex_result.tokens());
    let parse = parse_from_lex(&lex);
    parse.clone().into_output().map_or_else(
        || {
            println!("\n{}", "No parser Output".yellow());
        },
        |out| {
            println!("\n{}: {:#?}", "PARSER OUTPUT".green(), out);
        },
    );
    for error in parse.clone().into_errors() {
        crate::print_error(error, &input)
    }
    assert!(!parse.has_errors());
}
