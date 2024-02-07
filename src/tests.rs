use ariadne::{Color, ColorGenerator, Label, Report, ReportKind, Source};
use colored::Colorize;
use parser::{self, parse_from_lex, range_into_span, OutputError};
#[test]
fn test_basic_lex() {
    let lex = r#"use io_print
    x = xöla
    y = 69 / (56 - 0.45)
    print(Works?)
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
    add (4,5).sqrt
    
    if x == 4  (
        print ("oooops!")
    ) else (
        print ("phew")
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
    let input = "(500.sqrt):3";
    test(input);
}
// #[test]
// fn test_loops() {
//     let input = "loop (
//         a = 5
//         return 3
//         continue a
//     \n)";

//     let (parse, errors, has_errors) = parser::lex_arrow_program(input.to_owned());
//     let lex = parse
//         .into_iter()
//         .map(|pair| (pair.0.clone(), SimpleSpan::new(pair.1.start, pair.1.end)))
//         .collect();
//     let parse = crate::parser::statement_parser::statement_parser()
//         .3
//         .parse(lex.as_slice().spanned((input.len()..input.len()).into()));
//     for error in parse.clone().into_errors() {
//         print_error(error, &input.to_string());
//     }
//     assert!(false)
// }
#[test]
fn test_return() {
    let input = "return 3";
    test(input);
}
#[test]
fn test_continue() {
    let input = "loop (
        continue 3
    )";
    test(input);
}
#[test]
fn test_function_definitions() {
    let input = "int draw: 
    state #SnekGame, 
    frame #Canvas, 
    window #Window( 
        a-3
    )"
    .to_string();
    let mut colors = ColorGenerator::new();
    let _a = colors.next();
    let _b = colors.next();
    let _out = Color::Fixed(81);
    let lex_result = parser::lex_arrow_program(&input);
    if !lex_result.is_ok() {
        assert!(false);
    }
    {
        let lex = range_into_span(lex_result.tokens());
        let parse = parse_from_lex(&lex);
        if let Some(_out) = parse.clone().into_output() {
        } else {
        };
        for error in parse.clone().into_errors() {
            print_error(error, &input);
        }
        assert!(!parse.has_errors());
    }
}

fn print_error(error: OutputError, input: &String) {
    let span = error.span();
    let expected = error
        .expected()
        .map(|a| format!("{:#?} ", a))
        .collect::<Vec<_>>()
        .concat();
    let found = error.found();
    let context = error.contexts().collect::<Vec<_>>();
    let _reason = error.reason();
    Report::build(ReportKind::Error, (), span.start)
        .with_message(format!("error while parsing: {context:?}"))
        .with_label(
            Label::new(span.start..span.end)
                .with_message(format!("but found {:#?}", found.unwrap()))
                .with_color(Color::Red),
        )
        .with_note(format!("Expected {expected:#?}"))
        .finish()
        .eprint(Source::from(input.clone()))
        .expect("Whooo unable to create error...");
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
        print(ksjdfo) "#;
    test(input);
}
#[test]
fn test_angery_case() {
    let input = r#"x = 50.sqrt
        y = ksjdfo
        print(Works?)"#;
    test(input);
}
#[test]
fn test_assign() {
    let input = "x = 5 + 5 * (69 +420)";
    test(input);
}
#[test]
fn test_bool_expr() {
    let input = r#"4 == 4 and 5 <= (5 + 1)"#;
    test(input);
}
#[test]
fn test_call() {
    let input = r#"foo.bar(test) "#;
    test(input);
}
#[test]
fn test_string() {
    let input = r#""Hi!":3"#;
    test(input);
}

#[test]
fn test_multiple_expressions() {
    let input = "(x = 4+5\n x= 32)";
    test(input);
}
#[test]
fn test_conditions() {
    let input = r#"if 4 == 4 (print(foo))"#;
    test(input);
}
#[test]
fn test_call_string() {
    let input = r#"print ("foo")"#;
    test(input);
}
#[test]
fn test_math_operation() {
    let input = r#"2+7/(3+4)"#;
    test(input);
}
fn test(input: &str) {
    let input = input.to_string();
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let lex_result = parser::lex_arrow_program(&input);
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
    if let Some(out) = parse.clone().into_output() {
        println!("\n{}: {:#?}", "PARSER OUTPUT".green(), out);
    } else {
        println!("\n{}", "No parser Output".yellow());
    };
    for error in parse.clone().into_errors() {
        let span = error.span();
        let expected = error
            .expected()
            .map(|a| format!("{}, ", a))
            .collect::<Vec<_>>()
            .concat();
        let found = error.found();
        let context = error.contexts().collect::<Vec<_>>();
        let _reason = error.reason();
        Report::build(ReportKind::Error, (), span.start)
            .with_message(format!("error while parsing: {context:?}"))
            .with_label(
                Label::new(span.start..span.end)
                    .with_message(format!("but found {:?}", found.unwrap()))
                    .with_color(Color::Red),
            )
            .with_note(format!("Expected {expected:?}"))
            .finish()
            .eprint(Source::from(input.clone()))
            .expect("Whooo unable to create error...");
    }
    assert!(!parse.has_errors());
}
