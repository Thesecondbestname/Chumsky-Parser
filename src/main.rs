#![allow(non_snake_case)]
use ariadne::{Color, Label, Report, ReportKind, Source};
use colored::Colorize;
use parser::{OutputError, OutputType, SketchyParser};
mod compiler;
mod tests;

fn main() -> anyhow::Result<()> {
    let input_verified = r#"use io_print
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
    int add: int# x, int# y(
        x + y
    )
    // Some kinda idk  
    add (4,5).sqrt
    
    if x == 4  (
        print ("oooops!")
    ) else (
        print ("phew")
    )"#;

    print!("{esc}c", esc = 27 as char);
    let mut quit = false;

    let parse = SketchyParser::builder()
        .from_input(input_verified)
        .lex_sketchy_programm()
        .print_errors(|span, token, input| {
            println!("{} in span {span:?} with {token:?} at", "ERROR: ".red(),);
        })
        .into_result()?
        .parse_sketchy_programm()
        .print_errors(print_error)
        .into_result()?
        .build();

    while !quit {
        let mut usr_input = String::new();
        std::io::stdin()
            .read_line(&mut usr_input)
            .expect("Failed to read line");

        if usr_input == "q" {
            quit = true;
        }
        // parse_from_string(usr_input);
    }
    Ok(())
}
fn print_error(error: &OutputError, ast: &OutputType, input: &str) {
    let span = error.span();
    let expected = error
        .expected()
        .map(|a| format!("{:#?} ", a))
        .collect::<Vec<_>>()
        .concat();
    let found = error.found().unwrap_or(&parser::Token::Nothing);
    let empty_span = parser::span_from(span.start);
    let context = error
        .contexts()
        .last()
        .unwrap_or_else(|| (&"No context", &empty_span));
    Report::build(ReportKind::Error, (), span.start)
        .with_message(format!("error while parsing {:?}", context))
        .with_label(
            Label::new(span.start..span.end)
                .with_message(format!("found {found:?}",))
                .with_color(Color::Red),
        )
        .with_note(format!("Expected one of {expected} but found {found}"))
        .finish()
        .eprint(Source::from(input))
        .expect("Whooo unable to create error...");
}
// fn parse_from_string(inp: String) {
//     let lex_result = lex_sketchy_program(&inp);
//     if !lex_result.is_ok() {}
//     let parser_input = lex_result
//         .tokens()
//         .iter()
//         .map(|(tok, range)| (tok, range_to_span(range)))
//         .collect();
//     let parse_result = parse_from_lex(&parser_input);
//     parse_result.clone().into_output().map_or_else(
//         || {
//             println!("\n{}", "No parser Output".yellow());
//         },
//         |out| {
//             println!("\n{}: {:#?}", "PARSER OUTPUT".green(), out);
//         },
//     );
//     for error in parse_result.clone().into_errors() {
//         crate::print_error(error, &inp)
//     }
// }
