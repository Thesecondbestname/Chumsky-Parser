#![allow(non_snake_case)]
use ariadne::{Color, Label, Report, ReportKind, Source};
use colored::Colorize;
use parser::{lex_arrow_program, parse_from_lex, range_into_span};
mod compiler;

fn main() {
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
    parse_from_string(input_verified.to_string());
    while !quit {
        let mut usr_input = String::new();
        std::io::stdin()
            .read_line(&mut usr_input)
            .expect("Failed to read line");

        if usr_input == "q" {
            quit = true;
        }
        parse_from_string(usr_input);
    }
}
fn print_error(error: parser::OutputError, input: &str) {
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
                .with_message(format!(
                    "but found {:#?}",
                    found.unwrap_or(&parser::Token::Add)
                ))
                .with_color(Color::Red),
        )
        .with_note(format!("Expected {expected:#?}"))
        .finish()
        .eprint(Source::from(input.clone()))
        .expect("Whooo unable to create error...");
}
fn parse_from_string(inp: String) {
    let lex_result = lex_arrow_program(&inp);
    println!("{}", "finished Lexing!\n".yellow());
    if !lex_result.is_ok() {
        for error in lex_result.errors() {
            println!(
                "{} in span {:?} with {:?}",
                "ERROR: ".red(),
                error.1,
                error.2
            );
        }
    }
    let parser_input = &range_into_span(lex_result.tokens());
    let parse_result = parse_from_lex(parser_input);
    parse_result.clone().into_output().map_or_else(
        || {
            println!("\n{}", "No parser Output".yellow());
        },
        |out| {
            println!("\n{}: {:#?}", "PARSER OUTPUT".green(), out);
        },
    );
    for error in parse_result.clone().into_errors() {
        crate::print_error(error, &inp)
    }
}
#[cfg(test)]
mod tests;
