#![allow(non_snake_case)]
use ariadne::{Color, Label, Report, ReportKind, Source};
use colored::Colorize;
use parser::{OutputError, OutputType, SketchyParser};
mod compiler;
mod tests;

fn main() -> anyhow::Result<()> {
    let input_verified = r#"
    use io_print
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

    // print!("{esc}c", esc = 27 as char);
    let mut quit = false;

    let parse = SketchyParser::builder()
        .input(input_verified, "main")
        .lex_sketchy_programm()
        .print_errors(|span, token, input, name| {
            println!("{} in span {span:?} with {token:?} at", "ERROR: ".red(),);
        })
        .into_result()?
        .parse_sketchy_programm()
        .print_errors(print_error)
        .into_result()?
        .finish();

    while !quit {
        let mut usr_input = String::new();
        std::io::stdin()
            .read_line(&mut usr_input)
            .expect("Failed to read line");

        if usr_input.trim_end() == "q" {
            quit = true;
        }
        // parse_from_string(usr_input);
    }
    Ok(())
}
fn print_error(error: &OutputError, ast: &OutputType, input: &str, src_name: &str) {
    let span = error.span();
    let found = error.found().unwrap_or(&parser::Token::Nothing);
    let note = if error.expected().next().is_none() {
        format!("Unexpected Token \"{found}\"")
    } else {
        let expected = error
            .expected()
            .map(|a| format!("{a:#?} "))
            .collect::<Vec<_>>()
            .concat();
        format!("Expected {expected} but found {found}")
    };

    let empty_span = parser::span_functions::span_from(span.start);
    eprintln!("[DEBUG]\n\n{}", ast.0);
    let context = error
        .contexts()
        .last()
        .unwrap_or((&"No context", &empty_span));
    let _ = Report::build(ReportKind::Error, src_name, 0)
        .with_message(format!(
            "error while parsing {:?} at {:?}",
            context.0, context.1
        ))
        .with_label(
            Label::new((src_name, span.start..span.end))
                .with_message(format!("found {found:?}",))
                .with_color(Color::Red),
        )
        .with_note(note)
        .finish()
        .eprint((src_name, Source::from(input)));
}
