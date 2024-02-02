#![allow(non_snake_case)]
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
    let lex_result = lex_arrow_program(&input_verified.to_owned());
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
    } else {
        for token in lex_result.tokens() {
            println!("{} {:?}", "Match!".green(), token);
        }
        let _parse_result = parse_from_lex(&range_into_span(lex_result.tokens()));
    }
    let mut quit = false;
    while !quit {
        let mut temp_input = String::new();
        std::io::stdin()
            .read_line(&mut temp_input)
            .expect("Failed to read line");
        if temp_input == "q" {
            quit = true;
        }
    }
}
#[cfg(test)]
mod tests;
