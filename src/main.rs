use crate::lela::LelaError;
use lalrpop_util::lalrpop_mod;
use lela::create_default_scope;
use lela::{evaluate_program, ProgramEntry};
use lela_grammar::ProgramParser;
use lela_grammar::Token;
use std::env;
use std::fs;

pub mod lela;
pub mod tests;

lalrpop_mod!(pub lela_grammar); // synthesized by LALRPOP

// Reads the contents of a file, given its relative path.
fn read_from_file(file_path: &String) -> String {
    // TODO: change to a match expression:
    fs::read_to_string(file_path).expect("Should have been able to read the file")
}

// We want to take a file in the following form:
// func something(some, parameters) {
//   (does something)
// }
//
// something(hello, hi)

fn map_parsing_to_program(
    input: Result<Vec<Box<ProgramEntry>>, lalrpop_util::ParseError<usize, Token, &'static str>>,
) -> Vec<Result<Box<ProgramEntry>, LelaError>> {
    match input {
        Ok(entries) => entries
            .into_iter()
            .map(|entry| match entry.as_ref() {
                ProgramEntry::ParseError(s) => Err(LelaError::SyntaxError(s.clone())),
                _ => Ok(entry),
            })
            .collect(),
        Err(error) => panic!("{}", error),
    }
}

fn run_file(file_path: &String) {
    let read = read_from_file(&file_path);

    let mut errors = Vec::new();

    // Sample parsing code for a single expression
    let parser = ProgramParser::new();
    let output: Result<Vec<Box<ProgramEntry>>, lalrpop_util::ParseError<_, _, _>> =
        parser.parse(&mut errors, &read);
    let program: Vec<Result<Box<ProgramEntry>, LelaError>> = map_parsing_to_program(output);

    let mut def_scope = create_default_scope();
    for result in evaluate_program(program, &mut def_scope) {
        match result {
            Ok(val) => println!("{}", val),
            Err(error) => println!("{}", error),
        }
    }
}


use std::thread;

const STACK_SIZE: usize = 4 * 1024 * 1024;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut file_path: Option<String> = args.get(1).cloned();
    if file_path.is_none() {
        file_path = Some("sample_code.lela".to_string());
    }
    // Spawn thread with explicit stack size
    let child = thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(move | | run_file(&file_path.unwrap()))
        .unwrap();

    // Wait for thread to join
    child.join().unwrap();
}
