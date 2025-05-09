use crate::lela::LelaError;
use lalrpop_util::lalrpop_mod;
use lela::create_default_scope;
use lela::{ProgramEntry, evaluate_program};
use lela_grammar::ProgramParser;
use std::env;
use std::fs;

pub mod lela;

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

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut file_path: Option<String> = args.get(1).cloned();
    if file_path.is_none() {
        file_path = Some("sample_code.lela".to_string());
    }
    let read = read_from_file(&file_path.unwrap());

    let mut errors = Vec::new();

    // Sample parsing code for a single expression
    let parser = ProgramParser::new();
    let output = parser.parse(&mut errors, &read);
    let program: Vec<Result<Box<ProgramEntry>, LelaError>> = match output {
        Ok(entries) => entries
            .into_iter()
            .map(|entry| match entry.as_ref() {
                ProgramEntry::ParseError(s) => Err(LelaError::SyntaxError(s.clone())),
                _ => Ok(entry),
            })
            .collect(),
        Err(error) => panic!("{}", error),
    };

    /*match output {
        Ok(entries) => entries
            .iter()
            .map(|entry| match entry.as_ref() {
                ProgramEntry::ParseError => Err(LelaError::SyntaxError("".to_string())),
                _ => Ok(entry),
            })
            .collect(),
        Err(_) => Err(LelaError::SyntaxError("".to_string())),
    };*/

    //println!("{:?}", errors);
    //println!("{:?}", &program);
    let mut def_scope = create_default_scope();
    for val in evaluate_program(program, &mut def_scope) {
        println!("{:?}", val);
    }
}
