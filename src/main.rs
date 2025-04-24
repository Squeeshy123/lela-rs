use crate::lela_grammar::DefinitionOrExpressionParser;
use lalrpop_util::lalrpop_mod;
use lela::Expression;
use lela::ProgramEntry;
use lela::evaluate_expression;
use lela::Value;
use std::collections::HashMap;
use std::env;
use std::fs;
pub mod lela;

lalrpop_mod!(pub lela_grammar); // synthesized by LALRPOP

#[test]
fn test_valid_parsings() {
    let good_code = [
        "((8 - 2) / 1)",
        "((42 - 3) + 5)",
        "(22 + 3)",
        "(17 / Fire?)",
        "this_is_a_function_call(these, are, parameters)",
        "[line,\n breaks, \n should, \n be, \n fine]",
    ];
    let parser = lela_grammar::DefinitionOrExpressionParser::new();
    for string in good_code.iter() {
        assert!(
            parser.parse(string).is_ok(),
            "Parsing failed. Input = {}",
            string
        );
    }
}

#[test]
fn test_invalid_parsings() {
    let bad_code = [
        "?Fire",
        "50Fire",
        "[), (]",
        "(17 /- Fire?)",
        "this_isnt_a_function_call[these, arent, parameters]",
    ];
    let parser = lela_grammar::DefinitionOrExpressionParser::new();
    for string in bad_code.iter() {
        assert!(
            parser.parse(string).is_err(),
            "Parsing succeeded when it should've failed. Input = {}",
            string
        );
    }
}

#[test]
fn test_identifier_evaluation() {
    // create a scope
    let mut scope_map = HashMap::new();
    let new_expr = Box::new(lela::Definition::ConstantDefinition(
        "fifteen".to_string(),
        Box::new(lela::Expression::ValueExpr(Value::Number(15.to_string()))),
    ));
    scope_map.insert("fifteen".to_string(), new_expr);
    let scope = lela::Scope {
        definitions: scope_map,
    };
    // create an expression that uses an identifier from the scope, and be sure that
    // it is properly evaluated
    let test_expression = Box::new(lela::Expression::Operation(
        lela::Operator::Add,
        Box::new(lela::Expression::ValueExpr(Value::Number(5.to_string()))),
        Box::new(lela::Expression::Identifier("fifteen".to_string())),
    ));

    assert_eq!(lela::evaluate_expression(&test_expression, &scope), Value::Number(20.to_string()));
}

// Reads the contents of a file, given its relative path.
fn read_from_file(file_path: &String) -> String {
    // TODO: change to a match expression:
    fs::read_to_string(file_path).expect("Should have been able to read the file")
}

fn split_as_program(src: &String) -> Vec<String> {
    src.split(";").map(|s| s.to_string()).collect()
}

// Reads a file and splits it as a program
fn read_program_from_file(file_path: &String) -> Vec<String> {
    let source = read_from_file(file_path);
    let split_file_input = split_as_program(&source);
    let contents: Vec<String> = split_file_input;
    contents
}

// Produces parsed ASTs given a vector of strings
fn parse_program_strings(
    source: &Vec<String>,
) -> Vec<
    Result<
        Box<ProgramEntry>,
        lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &str>,
    >,
> {
    let parser = DefinitionOrExpressionParser::new();
    let output = source
        .iter()
        .map(|input_line| parser.parse(input_line))
        .collect();
    output
}

#[test]
fn test_program_reading() {
    // let args: Vec<String> = env::args().collect();
    // let file_path = &args[1];

    let input = "sample_code.lela".to_string();
    let program_read = read_program_from_file(&input);
    let mut global_scope = lela::Scope {
        definitions: HashMap::new(),
    };
    let cleaned_program = lela::filter_errors(parse_program_strings(&program_read).into());
    // Create answers from our expressions
    let answers = lela::evaluate_program(&cleaned_program, &mut global_scope);
    // Prints the evaluations
    for ans in answers {
        println!("{:?}", ans)
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let read = read_program_from_file(file_path);
    println!("{:?}", read);
    let parsed = parse_program_strings(&read);
    println!("{:?}", parsed);

    let mut global_scope = lela::Scope {
        definitions: HashMap::new(),
    };

    let cleaned_program = lela::filter_errors(parsed.into());

    // Create answers from our expressions
    let answers = lela::evaluate_program(&cleaned_program, &mut global_scope);
    // Prints the evaluations
    for ans in answers {
        println!("{:?}", ans)
    }
}
