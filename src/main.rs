use std::env;
use std::fs;
use std::collections::HashMap;
use lalrpop_util::lalrpop_mod;
use lela::Expression;
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
        "[line,\n breaks, \n should, \n be, \n fine]"
    ];
    let parser = lela_grammar::ExprParser::new();
    for string in good_code.iter() {
        assert!(parser.parse(string).is_ok(), "Parsing failed. Input = {}", string);
    }
}

#[test]
fn test_invalid_parsings() {
    let bad_code = [
        "?Fire",
        "50Fire",
        "[), (]",
        "(17 /- Fire?)",
        "this_isnt_a_function_call[these, arent, parameters]"
    ];
    let parser = lela_grammar::ExprParser::new();
    for string in bad_code.iter() {
        assert!(parser.parse(string).is_err(), "Parsing succeeded when it should've failed. Input = {}", string);
    }
}


#[test]
fn test_identifier_evaluation() {
    // create a scope
    let mut scope_map = HashMap::new();
    scope_map.insert("fifteen".to_string(), Box::new(lela::Expression::Number(15)));
    let scope = lela::Scope{definitions: scope_map};
    // create an expression that uses an identifier from the scope, and be sure that
    // it is properly evaluated
    let test_expression = Box::new(lela::Expression::Operation(
                                    lela::Operator::Add,
                                    Box::new(lela::Expression::Number(5)),
                                    Box::new(lela::Expression::Identifier("fifteen".to_string()))));

    assert_eq!(lela::evaluate_expression(&test_expression, &scope), 20);    
}

// Reads the contents of a file, given its relative path.
fn read_from_file(file_name: &String) -> String {
    // TODO: change to a match expression:
    fs::read_to_string(file_path).expect("Should have been able to read the file")
}

fn read_program() {
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];

    let source = read_from_file(file_path);

    

    println!("With text:\n{contents}");
    
}

fn parse_program_string(source: &String) -> Vec<Box<Expression>> {
    let parser = lela_grammar::ExprParser::new();
    let split_file_input = source.split(";");
    return split_file_input.map(|input_line| parser.parse(input_line)).collect();
}

#[test]
fn test_program_parsing() {
    
}

fn main() {
    
}
