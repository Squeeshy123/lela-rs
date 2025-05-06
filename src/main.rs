use crate::lela::{Expression, LelaError, Operator, evaluate_expression, evaluate_program};
use crate::lela_grammar::DefinitionOrExpressionParser;
use lalrpop_util::lalrpop_mod;
use lela::{FunctionObject, ProgramEntry, Value};
use std::collections::HashMap;
use std::env;
use std::fs;

pub mod lela;

lalrpop_mod!(pub lela_grammar); // synthesized by LALRPOP

// Reads the contents of a file, given its relative path.
fn read_from_file(file_path: &String) -> String {
    // TODO: change to a match expression:
    fs::read_to_string(file_path).expect("Should have been able to read the file")
}

// Splits a given string on how a program should be split.
fn split_as_program(src: &String) -> Vec<String> {
    src.split(";").map(|s| s.to_string()).collect()
}

// Given a file path, read it as a program
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

// Creates an empty scope.
fn create_empty_scope() -> lela::Scope {
    let global_scope = lela::Scope {
        definitions: HashMap::new(),
    };
    global_scope
}

fn create_default_scope() -> lela::Scope {
    let mut scope = create_empty_scope();

    lela::define_function(
        &mut scope,
        "first",
        vec!["list".to_owned()],
        FunctionObject::new(|_| {
            Ok(Box::new(Expression::UnaryOperation(
                lela::UnaryOperator::First,
                Box::new(Expression::Identifier("list".to_string())),
            )))
        }),
    );
    lela::define_function(
        &mut scope,
        "rest",
        vec!["list".to_owned()],
        FunctionObject::new(|_| {
            Ok(Box::new(Expression::UnaryOperation(
                lela::UnaryOperator::Rest,
                Box::new(Expression::Identifier("list".to_string())),
            )))
        }),
    );
    scope
}

/// Given a Vector of Strings, evaluate each as a lela program.
fn run_program(program: &Vec<String>) {
    let mut global_scope = create_default_scope();
    let decommented: Vec<String> = program.iter().map(|s| s.to_owned()).filter(|s| !s.starts_with("//")).collect();
    println!("{:?}", decommented);
    let cleaned_program = parse_program_strings(&decommented)
        .into_iter()
        .map(|entry| match entry {
            Ok(r) => Ok(r),
            Err(e) => Err(LelaError::SyntaxError(e.to_string())),
        })
        .collect();
    // Create answers from our list of expressions
    let answers = lela::evaluate_program(cleaned_program, &mut global_scope);
    // Prints the evaluations
    for ans in answers {
        println!("{:?}", ans)
    }
}

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
    let parser = DefinitionOrExpressionParser::new();
    for string in good_code.iter() {
        assert!(
            parser.parse(string).is_ok(),
            "Parsing failed. Input = {}",
            string
        );
    }
}

#[test]
fn test_proper_conditional_parsing() {
    let cases = vec![
        Box::new(Expression::ValueExpr(Value::Boolean("#true".to_string()))),
        Box::new(Expression::ValueExpr(Value::Boolean("#true".to_string()))),
    ];
    let values = vec![
        Box::new(Expression::ValueExpr(Value::Number(15.to_string()))),
        Box::new(Expression::ValueExpr(Value::Number(0.to_string()))),
    ];
    let five = Box::new(Expression::ConditionalTree(cases, values));

    let cond1 = "if #true { 15 } else { 0 }";
    let parser = DefinitionOrExpressionParser::new();
    let parse_result = parser.parse(cond1).unwrap();

    match parse_result.as_ref() {
        ProgramEntry::Expression(expr) => {
            assert_eq!(expr.clone(), five)
        }
        ProgramEntry::Definition(_) => {
            panic!("Parsing should have returned an expression.")
        }
    }
}

#[test]
fn test_valid_evaluations() {
    let fifteen = Expression::ValueExpr(Value::Number(15.to_string()));
    // let two = Expression::ValueExpr(Value::Number(2.to_string()));
    let one_and_a_half = Expression::ValueExpr(Value::Number(1.5.to_string()));
    let also_one_and_a_half = Expression::ValueExpr(Value::Number("3/2".to_string()));
    // let hello = Expression::ValueExpr(Value::String("hello".to_string()));
    // let true_val = Expression::ValueExpr(Value::String("true".to_string()));
    // let false_val = Expression::ValueExpr(Value::Boolean("false".to_string()));

    let thirty_added = Expression::Operation(
        Operator::Add,
        Box::new(fifteen.clone()),
        Box::new(fifteen.clone()),
    );
    let three_added = Expression::Operation(
        Operator::Add,
        Box::new(one_and_a_half.clone()),
        Box::new(also_one_and_a_half.clone()),
    );
    let test_scope = create_empty_scope();

    assert_eq!(
        lela::evaluate_expression(&Box::new(thirty_added), &test_scope).unwrap(),
        Value::Number(30.to_string())
    );
    assert_eq!(
        lela::evaluate_expression(&Box::new(three_added), &test_scope).unwrap(),
        Value::Number(3.to_string())
    );
    assert_eq!(
        lela::evaluate_expression(&Box::new(also_one_and_a_half), &test_scope).unwrap(),
        Value::Number(1.5.to_string())
    );
}

#[test]
fn test_proper_struct_definition() {
    // Test that our three (types of) functions get created
    let program_str = "struct book [isbn, author_id, release_date]; \n let htdp = make_book(100, 256, 02102001) \n (book.isbn(htdp) - 1)";
    let splits = split_as_program(&program_str.to_string());
    let program = clean_program(parse_program_strings(&splits));

    lela::evaluate_program(program, &mut create_empty_scope());
}

#[test]
fn test_parsing_conditional_expressions() {
    let cond1 = "if #true { (10 + 5) } else { (0 + 0) }";
    let parser = DefinitionOrExpressionParser::new();
    match parser.parse(cond1).unwrap().as_ref() {
        ProgramEntry::Expression(expr) => match evaluate_expression(expr, &create_empty_scope()) {
            Ok(val) => assert_eq!(val, Value::Number(15.to_string())),
            Err(e) => panic!("{:?}", e),
        },
        ProgramEntry::Definition(def) => {
            panic!("The following is being parsed as a definition: {:?}", def)
        }
    }
}

#[test]
fn test_recursive_program() {
    let recursive_prog_string = "func factorial(n) {
            switch {
                case (n == 1): (1),
                    case #true: factorial((n - 1)),
                }
            };
        factorial(2)";
    let mut scope = create_empty_scope();
    let binding = split_as_program(&recursive_prog_string.to_string());
    let program = parse_program_strings(&binding);
    let cleaned = clean_program(program);
    evaluate_program(cleaned, &mut scope);
}

#[test]
fn test_invalid_parsings() {
    let bad_code = [
        "?Fire",
        "50Fire",
        "[), (]",
        "(17 /- Fire?)",
        "this_isnt_a_function_call[this, is, a, list, [not, parameters]]",
    ];
    let parser = DefinitionOrExpressionParser::new();
    for string in bad_code.iter() {
        assert!(
            parser.parse(string).is_err(),
            "Parsing succeeded when it should've failed. Input = {}",
            string
        );
    }
}

// Tests that identifiers are properly evaluated in the given scope.
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

    assert_eq!(
        lela::evaluate_expression(&test_expression, &scope).unwrap(),
        Value::Number(20.to_string())
    );
}

#[test]
// Runs the sample_code.lela file
fn test_sample_program() {
    let input = "sample_code.lela".to_string();
    let program_read = read_program_from_file(&input);

    run_program(&program_read);
}

fn clean_program(
    parsed: Vec<
        Result<
            Box<ProgramEntry>,
            lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'_>, &str>,
        >,
    >,
) -> Vec<Result<Box<ProgramEntry>, LelaError>> {
    parsed
        .into_iter()
        .map(|entry| match entry {
            Ok(r) => Ok(r),
            Err(e) => Err(LelaError::SyntaxError(e.to_string())),
        })
        .collect()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut file_path: String = args[1].clone();
    if file_path.is_empty() {
        file_path = "sample_code.lela".to_owned();
    }
    let read = read_program_from_file(&file_path);

    run_program(&read);
}
