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
    let test_scope = lela::create_empty_scope();

    assert_eq!(
        (&Box::new(thirty_added))
            .evaluate_expression(&test_scope)
            .unwrap(),
        Value::Number(30.to_string())
    );
    assert_eq!(
        (&Box::new(three_added))
            .evaluate_expression(&test_scope)
            .unwrap(),
        Value::Number(3.to_string())
    );
    assert_eq!(
        (&Box::new(also_one_and_a_half))
            .evaluate_expression(&test_scope)
            .unwrap(),
        Value::Number(1.5.to_string())
    );
}

#[test]
fn test_proper_struct_definition() {
    // Test that our three (types of) functions get created
    let program_str = "struct book [isbn, author_id, release_date]; \n let htdp = make_book(100, 256, 02102001) \n (book.isbn(htdp) - 1)";
    let splits = split_as_program(&program_str.to_string());
    let program = clean_program(parse_program_strings(&splits));

    lela::evaluate_program(program, &mut lela::create_empty_scope());
}

#[test]
fn test_parsing_conditional_expressions() {
    let cond1 = "if #true { (10 + 5) } else { (0 + 0) }";
    let parser = DefinitionOrExpressionParser::new();
    match parser.parse(cond1).unwrap().as_ref() {
        ProgramEntry::Expression(expr) => {
            match expr.evaluate_expression(&lela::create_empty_scope()) {
                Ok(val) => assert_eq!(val, Value::Number(15.to_string())),
                Err(e) => panic!("{:?}", e),
            }
        }
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
    let mut scope = lela::create_empty_scope();
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
        (&test_expression).evaluate_expression(&scope).unwrap(),
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
