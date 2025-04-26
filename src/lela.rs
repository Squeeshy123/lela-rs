use std::collections::HashMap;
// Illegal Expressions can be created. Since this is a dynamically typed language,
// types are only important once evaluation occurs.

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Equals
}

#[derive(Debug, Clone)]
pub enum Definition {
    //              ConstantName Value
    ConstantDefinition(String, Box<Expression>),
    //               FuncName  Parameters   Value
    FunctionDefinition(String, Vec<String>, Box<Expression>),
}

#[derive(Debug)]
// Each "line" of code in Lela is a program entry
// Which is either a definition, or an expression
pub enum ProgramEntry {
    Expression(Box<Expression>),
    Definition(Definition),
}

#[derive(Clone)]
// A Scope is a set of definitions that 'replace' identifiers,
// if they correlate
pub struct Scope {
    pub definitions: HashMap<String, Box<Definition>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(String),
    String(String),
    Boolean(String),
}

#[derive(Debug)]
pub enum LelaError {
    EvaluationError(String),
    SyntaxError(String),
}

fn parse_boolean(val: &String) -> Result<bool, LelaError> {
    match val.as_str() {
        "#true" => Ok(true),
        "#false" => Ok(false),
        _ => Err(LelaError::EvaluationError(format!("{:?} is not a valid boolean", val.clone()).to_string())),
    }
}

fn same_type(val_a: &Value, val_b: &Value) -> bool {
    match (val_a, val_b) {
        (Value::Number(_), Value::Number(_)) => true,
        (Value::String(_), Value::String(_)) => true,
        (Value::Boolean(_), Value::Boolean(_)) => true,
        (_, _) => false,
    }
}

// Defines a function that takes two Values of the same type.
macro_rules! define_uniform_value_function {
    ($func_name:ident, $first_type:pat, $sec_type:pat, $to_do:block) => {
        fn $func_name(val_a: &Value, val_b: &Value) -> Value {
            match (val_a, val_b) {
                ($first_type, $sec_type) => $to_do,
                // TODO: this error message sucks
                (_, _) => panic!(""),
            }
        }
    };
}

define_uniform_value_function!(add_numbers, Value::Number(a), Value::Number(b), {
    Value::Number((a.parse::<i32>().unwrap() + b.parse::<i32>().unwrap()).to_string())
});

define_uniform_value_function!(subtract_numbers, Value::Number(a), Value::Number(b), {
    Value::Number((a.parse::<i32>().unwrap() - b.parse::<i32>().unwrap()).to_string())
});

define_uniform_value_function!(multiply_numbers, Value::Number(a), Value::Number(b), {
    Value::Number((a.parse::<i32>().unwrap() * b.parse::<i32>().unwrap()).to_string())
});

define_uniform_value_function!(divide_numbers, Value::Number(a), Value::Number(b), {
    Value::Number((a.parse::<i32>().unwrap() * b.parse::<i32>().unwrap()).to_string())
});

define_uniform_value_function!(and_booleans, Value::Boolean(a), Value::Boolean(b), {
    Value::Boolean((parse_boolean(&a).unwrap() && parse_boolean(&b).unwrap()).to_string())
});

define_uniform_value_function!(or_booleans, Value::Boolean(a), Value::Boolean(b), {
    Value::Boolean((parse_boolean(&a).unwrap() || parse_boolean(&b).unwrap()).to_string())
});

define_uniform_value_function!(equals_booleans, Value::Boolean(a), Value::Boolean(b), {
    Value::Boolean((parse_boolean(&a).unwrap() == parse_boolean(&b).unwrap()).to_string())
});
define_uniform_value_function!(equals_number, Value::Number(a), Value::Number(b), {
    Value::Boolean(format!("#{}", (a.parse::<i32>().unwrap() == b.parse::<i32>().unwrap())).to_string())
});

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
    SquareRoot,
}

#[derive(Debug, Clone, PartialEq)]
/// An Expression is a tree of operations as branches and values as leaves.
pub enum Expression {
    ValueExpr(Value),
    Identifier(String),
    List(Vec<Box<Expression>>),
    Operation(Operator, Box<Expression>, Box<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    ConditionalTree(Vec<Box<Expression>>, Vec<Box<Expression>>), // must be the same length
    FunctionCall(String, Vec<Box<Expression>>),
}

pub fn evaluate_program(
    clean_program: Vec<Result<Box<ProgramEntry>, LelaError>>,
    scope: &mut Scope,
) -> Vec<Result<Value, LelaError>> {
    let mut answers = Vec::new();
    for potential_entry in clean_program.into_iter() {
        match potential_entry {
            Ok(entry) => match entry.as_ref() {
                ProgramEntry::Expression(expression) => {
                    answers.push(evaluate_expression(&expression, scope));
                }
                ProgramEntry::Definition(definition) => match definition {
                    Definition::ConstantDefinition(name, _) => {
                        scope
                            .definitions
                            .insert(name.clone(), Box::new(definition.to_owned()));
                    }
                    Definition::FunctionDefinition(name, _, _) => {
                        scope
                            .definitions
                            .insert(name.clone(), Box::new(definition.to_owned()));
                    }
                },
            }
            Err(e) => answers.push(Err(e))
        }
    }
    answers
}

// Evaluates an operation expression, given the operator, the left expression, the right expression, and a scope.
fn evaluate_operation_expression(
    operator: &Operator,
    left: &Box<Expression>,
    right: &Box<Expression>,
    scope: &Scope,
) -> Result<Value, LelaError> {
    match operator {
        Operator::Add => Ok(add_numbers(
            &evaluate_expression(left, scope)?,
            &evaluate_expression(right, scope)?,
        )),
        Operator::Multiply => Ok(multiply_numbers(
            &evaluate_expression(left, scope)?,
            &evaluate_expression(right, scope)?,
        )),
        Operator::Divide => Ok(divide_numbers(
            &evaluate_expression(left, scope)?,
            &evaluate_expression(right, scope)?,
        )),
        Operator::Subtract => Ok(subtract_numbers(
            &evaluate_expression(left, scope)?,
            &evaluate_expression(right, scope)?,
        )),
        Operator::And => {
            Ok(and_booleans(
                &evaluate_expression(left, scope)?,
                &evaluate_expression(right, scope)?,
            ))
        }
        Operator::Or => {
            Ok(or_booleans(
                &evaluate_expression(left, scope)?,
                &evaluate_expression(right, scope)?,
            ))
        }
        Operator::Equals => {
            let left = evaluate_expression(left, scope)?;
            let right = evaluate_expression(right, scope)?;
            match (&left, &right) {
                (Value::Number(_), Value::Number(_)) => Ok(equals_number(&left, &right)),
                (Value::Boolean(_), Value::Boolean(_)) => Ok(equals_booleans(&left, &right)),
                (_, _) => Err(LelaError::EvaluationError("Cannot perform equals on these two types".to_string()))
            }

        }
    }
}

fn evaluate_identifier_expression(
    identifier_name: &String,
    scope: &Scope,
) -> Result<Value, LelaError> {
    match scope.definitions.get(identifier_name.as_str()) {
        Some(def) => match def.as_ref() {
            Definition::ConstantDefinition(_name, expression) => {
                Ok(evaluate_expression(&expression, scope)?)
            }
            Definition::FunctionDefinition(_, _, _) => {
                unreachable!("This identifer is somehow evaluating to a function")
            }
        },
        None => panic!("Identifier {} not in scope!", identifier_name.as_str()),
    }
}

fn evaluate_function_call(
    identifier: &String,
    parameters: &Vec<Box<Expression>>,
    scope: &Scope,
) -> Result<Value, LelaError> {
    let function = scope.definitions.get(identifier);
    match function {
        Some(definition) => evaluate_function_call_with_definition(definition, parameters, scope),
        None => panic!("Function of the name `{}` not found", identifier),
    }
}

fn evaluate_function_call_with_definition(
    function_def: &Definition,
    given_parameters: &Vec<Box<Expression>>,
    scope: &Scope,
) -> Result<Value, LelaError> {
    match function_def {
        Definition::ConstantDefinition(name, _) => panic!("{} is not a function!", name),
        Definition::FunctionDefinition(_name, def_parameters, expr) => {
            if def_parameters.len() != given_parameters.len() {
                panic!(
                    "Expected {} parameters, given {}",
                    def_parameters.len(),
                    given_parameters.len()
                )
            }
            let mut function_scope = scope.clone();

            for (i, param) in given_parameters.iter().enumerate() {
                let new_param_name = def_parameters.get(i).unwrap();
                let new_expression = evaluate_expression(param, scope)?;
                function_scope.definitions.insert(
                    new_param_name.clone(),
                    Box::new(Definition::ConstantDefinition(
                        new_param_name.clone(),
                        Box::new(Expression::ValueExpr(new_expression)),
                    )),
                );
            }
            evaluate_expression(expr, &function_scope)
        }
    }
}

// Evaluates a given expression with a given scope
pub fn evaluate_expression(expr: &Box<Expression>, scope: &Scope) -> Result<Value, LelaError> {
    match expr.as_ref() {
        Expression::ValueExpr(n) => Ok(n.clone()),
        Expression::Operation(op, left, right) => {
            evaluate_operation_expression(&op, left, right, scope)
        }
        Expression::Identifier(name) => Ok(evaluate_identifier_expression(name, scope)?),
        Expression::List(_list) => panic!("list"),
        Expression::FunctionCall(identifier, parameters) => {
            evaluate_function_call(identifier, parameters, scope)
        }
        Expression::ConditionalTree(cases, values) => evaluate_conditional(cases, values, scope),
        Expression::UnaryOperation(op, expr) => {
            evaluate_unary_operation(op, &evaluate_expression(expr, scope)?)
        }
    }
}

fn evaluate_unary_operation(operation: &UnaryOperator, val: &Value) -> Result<Value, LelaError> {
    match operation {
        UnaryOperator::Negate => match val {
            Value::Number(n) => Ok(Value::Number((-1 * n.parse::<i32>().unwrap()).to_string())),
            _ => Err(LelaError::EvaluationError(
                "Cannot negate a non-number".to_string(),
            )),
        },
        UnaryOperator::Not => match val {
            Value::Boolean(b) => Ok(Value::Boolean((!(parse_boolean(b).unwrap())).to_string())),
            _ => Err(LelaError::EvaluationError(
                "Cannot perform a not operations on a non-boolean value".to_string(),
            )),
        },
        UnaryOperator::SquareRoot => match val {
            Value::Number(n) => Ok(Value::Number(n.parse::<i32>().unwrap().to_string())),
            _ => Err(LelaError::EvaluationError(
                "Cannot perform a square root on a non-number value".to_string(),
            )),
        },
    }
}

fn evaluate_conditional(
    cases: &Vec<Box<Expression>>,
    values: &Vec<Box<Expression>>,
    scope: &Scope,
) -> Result<Value, LelaError> {
    let mut answer: Result<Value, LelaError> = Err(LelaError::EvaluationError(
        "Nothing in this program".to_string(),
    ));
    if cases.len() == values.len() {
        for (i, cond) in cases.iter().enumerate() {
            match evaluate_expression(cond, scope) {
                Ok(Value::Boolean(val)) => match parse_boolean(&val) {
                    Ok(b) => {
                        if b {
                            answer = evaluate_expression(&values[i], scope);
                            break;
                        }
                    }
                    Err(e) => answer = Err(e)
                },
                Err(e) => panic!("{:?}", e),

                _ => {}
            }
        }
        match answer {
            Ok(answer) => Ok(answer),
            Err(e) => Err(e),
        }
    } else {
        answer
    }
}
