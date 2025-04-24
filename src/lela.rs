use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

pub struct Program {
    global_scope: Scope,
    expressions: Vec<Box<Expression>>,
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
    String(String)
}


// Defines a function that takes two Values of the same type. 
macro_rules! define_uniform_value_function {
    ($func_name:ident, $first_type:pat, $sec_type:pat, $to_do:block) => {
        fn $func_name(val_a: Value, val_b: Value) -> Value {
            use Value::*;
            match (val_a, val_b) {
                ($first_type, $sec_type) => $to_do,
                // TODO: this error message sucks
                (_, _) => panic!("")
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

#[derive(Debug, Clone)]
// An Expression is a tree of operations as branches and values as leaves.
// The atomic types are numbers and identifiers so far.
pub enum Expression {
    ValueExpr(Value),
    Identifier(String),
    List(Vec<Box<Expression>>),
    Operation(Operator, Box<Expression>, Box<Expression>),
    FunctionCall(String, Vec<Box<Expression>>),
}

pub fn filter_errors<'a>(
    entries: Vec<
        Result<
            Box<ProgramEntry>,
            lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'a str>,
        >,
    >,
) -> Vec<Box<ProgramEntry>> {
    entries
        .into_iter()
        .filter_map(|entry| match entry {
            Ok(x) => Some(x),
            Err(_) => None,
        })
        .collect()
}

pub fn evaluate_program(clean_program: &Vec<Box<ProgramEntry>>, scope: &mut Scope) -> Vec<Value> {
    let mut answers = Vec::new();
    for entry in clean_program.iter() {
        match entry.as_ref() {
            ProgramEntry::Expression(expression) => {
                answers.push(evaluate_expression(expression, scope));
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
    }
    answers
}

// Evaluates an operation expression, given the operator, the left expression, the right expression, and a scope.
fn evaluate_operation_expression(
    operator: &Operator,
    left: &Box<Expression>,
    right: &Box<Expression>,
    scope: &Scope,
) -> Value {
    match operator {
        Operator::Add => add_numbers(evaluate_expression(left, scope), evaluate_expression(right, scope)),
        Operator::Multiply => multiply_numbers(evaluate_expression(left, scope), evaluate_expression(right, scope)),
        Operator::Divide => divide_numbers(evaluate_expression(left, scope), evaluate_expression(right, scope)),
        Operator::Subtract => subtract_numbers(evaluate_expression(left, scope), evaluate_expression(right, scope)),
    }
}                    

fn evaluate_identifier_expression(identifier_name: &String, scope: &Scope) -> Value {
    match scope.definitions.get(identifier_name.as_str()) {
        Some(def) => match def.as_ref() {
            Definition::ConstantDefinition(name, expression) => {
                evaluate_expression(&expression, scope)
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
) -> Value {
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
) -> Value {
    match function_def {
        Definition::ConstantDefinition(name, _) => panic!("{} is not a function!", name),
        Definition::FunctionDefinition(name, def_parameters, expr) => {
            if (def_parameters.len() != given_parameters.len()) {
                panic!(
                    "Expected {} parameters, given {}",
                    def_parameters.len(),
                    given_parameters.len()
                )
            }
            let mut function_scope = scope.clone();

            for (i, param) in given_parameters.iter().enumerate() {
                let new_param_name = def_parameters.get(i).unwrap();
                let new_expression = param.as_ref().clone();
                function_scope.definitions.insert(
                    new_param_name.clone(),
                    Box::new(Definition::ConstantDefinition(
                        new_param_name.clone(),
                        Box::new(new_expression),
                    )),
                );
            }
            evaluate_expression(expr, &function_scope)
        }
    }
}

// Evaluates a given expression with a given scope
pub fn evaluate_expression(expr: &Box<Expression>, scope: &Scope) -> Value {
    match expr.as_ref() {
        Expression::ValueExpr(n) => return n.clone(),
        Expression::Operation(op, left, right) => {
            return evaluate_operation_expression(&op, left, right, scope);
        }
        Expression::Identifier(name) => evaluate_identifier_expression(name, scope),
        Expression::List(list) => panic!("list"),
        Expression::FunctionCall(identifier, parameters) => {
            evaluate_function_call(identifier, parameters, scope)
        }
    }
}
