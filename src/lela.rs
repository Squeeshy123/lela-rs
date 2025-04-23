use std::collections::HashMap;

#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide
}

pub struct Program<'a> {
    global_scope: Scope<'a>,
    expressions: Vec<Box<Expression>>
}

#[derive(Debug)]
// Each "line" of code in Lela is a program entry
// Which is either a definition, or an expression
pub enum ProgramEntry {
    Expression(Box<Expression>),
    //            ConstantName Value
    ConstantDefinition(String, Box<Expression>),
    //               FuncName  Parameters   Value
    FunctionDefinition(String, Vec<String>, Box<Expression>)
}


pub struct Scope<'a> {
    pub definitions: HashMap<String, &'a Box<Expression>>
}

#[derive(Debug)]
pub enum Expression {
    Number(i32),
    Operation(Operator, Box<Expression>, Box<Expression>),
    Identifier(String),
    List(Vec<Box<Expression>>),
    FunctionCall(String, Vec<Box<Expression>>)
}

fn filter_errors<'a>(entries: &'a Vec<Result<Box<ProgramEntry>, lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'a str>>>) -> Vec<&'a Box<ProgramEntry>>{
    entries.iter().filter_map(|entry| {
        match entry {
            Ok(x) => Some(x),
            Err(_) => None
        }
    }).collect()
}

fn evaluate_program<'a>(clean_program: &'a Vec<Box<ProgramEntry>>, scope: &'a mut Scope<'a>) {
    for entry in clean_program.iter() {
        match entry.as_ref() {
            ProgramEntry::Expression(expression) => {
                Some(evaluate_expression(expression, scope));
            },
            ProgramEntry::ConstantDefinition(name, expression) => {
                scope.definitions.insert(name.clone(), expression.into());
                None::<i32>;
            },
            ProgramEntry::FunctionDefinition(name, params, expr) => {
                scope.definitions.insert(name.clone(), expr.into());
                None::<i32>;
            }
        }
    }
}

// Evaluates an operation expression 
fn evaluate_operation_expression(operator: &Operator, left: &Box<Expression>, right: &Box<Expression>, scope: &Scope) -> i32 {
    match operator {
        Operator::Add => evaluate_expression(left, scope) + evaluate_expression(right, scope),
        Operator::Multiply => evaluate_expression(left, scope) * evaluate_expression(right, scope),
        Operator::Divide => evaluate_expression(left, scope) / evaluate_expression(right, scope),
        Operator::Subtract => evaluate_expression(left, scope) - evaluate_expression(right, scope)
    }
}

fn evaluate_identifier_expression(identifier_name: &String, scope: &Scope) -> i32 {
    match scope.definitions.get(identifier_name.as_str()) {
        Some(x) => evaluate_expression(x, scope),
        None => panic!("Identifier {} not in scope!", identifier_name.as_str())
    }
}

fn evaluate_function(parameters: &Vec<Box<Expression>>, value: &Box<Expression>)

fn evaluate_function_call(identifier: &String, parameters: &Vec<Box<Expression>>, scope: &Scope) {
    let function = scope.definitions.get(identifier);
    match function {
        Some(expression) => 
        None(_) => panic!("Function of the name `{}` not found", identifier); 
    }
    
}

// Evaluates a given expression with a given scope
pub fn evaluate_expression(expr: &Box<Expression>, scope: &Scope) -> i32 {
    match expr.as_ref() {
        Expression::Number (n) => return *n,
        Expression::Operation (op, left, right ) => return evaluate_operation_expression(&op, left, right, scope),
        Expression::Identifier(name) => evaluate_identifier_expression(name, scope),
        Expression::List(list) => panic!("list"),
        Expression::FunctionCall ( identifier, parameters ) => evaluate_function_call(identifier, parameters)
    }
}
