use std::collections::HashMap;

#[derive(Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide
}

pub struct Program {
    global_scope: Scope,
    expressions: Vec<Box<Expression>>
}

pub struct Scope {
    pub definitions: HashMap<String, Box<Expression>>
}

#[derive(Debug)]
pub enum Expression {
    Number(i32),
    Operation(Operator, Box<Expression>, Box<Expression>),
    Identifier(String),
    List(Vec<Box<Expression>>),
    FunctionCall(String, Vec<Box<Expression>>)
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

// Evaluates a given expression with a given scope
pub fn evaluate_expression(expr: &Box<Expression>, scope: &Scope) -> i32 {
    match expr.as_ref() {
        Expression::Number (n) => return *n,
        Expression::Operation (op, left, right ) => return evaluate_operation_expression(&op, left, right, scope),
        Expression::Identifier(name) => evaluate_identifier_expression(name, scope),
        Expression::List(list) => panic!("list"),
        Expression::FunctionCall ( identifier, parameters ) => panic!("not here yet")
    }
}
