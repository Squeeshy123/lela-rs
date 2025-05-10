use std::collections::HashMap;
use std::ops::Add;

use std::clone::Clone;
use std::fmt::{self, Display};
use std::sync::Arc;

#[derive(Clone)]
pub struct FunctionObject(Arc<dyn Fn(&Scope) -> Result<Box<Expression>, LelaError>>);

impl fmt::Debug for FunctionObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionObject(...)")
    }
}

impl FunctionObject {
    pub fn new<F>(func: F) -> Self
    where
        F: Fn(&Scope) -> Result<Box<Expression>, LelaError> + 'static,
    {
        FunctionObject(Arc::new(func))
    }

    pub fn call(&self, scope: &Scope) -> Result<Box<Expression>, LelaError> {
        (self.0)(scope)
    }
}

// Illegal Expressions can be created. Since this is a dynamically typed language,
// types are only important once evaluation occurs.
// I was tempted to make it impossible for the Expression data structure to represent an illegal
// state, but alas, I must allow it because that would require it to be a statically typed language.
// Dynamically typed languages must handle typing at runtime

#[derive(Debug, Clone, PartialEq)]
pub enum LelaError {
    EvaluationError(String),
    SyntaxError(String),
}

impl Display for LelaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EvaluationError(msg) => write!(f, "Evaluation Error: {}", msg),
            Self::SyntaxError(msg) => write!(f, "Syntax Error: {}", msg)
        }
    }
}

// Each "line" of code in Lela is a program entry
// Which is either a definition, or an expression
#[derive(Debug)]
pub enum ProgramEntry {
    Expression(Box<Expression>),
    Definition(Definition),
    Comment,
    ParseError(String),
}

#[derive(Debug, Clone)]
pub enum Definition {
    // Constant Name, Value
    ConstantDefinition(String, Box<Expression>),
    // Function Name,  Parameters, Value
    FunctionDefinition(String, Vec<String>, FunctionObject),
    // Struct name, Struct fields
    StructDefinition(String, Vec<String>),
}

/// An Expression is a tree of operations as branches and values as leaves.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    ValueExpr(Value),
    Identifier(String),
    Operation(Operator, Box<Expression>, Box<Expression>),
    UnaryOperation(UnaryOperator, Box<Expression>),
    ConditionalTree(Vec<Box<Expression>>, Vec<Box<Expression>>), // must be the same length
    FunctionCall(String, Vec<Box<Expression>>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::ValueExpr(value) => write!(f, "{}", value),
            Expression::Identifier(name) => write!(f, "{}", name),
            Expression::Operation(op, left, right) => write!(f, "{left} {op} {right}",),
            Expression::UnaryOperation(op, expr) => write!(f, "{op}({expr})"),
            Expression::ConditionalTree(conds, vals) => {
                for (i, cond) in conds.iter().enumerate() {
                    write!(f, "[{cond}, {}]", vals.get(i).unwrap())?
                }
                Ok(())
            },
            Expression::FunctionCall(name, params) => write!(f, "{name}({:?})", params),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate,
    Not,
    SquareRoot,
    First,
    Rest,
}
impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            UnaryOperator::Negate => "-",
            UnaryOperator::Not => "not",
            UnaryOperator::SquareRoot => "sqrt",
            UnaryOperator::First => "first",
            UnaryOperator::Rest => "rest",
        };
        write!(f, "{op}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    GreaterEqTo,
    LessEqTo,
    And,
    Or,
    Equals,
}

impl Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op= match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::GreaterThan => ">",
            Operator::LessThan => "<",
            Operator::GreaterEqTo => ">=",
            Operator::LessEqTo => "<=",
            Operator::And => "and",
            Operator::Or => "or",
            Operator::Equals => "==",
        };

        write!(f, "{}", op)
    }
}

// A Scope is a set of definitions that 'replace' identifiers,
// if they correlate
#[derive(Clone)]
pub struct Scope {
    pub definitions: HashMap<String, Box<Definition>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(String),
    String(String),
    Boolean(String),
    Struct(String, Vec<Result<Box<Expression>, LelaError>>),
    Pair(Box<Expression>, Box<Expression>),
    Empty,
    List(Vec<Box<Expression>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, r##""{}""##, val),
            Value::Boolean(val) => write!(f, "{}", val),
            Value::Struct(name, expressions) => {
                write!(f, "{}(", name)?;
                for (i, res) in expressions.iter().enumerate() {
                    let val = match res {
                        Ok(expr) => format!("{}", expr),
                        Err(msg) => format!("{}", msg),
                    };
                    
                    if i < expressions.len() - 1 {
                        write!(f, "{val}, ")?;
                    } else {
                        write!(f, "{val}")?;
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::Pair(left, right) => write!(f, "cons({}, {})", left, right),
            Value::Empty => write!(f, "#[]"),
            Value::List(expressions) => write!(f, "#[{:?}]", expressions),
        }
    }
}

pub fn vec_to_pair_list(v: &Vec<Box<Expression>>) -> Expression {
    let p = v.split_first();
    match p {
        Some((first, rest)) => Expression::ValueExpr(Value::Pair(
            first.to_owned(),
            Box::new(vec_to_pair_list(&rest.to_vec())),
        )),
        None => Expression::ValueExpr(Value::Empty),
    }
}

// Given a string, attempt to parse it to a lela boolean
fn parse_boolean(val: &String) -> Result<bool, LelaError> {
    match val.as_str() {
        "#true" => Ok(true),
        "#false" => Ok(false),
        _ => Err(LelaError::EvaluationError(
            format!("{:?} is not a valid boolean.", val.clone()).to_string(),
        )),
    }
}

// Given the name and field-names of a struct, define it in the given scope.
pub fn define_struct(struct_name: String, fields: Vec<String>, scope: &mut Scope) {
    let make_struc_name = format!("make_{}", struct_name.clone().to_ascii_lowercase());
    let accessor = |field: &str| format!("{}.{}", struct_name.clone().to_ascii_lowercase(), field);
    let predicate = format!("{}?", struct_name.clone().to_ascii_lowercase());

    // Struct Maker

    // Struct Accessors
    for (i, field_name) in fields.iter().enumerate() {
        define_function(
            scope,
            accessor(&field_name).as_str(),
            vec!["this".to_string()],
            FunctionObject::new(move |fun_scope| {
                match (&Box::new(Expression::Identifier("this".to_string())))
                    .evaluate_expression(fun_scope)
                {
                    Ok(Value::Struct(_name, vals)) => {
                        let eval =
                            (&<Result<Box<Expression>, LelaError> as Clone>::clone(&vals[i])?)
                                .evaluate_expression(&fun_scope)?;

                        Ok(Box::new(Expression::ValueExpr(eval)))
                    }
                    _ => panic!("this should really be in a result format..."),
                }
            }),
        );
    }

    // Struct Predicate?
    // We may not need the struct predicate if we can make use of the (x is y) syntax.
    // the above seems like a more idiomatic and syntactically sensible approach.
    // But, it could be helpful to differentiate between checking values and checking types.
    // If we were to use the (x is y) syntax instead of type check function, then symmetry gets awkward
    // because you could say both `if (x is 10)` and flip it to say `if (10 is x)`
    // but when including type checking in the `is` syntax we can say
    // `if (x is integer)` you could also say `if (integer is x)`, which is semantically separate
    // scope.definitions.insert(format!("is_{}?", struct_name.clone()), );
    let dup_name = struct_name.clone();
    scope.definitions.insert(
        predicate.clone(),
        Box::new(Definition::FunctionDefinition(
            predicate,
            vec!["this".to_string()],
            FunctionObject::new(move |fun_scope: &Scope| {
                match evaluate_identifier_expression(&"this".to_string(), fun_scope)? {
                    Value::Struct(name, _) => Ok(Box::new(Expression::ValueExpr(Value::Boolean(
                        "#".to_string() + &(dup_name == name).to_string(),
                    )))),
                    _ => Err(LelaError::EvaluationError("Expected a Struct!".to_string())),
                }
            }),
        )),
    );

    // Struct Definition
    scope.definitions.insert(
        struct_name.clone(),
        Box::new(Definition::StructDefinition(
            struct_name.clone(),
            fields.clone(),
        )),
    );

    scope.definitions.insert(
        make_struc_name.clone(),
        Box::new(Definition::FunctionDefinition(
            make_struc_name,
            fields.clone(),
            FunctionObject::new(move |fun_scope: &Scope| {
                Ok(Box::new(Expression::ValueExpr(Value::Struct(
                    struct_name.clone(),
                    fields
                        .clone()
                        .iter()
                        .map(|name| {
                            Ok(Box::new(Expression::ValueExpr(
                                (&Box::new(Expression::Identifier(name.clone())))
                                    .evaluate_expression(fun_scope)?,
                            )))
                        })
                        .collect(),
                ))))
            }),
        )),
    );
}

// Defines a function that takes two Values of the same type.
macro_rules! define_uniform_value_function {
    ($func_name:ident, $first_type:pat, $sec_type:pat, $to_do:block) => {
        fn $func_name(val_a: &Value, val_b: &Value) -> Result<Value, LelaError> {
            match (val_a, val_b) {
                ($first_type, $sec_type) => Ok($to_do),
                // TODO: this error message sucks
                (_, _) => Err(LelaError::EvaluationError(format!(
                    "Cannot perform {} on {} and {} types",
                    stringify!($to_do),
                    stringify!($func_name),
                    stringify!($first_type)
                ))),
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

define_uniform_value_function!(less_than_number, Value::Number(a), Value::Number(b), {
    Value::Boolean(
        "#".to_string().add(
            (a.parse::<i32>().unwrap() < b.parse::<i32>().unwrap())
                .to_string()
                .as_str(),
        ),
    )
});

define_uniform_value_function!(greater_than_number, Value::Number(a), Value::Number(b), {
    Value::Boolean(
        "#".to_string().add(
            (a.parse::<i32>().unwrap() > b.parse::<i32>().unwrap())
                .to_string()
                .as_str(),
        ),
    )
});

define_uniform_value_function!(
    less_than_equal_number,
    Value::Number(a),
    Value::Number(b),
    {
        Value::Boolean(
            "#".to_string().add(
                (a.parse::<i32>().unwrap() <= b.parse::<i32>().unwrap())
                    .to_string()
                    .as_str(),
            ),
        )
    }
);

define_uniform_value_function!(
    greater_than_equal_number,
    Value::Number(a),
    Value::Number(b),
    {
        Value::Boolean(
            "#".to_string().add(
                (a.parse::<i32>().unwrap() >= b.parse::<i32>().unwrap())
                    .to_string()
                    .as_str(),
            ),
        )
    }
);

define_uniform_value_function!(and_booleans, Value::Boolean(a), Value::Boolean(b), {
    Value::Boolean(
        "#".to_string().add(
            (parse_boolean(&a).unwrap() && parse_boolean(&b).unwrap())
                .to_string()
                .as_str(),
        ),
    )
});

define_uniform_value_function!(or_booleans, Value::Boolean(a), Value::Boolean(b), {
    Value::Boolean(
        "#".to_string().add(
            (parse_boolean(&a).unwrap() || parse_boolean(&b).unwrap())
                .to_string()
                .as_str(),
        ),
    )
});

define_uniform_value_function!(equals_booleans, Value::Boolean(a), Value::Boolean(b), {
    Value::Boolean(
        "#".to_string().add(
            (parse_boolean(&a).unwrap() == parse_boolean(&b).unwrap())
                .to_string()
                .as_str(),
        ),
    )
});

define_uniform_value_function!(equals_number, Value::Number(a), Value::Number(b), {
    Value::Boolean(
        format!(
            "#{}",
            (a.parse::<i32>().unwrap() == b.parse::<i32>().unwrap())
        )
        .to_string(),
    )
});

pub fn define_function(
    scope: &mut Scope,
    name: &str,
    params: Vec<String>,
    func: FunctionObject,
) -> Option<Box<Definition>> {
    // Given a scope, we want to add a definition with the given name and that evaluates to the given expression
    let def = Definition::FunctionDefinition(name.to_string(), params, func);
    add_to_scope(scope, name.to_owned(), def)
}

// add a single definition to a given scope
pub fn add_to_scope(
    scope: &mut Scope,
    name: String,
    definition: Definition,
) -> Option<Box<Definition>> {
    scope.definitions.insert(name, Box::new(definition))
}

// Given a definition, add it to the given scope
fn create_program_definition(definition: &Definition, scope: &mut Scope) {
    match definition {
        Definition::ConstantDefinition(name, _) => {
            add_to_scope(scope, name.clone(), definition.to_owned());
        }
        Definition::FunctionDefinition(name, _, _) => {
            add_to_scope(scope, name.clone(), definition.to_owned());
        }
        Definition::StructDefinition(name, fields) => {
            define_struct(name.clone(), fields.clone(), scope)
        }
    }
}

// CREATES an empty scope.
pub fn create_empty_scope() -> Scope {
    let global_scope = Scope {
        definitions: HashMap::new(),
    };
    global_scope
}

pub fn create_default_scope() -> Scope {
    let mut scope = create_empty_scope();

    define_function(
        &mut scope,
        "first",
        vec!["list".to_owned()],
        FunctionObject::new(|_| {
            Ok(Box::new(Expression::UnaryOperation(
                UnaryOperator::First,
                Box::new(Expression::Identifier("list".to_string())),
            )))
        }),
    );
    define_function(
        &mut scope,
        "rest",
        vec!["list".to_owned()],
        FunctionObject::new(|_| {
            Ok(Box::new(Expression::UnaryOperation(
                UnaryOperator::Rest,
                Box::new(Expression::Identifier("list".to_string())),
            )))
        }),
    );
    scope
}

// Given a Vec of results between program entries and errors,
// evaluate each element that is an expression
pub fn evaluate_program(
    clean_program: Vec<Result<Box<ProgramEntry>, LelaError>>,
    scope: &mut Scope,
) -> Vec<Result<Value, LelaError>> {
    let mut answers = Vec::new();
    for potential_entry in clean_program.into_iter() {
        match potential_entry {
            Ok(entry) => match entry.as_ref() {
                ProgramEntry::Expression(expression) => {
                    answers.push((&expression).evaluate_expression(scope))
                }
                ProgramEntry::Definition(definition) => {
                    create_program_definition(definition, scope)
                }
                _ => {}
            },
            Err(e) => answers.push(Err(e)),
        }
    }
    answers
}

/// Lots of repeated code in this function: Maybe a macro could fix it??? `register_operation` function?

// Evaluates an operation expression, given the operator, the left expression, the right expression, and a scope.
fn evaluate_operation_expression(
    operator: &Operator,
    left: &Box<Expression>,
    right: &Box<Expression>,
    scope: &Scope,
) -> Result<Value, LelaError> {
    match operator {
        Operator::Add => Ok(add_numbers(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::Multiply => Ok(multiply_numbers(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::Divide => Ok(divide_numbers(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::Subtract => Ok(subtract_numbers(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::And => Ok(and_booleans(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::Or => Ok(or_booleans(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::Equals => {
            let left = left.evaluate_expression(scope)?;
            let right = right.evaluate_expression(scope)?;
            match (&left, &right) {
                (Value::Number(_), Value::Number(_)) => Ok(equals_number(&left, &right)?),
                (Value::Boolean(_), Value::Boolean(_)) => Ok(equals_booleans(&left, &right)?),
                (Value::List(_), Value::List(_)) => todo!(),
                (Value::Empty, Value::Empty) => Ok(Value::Boolean("#true".to_string())),
                (_, _) => Ok(Value::Boolean("#false".to_string())),
            }
        }
        Operator::GreaterThan => Ok(greater_than_number(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::LessThan => Ok(less_than_number(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::GreaterEqTo => Ok(greater_than_equal_number(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
        Operator::LessEqTo => Ok(less_than_equal_number(
            &left.evaluate_expression(scope)?,
            &right.evaluate_expression(scope)?,
        )?),
    }
}

fn evaluate_identifier_expression(
    identifier_name: &String,
    scope: &Scope,
) -> Result<Value, LelaError> {
    match scope.definitions.get(identifier_name.as_str()) {
        Some(def) => match def.as_ref() {
            Definition::ConstantDefinition(_name, expression) => {
                Ok((&expression).evaluate_expression(scope)?)
            }
            Definition::FunctionDefinition(_, _, _) => {
                unreachable!("This identifer is somehow evaluating to a function")
            }
            Definition::StructDefinition(_, _) => {
                unreachable!("This identifer is somehow evaluating to a struct")
            }
        },
        None => Err(LelaError::EvaluationError(format!(
            "Identifier {} not in scope! Here's what was in scope: {:?}",
            identifier_name.as_str(),
            scope.definitions
        ))),
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
        None => Err(LelaError::EvaluationError(format!(
            "Function {} not found",
            identifier
        ))),
    }
}

// Given a function definition, evaluate the function with the given parameters
fn evaluate_function_call_with_definition(
    function_def: &Definition,
    given_parameters: &Vec<Box<Expression>>,
    scope: &Scope,
) -> Result<Value, LelaError> {
    match function_def {
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
                let new_expression = param.evaluate_expression(scope)?;
                function_scope.definitions.insert(
                    new_param_name.clone(),
                    Box::new(Definition::ConstantDefinition(
                        new_param_name.clone(),
                        Box::new(Expression::ValueExpr(new_expression)),
                    )),
                );
            }
            (&expr.call(&function_scope)?).evaluate_expression(&function_scope)
        }
        _ => unreachable!(),
    }
}

// Evaluates a given expression with a given scope
impl Expression {
    pub fn evaluate_expression(&self, scope: &Scope) -> Result<Value, LelaError> {
        match self {
            Expression::ValueExpr(n) => Ok(n.clone()),
            Expression::Operation(op, left, right) => {
                evaluate_operation_expression(&op, left, right, scope)
            }
            Expression::Identifier(name) => Ok(evaluate_identifier_expression(name, scope)?),
            Expression::FunctionCall(identifier, parameters) => {
                evaluate_function_call(identifier, parameters, scope)
            }
            Expression::ConditionalTree(cases, values) => {
                evaluate_conditional(cases, values, scope)
            }
            Expression::UnaryOperation(op, expr) => {
                evaluate_unary_operation(op, &expr.evaluate_expression(scope)?, scope)
            }
        }
    }
}

fn evaluate_unary_operation(
    operation: &UnaryOperator,
    val: &Value,
    scope: &Scope,
) -> Result<Value, LelaError> {
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
        UnaryOperator::First => match val {
            Value::Pair(first, _) => first.evaluate_expression(scope),
            _ => Err(LelaError::EvaluationError(
                "Cannot get the first on a non-pair type.".to_string(),
            )),
        },
        UnaryOperator::Rest => match val {
            Value::Pair(_, rest) => rest.evaluate_expression(scope),
            _ => Err(LelaError::EvaluationError(
                "Cannot get the rest on a non-pair type.".to_string(),
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
            match cond.evaluate_expression(scope) {
                Ok(Value::Boolean(val)) => match parse_boolean(&val) {
                    Ok(b) => {
                        if b {
                            answer = (&values[i]).evaluate_expression(scope);
                            break;
                        }
                    }
                    Err(e) => answer = Err(e),
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
