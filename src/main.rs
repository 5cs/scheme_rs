use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::ops::{Add, Div, Mul, Sub};

use std::rc::Rc;

fn tokenize(s: &str) -> Box<Vec<String>> {
    let tmp = s.replace("(", " ( ").replace(")", " ) ");
    let s = tmp.trim().split(" ");
    let mut v: Vec<String> = vec![];
    for x in s {
        if x == "" {
            continue;
        }
        v.push(x.to_string());
    }
    Box::new(v)
}

fn read_from_tokens(tokens: &mut Box<Vec<String>>) -> Result<SyntaxTree, ()> {
    if tokens.len() == 0 {
        return Err(());
    }
    let token: String = String::from(tokens.get(0).unwrap());
    tokens.drain(0..1);
    if token == "(" {
        let mut l: Vec<SyntaxTree> = vec![];
        while tokens.get(0).unwrap() != ")" {
            if let Ok(s) = read_from_tokens(tokens) {
                l.push(s);
            } else {
                panic!();
            }
        }
        tokens.drain(0..1);
        Ok(SyntaxTree::List(l))
    } else if token != ")" {
        // atom type: int, float, str
        if let Ok(v) = token.parse::<i32>() {
            Ok(SyntaxTree::Integer(v))
        } else if let Ok(v) = token.parse::<f32>() {
            Ok(SyntaxTree::Float(v))
        } else {
            Ok(SyntaxTree::Symbol(token))
        }
    } else {
        Err(())
    }
}

enum SyntaxTree {
    Nil,
    Bool(bool),
    Integer(i32),
    Float(f32),
    Symbol(String),
    List(Vec<SyntaxTree>),
    BinaryOp(Rc<dyn Fn(SyntaxTree, SyntaxTree) -> SyntaxTree>),
    UnaryOp(Rc<dyn Fn(SyntaxTree) -> SyntaxTree>),
    LambdaOp(Procedure),
    SyntaxError,
}

struct Procedure {
    env: Env,
    parms: HashMap<String, Box<SyntaxTree>>,
    body: Box<SyntaxTree>,
}

fn parse(program: &str) -> Result<SyntaxTree, ()> {
    read_from_tokens(&mut tokenize(program))
}

// impl Copy for SyntaxTree {}

impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxTree::Nil => f.write_str(&format!("nil")),
            SyntaxTree::Bool(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::Integer(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::Float(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::Symbol(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::BinaryOp(_) => f.write_str(&format!("bop")),
            SyntaxTree::UnaryOp(_) => f.write_str(&format!("uop")),
            SyntaxTree::List(v) => {
                let mut s = String::new();
                s.push('[');
                for i in 0..v.len() {
                    s.push_str(&format!("{:?} ", v[i]));
                }
                if v.len() != 0 {
                    s.pop();
                }
                s.push(']');
                f.write_str(&s)
            }
            _ => f.write_str(&format!("{:?}", "null")),
        }
    }
}

impl Clone for SyntaxTree {
    fn clone(&self) -> Self {
        match self {
            SyntaxTree::Bool(v) => SyntaxTree::Bool(*v),
            SyntaxTree::Integer(v) => SyntaxTree::Integer(*v),
            SyntaxTree::Float(v) => SyntaxTree::Float(*v),
            SyntaxTree::Symbol(v) => SyntaxTree::Symbol(v.to_string()),
            SyntaxTree::BinaryOp(v) => SyntaxTree::BinaryOp(v.clone()),
            SyntaxTree::List(v) => SyntaxTree::List(v.clone()), // deep copy?
            _ => SyntaxTree::SyntaxError,
        }
    }
}

impl Add for SyntaxTree {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        match self {
            SyntaxTree::Integer(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Integer(l + r),
                SyntaxTree::Float(r) => SyntaxTree::Float((l as f32) + r),
                _ => SyntaxTree::SyntaxError,
            },
            SyntaxTree::Float(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Float(l + (r as f32)),
                SyntaxTree::Float(r) => SyntaxTree::Float(l + r),
                _ => SyntaxTree::SyntaxError,
            },
            _ => SyntaxTree::SyntaxError,
        }
    }
}

impl Sub for SyntaxTree {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        match self {
            SyntaxTree::Integer(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Integer(l - r),
                SyntaxTree::Float(r) => SyntaxTree::Float((l as f32) - r),
                _ => SyntaxTree::SyntaxError,
            },
            SyntaxTree::Float(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Float(l - (r as f32)),
                SyntaxTree::Float(r) => SyntaxTree::Float(l - r),
                _ => SyntaxTree::SyntaxError,
            },
            _ => SyntaxTree::SyntaxError,
        }
    }
}

impl Mul for SyntaxTree {
    type Output = Self;
    fn mul(self, other: Self) -> Self {
        match self {
            SyntaxTree::Integer(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Integer(l * r),
                SyntaxTree::Float(r) => SyntaxTree::Float((l as f32) * r),
                _ => SyntaxTree::SyntaxError,
            },
            SyntaxTree::Float(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Float(l * (r as f32)),
                SyntaxTree::Float(r) => SyntaxTree::Float(l * r),
                _ => SyntaxTree::SyntaxError,
            },
            _ => SyntaxTree::SyntaxError,
        }
    }
}

impl Div for SyntaxTree {
    type Output = Self;
    fn div(self, other: Self) -> Self {
        match self {
            SyntaxTree::Integer(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Integer(l / r),
                SyntaxTree::Float(r) => SyntaxTree::Float((l as f32) / r),
                _ => SyntaxTree::SyntaxError,
            },
            SyntaxTree::Float(l) => match other {
                SyntaxTree::Integer(r) => SyntaxTree::Float(l / (r as f32)),
                SyntaxTree::Float(r) => SyntaxTree::Float(l / r),
                _ => SyntaxTree::SyntaxError,
            },
            _ => SyntaxTree::SyntaxError,
        }
    }
}

// eq, ne
impl PartialEq for SyntaxTree {
    fn eq(&self, other: &SyntaxTree) -> bool {
        match self {
            SyntaxTree::Integer(l) => match other {
                SyntaxTree::Integer(r) => l == r,
                _ => false,
            },
            SyntaxTree::Float(l) => match other {
                SyntaxTree::Float(r) => l == r,
                _ => false,
            },
            SyntaxTree::Symbol(l) => match other {
                SyntaxTree::Symbol(r) => l == r,
                _ => false,
            },
            _ => false,
        }
    }
}

// lt, le, gt, ge
impl PartialOrd for SyntaxTree {
    fn partial_cmp(&self, other: &SyntaxTree) -> Option<Ordering> {
        match self {
            SyntaxTree::Integer(l) => match other {
                SyntaxTree::Integer(r) => l.partial_cmp(r),
                SyntaxTree::Float(r) => (*l as f32).partial_cmp(r),
                _ => None,
            },
            SyntaxTree::Float(l) => match other {
                SyntaxTree::Integer(r) => l.partial_cmp(&(*r as f32)),
                SyntaxTree::Float(r) => l.partial_cmp(r),
                _ => None,
            },
            SyntaxTree::Symbol(l) => match other {
                SyntaxTree::Symbol(r) => l.partial_cmp(r),
                _ => None,
            },
            _ => None,
        }
    }
}

trait BinaryOps<Rhs = Self> {
    fn append(&self, rhs: Rhs) -> Self;
}

impl BinaryOps for SyntaxTree {
    fn append(&self, other: SyntaxTree) -> Self {
        match self {
            SyntaxTree::List(l) => match other {
                SyntaxTree::List(r) => {
                    let mut res: Vec<SyntaxTree> = vec![];
                    for i in 0..l.len() {
                        res.push(l[i].clone());
                    }
                    for i in 0..r.len() {
                        res.push(r[i].clone());
                    }
                    SyntaxTree::List(res)
                }
                _ => SyntaxTree::SyntaxError,
            },
            SyntaxTree::Integer(_) => self.clone() + other,
            SyntaxTree::Float(_) => self.clone() + other,
            _ => SyntaxTree::SyntaxError,
        }
    }
}

trait UnaryOps {
    fn abs(&self) -> Self;
    fn quote(&self) -> Self;
}

impl UnaryOps for SyntaxTree {
    fn abs(&self) -> Self {
        match self {
            SyntaxTree::Integer(v) => SyntaxTree::Integer(v.abs()),
            SyntaxTree::Float(v) => SyntaxTree::Float(v.abs()),
            _ => SyntaxTree::SyntaxError,
        }
    }
    fn quote(&self) -> Self {
        match self {
            SyntaxTree::Integer(v) => {
                let mut l: Vec<SyntaxTree> = vec![];
                l.push(SyntaxTree::Integer(*v));
                SyntaxTree::List(l)
            }
            SyntaxTree::Float(v) => {
                let mut l: Vec<SyntaxTree> = vec![];
                l.push(SyntaxTree::Float(*v));
                SyntaxTree::List(l)
            }
            SyntaxTree::List(v) => SyntaxTree::List(v.clone()),
            _ => SyntaxTree::SyntaxError,
        }
    }
}

struct Env {
    outer: Option<Box<Env>>,
    binary_ops: HashMap<String, Rc<dyn Fn(SyntaxTree, SyntaxTree) -> SyntaxTree>>,
    unary_ops: HashMap<String, Rc<dyn Fn(SyntaxTree) -> SyntaxTree>>,
    variables: HashMap<String, SyntaxTree>,
}

impl Env {
    fn new() -> Self {
        let mut env = Env {
            outer: None,
            binary_ops: HashMap::new(),
            unary_ops: HashMap::new(),
            variables: HashMap::new(),
        };
        env.make_binary_op("+", |x, y| x + y);
        env.make_binary_op("-", |x, y| x - y);
        env.make_binary_op("*", |x, y| x * y);
        env.make_binary_op("/", |x, y| x / y);
        env.make_binary_op(">", |x, y| SyntaxTree::Bool(x > y));
        env.make_binary_op("<", |x, y| SyntaxTree::Bool(x < y));
        env.make_binary_op(">=", |x, y| SyntaxTree::Bool(x >= y));
        env.make_binary_op("<=", |x, y| SyntaxTree::Bool(x <= y));
        env.make_binary_op("==", |x, y| SyntaxTree::Bool(x == y));
        env.make_binary_op("!=", |x, y| SyntaxTree::Bool(x != y));
        env.make_binary_op("append", |x, y| x.append(y));
        env.make_unary_op("abs", |x| x.abs());
        env.make_unary_op("quote", |x| x.quote());
        env.make_unary_op("'", |x| x.quote()); // syntax sugar
        env
    }

    fn make_binary_op<F: 'static>(&mut self, name: &str, func: F)
    where
        F: Fn(SyntaxTree, SyntaxTree) -> SyntaxTree,
    {
        self.binary_ops.insert(name.to_owned(), Rc::new(func));
    }

    fn make_unary_op<F: 'static>(&mut self, name: &str, func: F)
    where
        F: Fn(SyntaxTree) -> SyntaxTree,
    {
        self.unary_ops.insert(name.to_owned(), Rc::new(func));
    }

    fn update(&mut self, map: HashMap<String, Box<SyntaxTree>>) {}

    fn find(&self, x: String) -> Result<SyntaxTree, ()> {
        if self.binary_ops.contains_key(&x) {
            Ok(SyntaxTree::BinaryOp(
                self.binary_ops.get(&x).unwrap().clone(),
            ))
        } else if self.unary_ops.contains_key(&x) {
            Ok(SyntaxTree::UnaryOp(self.unary_ops.get(&x).unwrap().clone()))
        } else if self.variables.contains_key(&x) {
            Ok(self.variables.get(&x).unwrap().clone())
        } else {
            match self.outer {
                Some(ref v) => v.find(x),
                None => Err(()),
            }
        }
    }
}

fn eval(x: &SyntaxTree, env: &mut Env) -> Result<SyntaxTree, ()> {
    match x {
        SyntaxTree::Bool(v) => Ok(SyntaxTree::Bool(*v)),
        SyntaxTree::Integer(v) => Ok(SyntaxTree::Integer(*v)),
        SyntaxTree::Float(v) => Ok(SyntaxTree::Float(*v)),
        SyntaxTree::Symbol(v) => env.find(v.to_owned()),
        SyntaxTree::List(v) => match v[0] {
            SyntaxTree::Symbol(ref s) => {
                if s == "if" {
                    let (test, conseq, alt) = (v[1].clone(), v[2].clone(), v[3].clone());
                    let pred = eval(&test, env)?;
                    let exp = match pred {
                        SyntaxTree::Bool(v) => {
                            if v {
                                conseq
                            } else {
                                alt
                            }
                        }
                        _ => SyntaxTree::SyntaxError,
                    };
                    return eval(&exp, env);
                }

                if s == "define" {
                    let var = v[1].clone();
                    match var {
                        SyntaxTree::Symbol(ref k) => {
                            let res = eval(&v[2], env)?;
                            env.variables.insert(k.to_owned(), res)
                        }
                        _ => None,
                    };
                    return Ok(SyntaxTree::Nil);
                }

                // (define x 10)
                // (define abs
                //  (lambda (x)
                //   (if (< x 0)
                //       (- 0 x) x)))
                // (abs -1)
                // ((lambda (x)
                //          (if (< x 0)
                //              (- 0 x) x))
                //  -1)
                if s == "lambda" {
                    let (parms, body) = (v[1].clone(), v[2].clone());
                    //struct Procedure {
                    //    env: Env,
                    //    parms: HashMap<String, Box<SyntaxTree>>,
                    //    body: Box<SyntaxTree>,
                    //}
                    // let mut proc = Procedure {
                    //     env: *env,
                    //     parms: HashMap::new(),
                    //     body: Box::new(SyntaxTree::List(vec![])),
                    // };
                }

                if let Ok(proc) = eval(&v[0], env) {
                    let val = match proc {
                        SyntaxTree::BinaryOp(op) => {
                            let arg1 = eval(&v[1], env)?;
                            let arg2 = eval(&v[2], env)?;
                            op(arg1, arg2)
                        }
                        SyntaxTree::UnaryOp(op) => op(v[1].clone()),
                        SyntaxTree::LambdaOp(op) => SyntaxTree::SyntaxError,
                        _ => SyntaxTree::SyntaxError,
                    };
                    Ok(val)
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        },
        _ => Err(()),
    }
}

fn main() {
    let mut global_env = Env::new();

    let a = SyntaxTree::Integer(5);
    let b = SyntaxTree::Float(2.0);
    if let Ok(res) = global_env.find("*".to_owned()) {
        let val = match res {
            SyntaxTree::BinaryOp(op) => op(a, b),
            _ => SyntaxTree::SyntaxError,
        };
        match val {
            SyntaxTree::Integer(v) => println!("{:?}", v),
            SyntaxTree::Float(v) => println!("{:?}", v),
            _ => println!("{:?}", "err"),
        };
    }

    let program = "(+ (/ (- 5 2) 3) (* 2 5.1))";
    if let Ok(tree) = parse(program) {
        if let Ok(val) = eval(&tree, &mut global_env) {
            match val {
                SyntaxTree::Integer(v) => println!("{:?}", v),
                SyntaxTree::Float(v) => println!("{:?}", v),
                _ => println!("{:?}", "err"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_binary_op() {
        let mut global_env = Env::new();
        let program = "(+ 1 2)";
        match parse(program) {
            Err(_) => assert!(false, "parse error"),
            Ok(tree) => match eval(&tree, &mut global_env) {
                Err(_) => assert!(false, "eval error"),
                Ok(val) => match val {
                    SyntaxTree::Integer(v) => assert!(v == 3),
                    _ => assert!(false, "binary op wrong result"),
                },
            },
        }
    }

    #[test]
    fn test_append_op() {
        let mut global_env = Env::new();

        let program = "(append (quote (1 1)) (quote 1))";
        match parse(program) {
            Err(_) => assert!(false, "parse error"),
            Ok(tree) => match eval(&tree, &mut global_env) {
                Err(_) => assert!(false, "eval error"),
                Ok(val) => match val {
                    SyntaxTree::List(v) => {
                        assert!(v.len() == 3);
                        for i in 0..v.len() {
                            match v[i] {
                                SyntaxTree::Integer(k) => assert!(k == 1),
                                _ => assert!(false, "type error"),
                            }
                        }
                    }
                    _ => assert!(false, "append op wrong result"),
                },
            },
        }
    }

    #[test]
    fn test_unary_op() {
        let mut global_env = Env::new();
        let program = "(> 2 1)";
        match parse(program) {
            Err(_) => assert!(false, "parse error"),
            Ok(tree) => match eval(&tree, &mut global_env) {
                Err(_) => assert!(false, "eval error"),
                Ok(val) => match val {
                    SyntaxTree::Bool(v) => assert!(v == true),
                    _ => assert!(false, "unary op wrong result"),
                },
            },
        }
    }

    #[test]
    fn test_if() {
        let mut global_env = Env::new();
        let program = "(if (> (+ 1 1) 1) (+ (- 2 1) (* 2 4)) 5)";
        match parse(program) {
            Err(_) => assert!(false, "parse error"),
            Ok(tree) => match eval(&tree, &mut global_env) {
                Err(_) => assert!(false, "eval error"),
                Ok(val) => match val {
                    SyntaxTree::Integer(v) => assert!(v == 9),
                    _ => assert!(false, "if error"),
                },
            },
        }
    }

    #[test]
    fn test_define() {
        let mut global_env = Env::new();
        let program = "(define a 1)";
        match parse(program) {
            Err(_) => assert!(false, "parse error"),
            Ok(tree) => match eval(&tree, &mut global_env) {
                Err(_) => assert!(false, "eval error"),
                Ok(val) => match val {
                    SyntaxTree::Nil => assert!(true),
                    _ => assert!(false, "define error"),
                },
            },
        }

        let program = "a";
        match parse(program) {
            Err(_) => assert!(false, "parse error"),
            Ok(tree) => match eval(&tree, &mut global_env) {
                Err(_) => assert!(false, "eval error"),
                Ok(val) => match val {
                    SyntaxTree::Integer(v) => assert!(v == 1),
                    _ => assert!(false, "defined var wrong result"),
                },
            },
        }
    }
}
