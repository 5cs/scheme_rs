use std::cmp::Ordering;
use std::collections::HashMap;
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

fn read_from_tokens(tokens: &mut Box<Vec<String>>) -> Result<Box<SyntaxTree>, ()> {
    if tokens.len() == 0 {
        return Err(());
    }
    let token: String = String::from(tokens.get(0).unwrap());
    tokens.drain(0..1);
    // println!("{:?}", tokens);
    if token == "(" {
        let mut l: Box<Vec<Box<SyntaxTree>>> = Box::new(vec![]);
        while tokens.get(0).unwrap() != ")" {
            if let Ok(s) = read_from_tokens(tokens) {
                l.push(s);
            } else {
                panic!();
            }
        }
        tokens.drain(0..1);
        // println!("{:?}", tokens);
        Ok(Box::new(SyntaxTree::List(l)))
    } else if token != ")" {
        // atom type: int, float, str
        if let Ok(v) = token.parse::<i32>() {
            Ok(Box::new(SyntaxTree::Integer(v)))
        } else if let Ok(v) = token.parse::<f32>() {
            Ok(Box::new(SyntaxTree::Float(v)))
        } else {
            Ok(Box::new(SyntaxTree::Symbol(token)))
        }
    } else {
        Err(())
    }
}

// #[derive(Debug)]
enum SyntaxTree {
    Bool(bool),
    Integer(i32),
    Float(f32),
    Symbol(String),
    List(Box<Vec<Box<SyntaxTree>>>),
    BinaryOp(Rc<dyn Fn(SyntaxTree, SyntaxTree) -> SyntaxTree>),
    SyntaxError,
}

fn parse(program: &str) -> Result<Box<SyntaxTree>, ()> {
    read_from_tokens(&mut tokenize(program))
}

impl Copy for SyntaxTree {}

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
                    let mut res: Vec<Box<SyntaxTree>> = vec![];
                    for i in 0..l.len() {
                        let v = *l[i];
                        res.push(Box::new(v));
                    }
                    SyntaxTree::List(Box::new(res))
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
    // abs
    fn abs(&self) -> Self;
}

impl UnaryOps for SyntaxTree {
    fn abs(&self) -> Self {
        match self {
            SyntaxTree::Integer(v) => SyntaxTree::Integer(v.abs()),
            SyntaxTree::Float(v) => SyntaxTree::Float(v.abs()),
            _ => SyntaxTree::SyntaxError,
        }
    }
}

struct Env {
    outer: Option<Box<Env>>,
    binary_ops: HashMap<String, Rc<dyn Fn(SyntaxTree, SyntaxTree) -> SyntaxTree>>,
    unary_ops: HashMap<String, Rc<dyn Fn(SyntaxTree) -> SyntaxTree>>,
}

impl Env {
    fn new() -> Self {
        let mut env = Env {
            outer: None,
            binary_ops: HashMap::new(),
            unary_ops: HashMap::new(),
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
        env.make_unary_op("abs", |x| x.abs());
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

    fn find(&self, x: String) -> Result<Box<SyntaxTree>, ()> {
        if self.binary_ops.contains_key(&x) {
            Ok(Box::new(SyntaxTree::BinaryOp(
                self.binary_ops.get(&x).unwrap().clone(),
            )))
        } else {
            match self.outer {
                Some(ref v) => v.find(x),
                None => Err(()),
            }
        }
    }
}

fn eval(x: &SyntaxTree, env: &mut Env) -> Result<Box<SyntaxTree>, ()> {
    match x {
        SyntaxTree::Symbol(v) => env.find(v.to_owned()),
        SyntaxTree::List(v) => match *v[0] {
            SyntaxTree::Symbol(_) => {
                if let Ok(proc) = eval(&v[0], env) {
                    let val = match *proc {
                        SyntaxTree::BinaryOp(op) => {
                            let arg1 = *eval(&v[1], env)?;
                            let arg2 = *eval(&v[2], env)?;
                            op(arg1, arg2)
                        }
                        _ => SyntaxTree::SyntaxError,
                    };
                    Ok(Box::new(val))
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        },
        SyntaxTree::Integer(v) => Ok(Box::new(SyntaxTree::Integer(*v))),
        SyntaxTree::Float(v) => Ok(Box::new(SyntaxTree::Float(*v))),
        _ => Err(()),
    }
}

fn main() {
    let mut global_env = Env::new();
    global_env.make_binary_op("^", |x, y| x + y);

    let a = SyntaxTree::Integer(5);
    let b = SyntaxTree::Float(2.0);
    if let Ok(res) = global_env.find("*".to_owned()) {
        let val = match *res {
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
            match *val {
                SyntaxTree::Integer(v) => println!("{:?}", v),
                SyntaxTree::Float(v) => println!("{:?}", v),
                _ => println!("{:?}", "err"),
            }
        }
    }
}
