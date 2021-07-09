use super::op::{BinaryOps, UnaryOps};
use super::tree::SyntaxTree;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Env {
    outer: Option<Box<Env>>,
    binary_ops: HashMap<&'static str, Rc<dyn Fn(SyntaxTree, SyntaxTree) -> SyntaxTree>>,
    unary_ops: HashMap<&'static str, Rc<dyn Fn(SyntaxTree) -> SyntaxTree>>,
    builtin_ops: HashMap<&'static str, Rc<dyn Fn(SyntaxTree) -> SyntaxTree>>,
    vars: HashMap<String, SyntaxTree>,
}

impl Default for Env {
    fn default() -> Env {
        Env {
            outer: None,
            binary_ops: HashMap::new(),
            unary_ops: HashMap::new(),
            builtin_ops: HashMap::new(),
            vars: HashMap::new(),
        }
    }
}

impl Env {
    pub fn new() -> Self {
        let mut env = Env::default();
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
        env.make_binary_op("equal?", |x, y| SyntaxTree::Bool(x == y));
        env.make_binary_op("append", |x, y| x.append(y));
        env.make_binary_op("cons", |x, y| x.cons(y));
        env.make_unary_op("abs", |x| x.abs());
        env.make_unary_op("quote", |x| x.quote());
        env.make_unary_op("'", |x| x.quote()); // syntax sugar
        env.make_unary_op("car", |x| x.car());
        env.make_unary_op("cdr", |x| x.cdr());
        env.make_unary_op("null?", |x| x.is_null());
        env.make_unary_op("number?", |x| x.is_number());
        env.make_unary_op("symbol?", |x| x.is_symbol());
        env.make_unary_op("list?", |x| x.is_list());
        env.make_unary_op("procedure?", |x| x.is_procedure());
        env.make_builtin_op("list", |x| x.list());
        env
    }

    fn make_binary_op<F: 'static>(&mut self, name: &'static str, func: F)
    where
        F: Fn(SyntaxTree, SyntaxTree) -> SyntaxTree,
    {
        self.binary_ops.insert(name, Rc::new(func));
    }

    fn make_unary_op<F: 'static>(&mut self, name: &'static str, func: F)
    where
        F: Fn(SyntaxTree) -> SyntaxTree,
    {
        self.unary_ops.insert(name, Rc::new(func));
    }

    fn make_builtin_op<F: 'static>(&mut self, name: &'static str, func: F)
    where
        F: Fn(SyntaxTree) -> SyntaxTree,
    {
        self.builtin_ops.insert(name, Rc::new(func));
    }

    pub fn make_env(outer: Box<Env>) -> Self {
        Env {
            outer: Some(outer),
            ..Default::default()
        }
    }

    pub fn make_var(&mut self, name: &str, value: &SyntaxTree) {
        self.vars.insert(name.to_owned(), value.clone());
    }

    pub fn find(&self, x: &str) -> Result<SyntaxTree, ()> {
        if self.vars.contains_key(x) {
            Ok(self.vars.get(x).unwrap().clone())
        } else if self.binary_ops.contains_key(x) {
            Ok(SyntaxTree::BinaryOp(
                self.binary_ops.get(x).unwrap().clone(),
            ))
        } else if self.unary_ops.contains_key(x) {
            Ok(SyntaxTree::UnaryOp(self.unary_ops.get(x).unwrap().clone()))
        } else if self.builtin_ops.contains_key(x) {
            Ok(SyntaxTree::BuiltinOp(
                self.builtin_ops.get(x).unwrap().clone(),
            ))
        } else {
            match self.outer {
                Some(ref v) => v.find(x),
                None => Ok(SyntaxTree::Symbol(x.to_owned())),
            }
        }
    }
}
