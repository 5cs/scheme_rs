use super::ops::{BinaryOp, UnaryOp};
use super::tree::SExpr::{self, *};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Env {
    outer: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, SExpr>,
}

impl Default for Env {
    fn default() -> Env {
        Env {
            outer: None,
            vars: HashMap::new(),
        }
    }
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        let mut env = Env::default();
        env.make_binary_op("+", |x, y| x + y);
        env.make_binary_op("-", |x, y| x - y);
        env.make_binary_op("*", |x, y| x * y);
        env.make_binary_op("/", |x, y| x / y);
        env.make_binary_op(">", |x, y| Bool(x > y));
        env.make_binary_op("<", |x, y| Bool(x < y));
        env.make_binary_op(">=", |x, y| Bool(x >= y));
        env.make_binary_op("<=", |x, y| Bool(x <= y));
        env.make_binary_op("==", |x, y| Bool(x == y));
        env.make_binary_op("!=", |x, y| Bool(x != y));
        env.make_binary_op("equal?", |x, y| Bool(x == y));
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
        Rc::new(RefCell::new(env))
    }

    fn make_binary_op<F: 'static>(&mut self, name: &'static str, func: F)
    where
        F: Fn(&SExpr, &SExpr) -> SExpr,
    {
        self.vars.insert(name.to_owned(), BinaryOp(Rc::new(func)));
    }

    fn make_unary_op<F: 'static>(&mut self, name: &'static str, func: F)
    where
        F: Fn(&SExpr) -> SExpr,
    {
        self.vars.insert(name.to_owned(), UnaryOp(Rc::new(func)));
    }

    fn make_builtin_op<F: 'static>(&mut self, name: &'static str, func: F)
    where
        F: Fn(&SExpr) -> SExpr,
    {
        self.vars.insert(name.to_owned(), BuiltinOp(Rc::new(func)));
    }

    pub fn make_env(outer: Option<Rc<RefCell<Env>>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Env {
            outer,
            ..Default::default()
        }))
    }

    pub fn make_var(&mut self, name: &str, value: &SExpr) {
        if self.vars.contains_key(name) {
            *self.vars.get_mut(name).unwrap() = value.clone();
        } else {
            self.vars.insert(name.to_owned(), value.clone());
        }
    }

    pub fn set_var(&mut self, name: &str, value: &SExpr) {
        if self.vars.contains_key(name) {
            *self.vars.get_mut(name).unwrap() = value.clone();
        } else {
            match self.outer {
                Some(ref v) => v.borrow_mut().set_var(name, value),
                None => self.make_var(name, value),
            }
        }
    }

    pub fn find(&self, x: &str) -> Result<SExpr, ()> {
        if self.vars.contains_key(x) {
            Ok(self.vars.get(x).unwrap().clone())
        } else {
            match self.outer {
                Some(ref v) => v.borrow().find(x),
                None => Ok(Symbol(x.to_owned())),
            }
        }
    }
}
