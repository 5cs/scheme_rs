use std::fmt;
use std::iter::FromIterator;
use std::rc::Rc;

pub enum SExpr {
    Nil,
    Bool(bool),
    Integer(i32),
    Float(f32),
    Symbol(String),
    List(Vec<SExpr>),
    BinaryOp(Rc<dyn Fn(&SExpr, &SExpr) -> SExpr>),
    UnaryOp(Rc<dyn Fn(&SExpr) -> SExpr>),
    BuiltinOp(Rc<dyn Fn(&SExpr) -> SExpr>),
    LambdaOp(Procedure),
    SyntaxError,
}

#[derive(Clone)]
pub struct Procedure {
    pub parms: Box<SExpr>,
    pub body: Box<SExpr>,
}

use SExpr::*;

impl fmt::Debug for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Nil => f.write_str(&format!("nil")),
            Bool(v) => f.write_str(&format!("{:?}", v)),
            Integer(v) => f.write_str(&format!("{:?}", v)),
            Float(v) => f.write_str(&format!("{:?}", v)),
            Symbol(v) => f.write_str(&format!("{:?}", v)),
            BinaryOp(_) => f.write_str(&format!("BinaryOp")),
            UnaryOp(_) => f.write_str(&format!("UnaryOp")),
            LambdaOp(_) => f.write_str(&format!("LambdaOp")),
            BuiltinOp(_) => f.write_str(&format!("BuiltinOp")),
            List(v) => {
                let mut s = String::new();
                s.push('(');
                for i in 0..v.len() {
                    s.push_str(&format!("{:?} ", v[i]));
                }
                if v.len() != 0 {
                    s.pop();
                }
                s.push(')');
                f.write_str(&s)
            }
            SyntaxError => f.write_str(&format!("{:?}", "SyntaxError")),
        }
    }
}

impl Clone for SExpr {
    fn clone(&self) -> Self {
        match self {
            Bool(v) => Bool(*v),
            Integer(v) => Integer(*v),
            Float(v) => Float(*v),
            Symbol(v) => Symbol(v.to_string()),
            UnaryOp(v) => UnaryOp(v.clone()),
            BinaryOp(v) => BinaryOp(v.clone()),
            LambdaOp(v) => LambdaOp(v.clone()),
            BuiltinOp(v) => BuiltinOp(v.clone()),
            List(v) => List(v.clone()), // deep copy?
            _ => SyntaxError,
        }
    }
}

impl FromIterator<SExpr> for SExpr {
    fn from_iter<T: IntoIterator<Item = SExpr>>(iter: T) -> Self {
        let mut v: Vec<SExpr> = vec![];
        for i in iter {
            v.push(i);
        }
        List(v)
    }
}
