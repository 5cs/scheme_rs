use std::fmt;
use std::iter::FromIterator;
use std::rc::Rc;

pub enum SyntaxTree {
    Nil,
    Bool(bool),
    Integer(i32),
    Float(f32),
    Symbol(String),
    List(Vec<SyntaxTree>),
    BinaryOp(Rc<dyn Fn(&SyntaxTree, &SyntaxTree) -> SyntaxTree>),
    UnaryOp(Rc<dyn Fn(&SyntaxTree) -> SyntaxTree>),
    BuiltinOp(Rc<dyn Fn(&SyntaxTree) -> SyntaxTree>),
    LambdaOp(Procedure),
    SyntaxError,
}

#[derive(Clone)]
pub struct Procedure {
    pub parms: Box<SyntaxTree>,
    pub body: Box<SyntaxTree>,
}

use SyntaxTree::*;

impl fmt::Debug for SyntaxTree {
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

impl Clone for SyntaxTree {
    fn clone(&self) -> Self {
        match self {
            Bool(v) => Bool(*v),
            Integer(v) => Integer(*v),
            Float(v) => Float(*v),
            Symbol(v) => Symbol(v.to_string()),
            BinaryOp(v) => BinaryOp(v.clone()),
            LambdaOp(v) => LambdaOp(v.clone()),
            BuiltinOp(v) => BuiltinOp(v.clone()),
            List(v) => List(v.clone()), // deep copy?
            _ => SyntaxError,
        }
    }
}

impl FromIterator<SyntaxTree> for SyntaxTree {
    fn from_iter<T: IntoIterator<Item = SyntaxTree>>(iter: T) -> Self {
        let mut v: Vec<SyntaxTree> = vec![];
        for i in iter {
            v.push(i);
        }
        List(v)
    }
}
