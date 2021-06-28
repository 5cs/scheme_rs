use std::fmt;
use std::rc::Rc;

pub enum SyntaxTree {
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

#[derive(Clone)]
pub struct Procedure {
    pub parms: Box<SyntaxTree>,
    pub body: Box<SyntaxTree>,
}

impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxTree::Nil => f.write_str(&format!("nil")),
            SyntaxTree::Bool(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::Integer(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::Float(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::Symbol(v) => f.write_str(&format!("{:?}", v)),
            SyntaxTree::BinaryOp(_) => f.write_str(&format!("BinaryOp")),
            SyntaxTree::UnaryOp(_) => f.write_str(&format!("UnaryOp")),
            SyntaxTree::LambdaOp(_) => f.write_str(&format!("LambdaOp")),
            SyntaxTree::List(v) => {
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
            SyntaxTree::SyntaxError => f.write_str(&format!("{:?}", "SyntaxError")),
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
            SyntaxTree::LambdaOp(v) => SyntaxTree::LambdaOp(v.clone()),
            SyntaxTree::List(v) => SyntaxTree::List(v.clone()), // deep copy?
            _ => SyntaxTree::SyntaxError,
        }
    }
}
