use super::tree::SyntaxTree::{self, *};
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Sub};

pub trait BinaryOps<Rhs = SyntaxTree> {
    fn append(&self, rhs: &Rhs) -> Self;
    fn cons(&self, rhs: &Rhs) -> Self;
}

pub trait UnaryOps {
    fn abs(&self) -> Self;
    fn quote(&self) -> Self;
    fn car(&self) -> Self;
    fn cdr(&self) -> Self;
    fn list(&self) -> Self;
    fn is_null(&self) -> Self;
    fn is_number(&self) -> Self;
    fn is_symbol(&self) -> Self;
    fn is_list(&self) -> Self;
    fn is_procedure(&self) -> Self;
}

macro_rules! arith_op {
    ($bound:ident, $func:ident, $op:tt) => {
        impl $bound for &SyntaxTree {
            type Output = SyntaxTree;
            fn $func(self, other: Self) -> Self::Output {
                match (self, other) {
                    (Integer(l), Integer(r)) => Integer(l $op r),
                    (Float(l), Float(r)) => Float(l $op r),
                    (Integer(l), Float(r)) => {
                        Float((*l as f32) $op r)
                    }
                    (Float(l), Integer(r)) => {
                        Float(l $op (*r as f32))
                    }
                    _ => SyntaxError,
                }
            }
        }
    };
}

arith_op!(Add, add, +);
arith_op!(Sub, sub, -);
arith_op!(Mul, mul, *);
arith_op!(Div, div, /);

// eq, ne
impl PartialEq for SyntaxTree {
    fn eq(&self, other: &SyntaxTree) -> bool {
        match (self, other) {
            (Nil, Nil) => true,
            (Bool(l), Bool(r)) => l == r,
            (Integer(l), Integer(r)) => l == r,
            (Float(l), Float(r)) => l == r,
            (Symbol(l), Symbol(r)) => l == r,
            (List(l), List(r)) => l.len() == r.len() && l.iter().zip(r).all(|(x, y)| x == y),
            _ => false,
        }
    }
}

// lt, le, gt, ge
impl PartialOrd for SyntaxTree {
    fn partial_cmp(&self, other: &SyntaxTree) -> Option<Ordering> {
        match (self, other) {
            (Bool(l), Bool(r)) => l.partial_cmp(r),
            (Integer(l), Integer(r)) => l.partial_cmp(r),
            (Integer(l), Float(r)) => (*l as f32).partial_cmp(r),
            (Float(l), Integer(r)) => l.partial_cmp(&(*r as f32)),
            (Float(l), Float(r)) => l.partial_cmp(r),
            (Symbol(l), Symbol(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}

impl BinaryOps for SyntaxTree {
    fn append(&self, other: &SyntaxTree) -> Self {
        match (self, other) {
            (List(l), List(ref r)) => l.iter().cloned().chain(r.iter().cloned()).collect(),
            (Integer(_), r) | (Float(_), r) => self + &r,
            _ => SyntaxError,
        }
    }

    fn cons(&self, other: &SyntaxTree) -> Self {
        match (self, other) {
            (List(l), List(ref r)) => l.iter().cloned().chain(r.iter().cloned()).collect(),
            (Integer(_), List(r)) | (Float(_), List(r)) => [self.clone()]
                .to_vec()
                .iter()
                .cloned()
                .chain(r.iter().cloned())
                .collect(),
            _ => SyntaxError,
        }
    }
}

impl UnaryOps for SyntaxTree {
    fn abs(&self) -> Self {
        match self {
            Integer(v) => Integer(v.abs()),
            Float(v) => Float(v.abs()),
            _ => SyntaxError,
        }
    }

    fn quote(&self) -> Self {
        match self {
            Integer(v) => List(vec![Integer(*v)]),
            Float(v) => List(vec![Float(*v)]),
            List(v) => List(v.clone()),
            _ => SyntaxError,
        }
    }

    fn car(&self) -> Self {
        match self {
            List(v) => v[0].clone(),
            _ => SyntaxError,
        }
    }

    fn cdr(&self) -> Self {
        match self {
            List(v) => List(v[1..].to_vec()),
            _ => SyntaxError,
        }
    }

    fn list(&self) -> Self {
        match self {
            List(v) => List(v[1..].to_vec()),
            _ => SyntaxError,
        }
    }

    fn is_null(&self) -> Self {
        match self {
            List(v) => Bool(v.len() == 0),
            _ => Bool(false),
        }
    }

    fn is_number(&self) -> Self {
        match self {
            Integer(_) | Float(_) => Bool(true),
            _ => Bool(false),
        }
    }

    fn is_symbol(&self) -> Self {
        match self {
            Symbol(_) => Bool(true),
            _ => Bool(false),
        }
    }

    fn is_list(&self) -> Self {
        match self {
            List(_) => Bool(true),
            _ => Bool(false),
        }
    }

    fn is_procedure(&self) -> Self {
        match self {
            BuiltinOp(_) | UnaryOp(_) | BinaryOp(_) | LambdaOp(_) => Bool(true),
            _ => Bool(false),
        }
    }
}
