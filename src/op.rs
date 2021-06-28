use super::tree::SyntaxTree;
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Sub};

pub trait BinaryOps<Rhs = Self> {
    fn append(&self, rhs: Rhs) -> Self;
}

pub trait UnaryOps {
    fn abs(&self) -> Self;
    fn quote(&self) -> Self;
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
