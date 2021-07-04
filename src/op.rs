use super::tree::SyntaxTree;
use std::cmp::Ordering;
use std::ops::{Add, Div, Mul, Sub};

pub trait BinaryOps<Rhs = Self> {
    fn append(&self, rhs: Rhs) -> Self;
    fn cons(&self, rhs: Rhs) -> Self;
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
            SyntaxTree::Nil => match other {
                SyntaxTree::Nil => true,
                _ => false,
            },
            SyntaxTree::Bool(l) => match other {
                SyntaxTree::Bool(r) => l == r,
                _ => false,
            },
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
            SyntaxTree::List(l) => match other {
                SyntaxTree::List(r) => l.len() == r.len() && l.iter().zip(r).all(|(x, y)| x == y),
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
                SyntaxTree::List(r) => l.iter().cloned().chain(r.iter().cloned()).collect(),
                _ => SyntaxTree::SyntaxError,
            },
            SyntaxTree::Integer(_) => self.clone() + other,
            SyntaxTree::Float(_) => self.clone() + other,
            _ => SyntaxTree::SyntaxError,
        }
    }

    fn cons(&self, other: SyntaxTree) -> Self {
        match other {
            SyntaxTree::List(r) => match self {
                SyntaxTree::List(l) => l.iter().cloned().chain(r.iter().cloned()).collect(),
                SyntaxTree::Integer(_) | SyntaxTree::Float(_) => [self.clone()]
                    .to_vec()
                    .iter()
                    .cloned()
                    .chain(r.iter().cloned())
                    .collect(),
                _ => SyntaxTree::SyntaxError,
            },
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

    fn car(&self) -> Self {
        match self {
            SyntaxTree::List(v) => v[0].clone(),
            _ => SyntaxTree::SyntaxError,
        }
    }

    fn cdr(&self) -> Self {
        match self {
            SyntaxTree::List(v) => SyntaxTree::List(v[1..].to_vec()),
            _ => SyntaxTree::SyntaxError,
        }
    }

    fn list(&self) -> Self {
        match self {
            SyntaxTree::List(v) => SyntaxTree::List(v[1..].to_vec()),
            _ => SyntaxTree::SyntaxError,
        }
    }

    fn is_null(&self) -> Self {
        match self {
            SyntaxTree::List(v) => SyntaxTree::Bool(v.len() == 0),
            _ => SyntaxTree::Bool(false),
        }
    }

    fn is_number(&self) -> Self {
        match self {
            SyntaxTree::Integer(_) | SyntaxTree::Float(_) => SyntaxTree::Bool(true),
            _ => SyntaxTree::Bool(false),
        }
    }

    fn is_symbol(&self) -> Self {
        match self {
            SyntaxTree::Symbol(_) => SyntaxTree::Bool(true),
            _ => SyntaxTree::Bool(false),
        }
    }

    fn is_list(&self) -> Self {
        match self {
            SyntaxTree::List(_) => SyntaxTree::Bool(true),
            _ => SyntaxTree::Bool(false),
        }
    }

    fn is_procedure(&self) -> Self {
        match self {
            SyntaxTree::BuiltinOp(_)
            | SyntaxTree::UnaryOp(_)
            | SyntaxTree::BinaryOp(_)
            | SyntaxTree::LambdaOp(_) => SyntaxTree::Bool(true),
            _ => SyntaxTree::Bool(false),
        }
    }
}
