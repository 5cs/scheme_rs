use super::env::Env;
use super::tree::Procedure;
use super::tree::SyntaxTree::{self, *};
use std::cell::RefCell;
use std::rc::Rc;

pub fn eval(x: &SyntaxTree, env: &mut Rc<RefCell<Env>>) -> Result<SyntaxTree, ()> {
    match x {
        Bool(v) => Ok(Bool(*v)),
        Integer(v) => Ok(Integer(*v)),
        Float(v) => Ok(Float(*v)),
        Symbol(v) => env.borrow().find(v),
        List(v) => {
            if v.len() == 0 {
                return Ok(x.clone());
            }

            if let Symbol(ref s) = v[0] {
                if s == "if" {
                    let (test, conseq, alt) = (&v[1], &v[2], &v[3]);
                    let pred = eval(&test, env)?;
                    let res = match pred {
                        Bool(true) => eval(conseq, env)?,
                        Bool(false) => eval(alt, env)?,
                        _ => SyntaxError,
                    };
                    return Ok(res);
                }

                if s == "define" {
                    if let Symbol(ref k) = v[1] {
                        let res = eval(&v[2], env)?;
                        env.borrow_mut().make_var(k, &res);
                    }
                    return Ok(Nil);
                }

                if s == "set!" {
                    if let Symbol(ref k) = v[1] {
                        let res = eval(&v[2], env)?;
                        env.borrow_mut().set_var(k, &res);
                    }
                    return Ok(Nil);
                }

                if s == "lambda" {
                    let (parms, body) = (v[1].clone(), v[2].clone());
                    let proc = Procedure {
                        parms: Box::new(parms),
                        body: Box::new(body),
                    };
                    return Ok(LambdaOp(proc));
                }

                let proc = eval(&v[0], env)?;
                match proc {
                    BinaryOp(op) => {
                        let arg1 = eval(&v[1], env)?;
                        let arg2 = eval(&v[2], env)?;
                        return Ok(op(&arg1, &arg2));
                    }
                    UnaryOp(op) => {
                        let arg = eval(&v[1], env)?;
                        return Ok(op(&arg));
                    }
                    BuiltinOp(op) => return Ok(op(x)),
                    LambdaOp(_) => (), // fallthrough
                    _ => return Ok(SyntaxError),
                };
            }

            let proc = eval(&v[0], env)?;
            if let LambdaOp(op) = proc {
                // eval args
                let mut args: Vec<SyntaxTree> = vec![];
                for i in 1..v.len() {
                    let arg = eval(&v[i], env)?;
                    args.push(arg);
                }

                // bind args
                let mut local_env = Env::make_env(Some(env.clone()));
                if let List(ref p) = *op.parms {
                    for i in 0..args.len() {
                        if let Symbol(k) = &p[i] {
                            local_env.borrow_mut().make_var(k, &args[i]);
                        }
                    }
                }
                return eval(&op.body, &mut local_env);
            }
            return Ok(x.clone());
        }
        _ => Err(()),
    }
}
