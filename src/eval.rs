use super::env::Env;
use super::tree::Procedure;
use super::tree::SyntaxTree::{self, *};

pub fn eval(x: &SyntaxTree, env: &mut Env) -> Result<SyntaxTree, ()> {
    match x {
        Bool(v) => Ok(Bool(*v)),
        Integer(v) => Ok(Integer(*v)),
        Float(v) => Ok(Float(*v)),
        Symbol(v) => env.find(v),
        List(v) => {
            if v.len() == 0 {
                return Ok(x.clone());
            }

            if let Symbol(ref s) = v[0] {
                if s == "if" {
                    let (test, conseq, alt) = (v[1].clone(), v[2].clone(), v[3].clone());
                    let pred = eval(&test, env)?;
                    let exp = match pred {
                        Bool(true) => conseq,
                        Bool(false) => alt,
                        _ => SyntaxError,
                    };
                    return eval(&exp, env);
                }

                if s == "define" {
                    let var = v[1].clone();
                    if let Symbol(ref k) = var {
                        let res = eval(&v[2], env)?;
                        env.make_var(k, &res)
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
                let mut local_env = Env::make_env(Some(env));
                if let List(p) = *op.parms {
                    for i in 0..args.len() {
                        if let Symbol(k) = &p[i] {
                            local_env.make_var(k, &args[i]);
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
