use super::env::Env;
use super::tree::Procedure;
use super::tree::SyntaxTree;

pub fn eval(x: &SyntaxTree, env: &mut Env) -> Result<SyntaxTree, ()> {
    match x {
        SyntaxTree::Bool(v) => Ok(SyntaxTree::Bool(*v)),
        SyntaxTree::Integer(v) => Ok(SyntaxTree::Integer(*v)),
        SyntaxTree::Float(v) => Ok(SyntaxTree::Float(*v)),
        SyntaxTree::Symbol(v) => env.find(v),
        SyntaxTree::List(v) => {
            if v.len() == 0 {
                return Ok(x.clone());
            }

            if let SyntaxTree::Symbol(ref s) = v[0] {
                if s == "if" {
                    let (test, conseq, alt) = (v[1].clone(), v[2].clone(), v[3].clone());
                    let pred = eval(&test, env)?;
                    let exp = match pred {
                        SyntaxTree::Bool(true) => conseq,
                        SyntaxTree::Bool(false) => alt,
                        _ => SyntaxTree::SyntaxError,
                    };
                    return eval(&exp, env);
                }

                if s == "define" {
                    let var = v[1].clone();
                    if let SyntaxTree::Symbol(ref k) = var {
                        let res = eval(&v[2], env)?;
                        env.make_var(k, &res)
                    }
                    return Ok(SyntaxTree::Nil);
                }

                if s == "lambda" {
                    let (parms, body) = (v[1].clone(), v[2].clone());
                    let proc = Procedure {
                        parms: Box::new(parms),
                        body: Box::new(body),
                    };
                    return Ok(SyntaxTree::LambdaOp(proc));
                }

                let proc = eval(&v[0], env)?;
                match proc {
                    SyntaxTree::BinaryOp(op) => {
                        let arg1 = eval(&v[1], env)?;
                        let arg2 = eval(&v[2], env)?;
                        return Ok(op(arg1, arg2));
                    }
                    SyntaxTree::UnaryOp(op) => {
                        let arg = eval(&v[1], env)?;
                        return Ok(op(arg));
                    }
                    SyntaxTree::BuiltinOp(op) => return Ok(op(x.clone())),
                    SyntaxTree::LambdaOp(_) => (), // fallthrough
                    _ => return Ok(SyntaxTree::SyntaxError),
                };
            }

            let proc = eval(&v[0], env)?;
            if let SyntaxTree::LambdaOp(op) = proc {
                // eval args
                let mut args: Vec<SyntaxTree> = vec![];
                for i in 1..v.len() {
                    let arg = eval(&v[i], env)?;
                    args.push(arg);
                }

                // bind args
                let mut local_env = Env::make_env(Box::new(env.clone()));
                if let SyntaxTree::List(p) = *op.parms {
                    for i in 0..args.len() {
                        if let SyntaxTree::Symbol(k) = &p[i] {
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
