mod env;
mod eval;
mod ops;
mod parser;
mod tree;

use env::Env;
use eval::*;
use parser::*;

fn main() {
    let mut global_env = Env::new();
    let program = "(+ (/ (- 5 2) 3) (* 2 5.1))";
    match parse(program) {
        Ok(tree) => match eval(&tree, &mut global_env) {
            Ok(val) => println!("{:?}", val),
            Err(_) => println!("eval error"),
        },
        Err(_) => println!("parse error"),
    }
}

#[cfg(test)]
mod test;
