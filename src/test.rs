use super::tree::SyntaxTree;
use super::*;

#[test]
fn test_binary_op() {
    let mut global_env = Env::new();
    let program = "(+ 1 2)";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Integer(v) => assert!(v == 3),
                _ => assert!(false, "binary op wrong result"),
            },
        },
    }
}

#[test]
fn test_append_op() {
    let mut global_env = Env::new();

    let program = "(append (quote (1 1)) (quote 1))";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::List(v) => {
                    assert!(v.len() == 3);
                    for i in 0..v.len() {
                        match v[i] {
                            SyntaxTree::Integer(k) => assert!(k == 1),
                            _ => assert!(false, "type error"),
                        }
                    }
                }
                _ => assert!(false, "append op wrong result"),
            },
        },
    }
}

#[test]
fn test_unary_op() {
    let mut global_env = Env::new();
    let program = "(abs (- 10 2))";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Integer(v) => assert!(v == 8),
                _ => assert!(false, "unary op wrong result"),
            },
        },
    }
}

#[test]
fn test_if() {
    let mut global_env = Env::new();
    let program = "(if (> (+ 1 1) 1) (+ (- 2 1) (* 2 4)) 5)";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Integer(v) => assert!(v == 9),
                _ => assert!(false, "if error"),
            },
        },
    }
}

#[test]
fn test_define() {
    let mut global_env = Env::new();
    let program = "(define a 1)";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Nil => assert!(true),
                _ => assert!(false, "define error"),
            },
        },
    }

    let program = "a";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Integer(v) => assert!(v == 1),
                _ => assert!(false, "defined var wrong result"),
            },
        },
    }
}

#[test]
fn test_lambda_op() {
    let mut global_env = Env::new();
    let program = "(define myabs
                         (lambda (x)
                          (if (< x 0)
                              (- 0 x) x)))";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Nil => assert!(true),
                _ => assert!(false, "defined error"),
            },
        },
    }

    let program = "myabs";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::LambdaOp(_) => assert!(true),
                _ => assert!(false, "defined var wrong result"),
            },
        },
    }

    let program = "(myabs -1)";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Integer(v) => assert!(v == 1),
                _ => assert!(false, "defined var wrong result"),
            },
        },
    }
}

#[test]
fn test_anonymous_lambda() {
    let mut global_env = Env::new();
    let program = "((lambda (x)
                          (if (< x 0)
                              (- 0 x) x)) -1)";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                SyntaxTree::Integer(v) => assert!(v == 1),
                _ => assert!(false, "defined error"),
            },
        },
    }
}

#[test]
fn test_numeric() {
    let mut global_env = Env::new();
    let program = "(define square (lambda (x) (* x x)))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(define average (lambda (x y) (/ (+ x y) 2)))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(define abs (lambda (x) (if (< x 0) (- 0 x) x)))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(define good-enough? (lambda (guess x) (< (abs (- (square guess) x)) 0.001)))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(define improve (lambda (guess x) (average guess (/ x guess))))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(define sqrt-iter (lambda (guess x) (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(define sqrt (lambda (x) (sqrt-iter 1.0 x)))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    for i in 1..1000 {
        let program = &format!("(sqrt {:?})", i);
        let tree = parse(program).unwrap();
        if let Ok(res) = eval(&tree, &mut global_env) {
            match res {
                SyntaxTree::Float(v) => {
                    assert!((v * v - (i as f32)).abs() < 0.001);
                }
                _ => assert!(false),
            }
        }
    }
}
