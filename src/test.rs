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
            Ok(val) => assert_eq!(val, SyntaxTree::Integer(3)),
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
            Ok(val) => assert_eq!(
                val,
                SyntaxTree::List(vec![
                    SyntaxTree::Integer(1),
                    SyntaxTree::Integer(1),
                    SyntaxTree::Integer(1)
                ])
            ),
        },
    }
}

#[test]
fn test_cons() {
    let mut global_env = Env::new();
    let program = "(cons (quote (0 1)) (quote (2)))";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => assert_eq!(
                val,
                SyntaxTree::List(vec![
                    SyntaxTree::Integer(0),
                    SyntaxTree::Integer(1),
                    SyntaxTree::Integer(2)
                ])
            ),
        },
    }
}

#[test]
fn test_car() {
    let mut global_env = Env::new();
    let program = "(car (quote ((0 1) 2)))";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => assert_eq!(
                val,
                SyntaxTree::List(vec![SyntaxTree::Integer(0), SyntaxTree::Integer(1)])
            ),
        },
    }
}

#[test]
fn test_cdr() {
    let mut global_env = Env::new();
    let program = "(cdr (quote ((0 1) 2)))";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => assert_eq!(val, SyntaxTree::List(vec![SyntaxTree::Integer(2)])),
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
            Ok(val) => assert_eq!(val, SyntaxTree::Integer(8)),
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
            Ok(val) => assert_eq!(val, SyntaxTree::Integer(9)),
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
            Ok(val) => assert_eq!(val, SyntaxTree::Nil),
        },
    }

    let program = "a";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => assert_eq!(val, SyntaxTree::Integer(1)),
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
            Ok(val) => assert_eq!(val, SyntaxTree::Nil),
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
            Ok(val) => assert_eq!(val, SyntaxTree::Integer(1)),
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
            Ok(val) => assert_eq!(val, SyntaxTree::Integer(1)),
        },
    }
}

#[test]
fn test_sqrt_newton() {
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

#[test]
fn test_fibonacci() {
    let mut global_env = Env::new();
    let program = "(define fib
                    (lambda (n)
                     (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(fib 10)";
    let tree = parse(program).unwrap();
    if let Ok(res) = eval(&tree, &mut global_env) {
        assert_eq!(res, SyntaxTree::Integer(55));
    } else {
        assert!(false, "eval error");
    }
}

#[test]
fn test_fibonacci_cps() {
    let mut global_env = Env::new();
    let program = "(define id (lambda (x) x))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    // continuation passing style
    let program = "(define fib-cps
                    (lambda (n k)
                     (if (< n 2)
                      (k n)
                      (k (+ (fib-cps (- n 1) id) (fib-cps (- n 2) id)))))))";
    let tree = parse(program).unwrap();
    let _ = eval(&tree, &mut global_env);

    let program = "(fib-cps 10 (lambda (x) x))";
    let tree = parse(program).unwrap();
    if let Ok(res) = eval(&tree, &mut global_env) {
        assert_eq!(res, SyntaxTree::Integer(55));
    } else {
        assert!(false, "eval error");
    }
}
