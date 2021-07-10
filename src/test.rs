use super::tree::SyntaxTree::*;
use super::*;

#[test]
fn test_binary_op() {
    let mut global_env = Env::new();
    let program = "(+ 1 2)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(3)
    );
}

#[test]
fn test_append_op() {
    let mut global_env = Env::new();
    let program = "(append (quote (1 1)) (quote 1))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        List(vec![Integer(1), Integer(1), Integer(1)])
    );
}

#[test]
fn test_cons() {
    let mut global_env = Env::new();
    let program = "(cons (quote (0 1)) (quote (2)))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        List(vec![Integer(0), Integer(1), Integer(2)])
    );
}

#[test]
fn test_car() {
    let mut global_env = Env::new();
    let program = "(car (quote ((0 1) 2)))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        List(vec![Integer(0), Integer(1)])
    );
}

#[test]
fn test_cdr() {
    let mut global_env = Env::new();
    let program = "(cdr (quote ((0 1) 2)))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        List(vec![Integer(2)])
    );
}

#[test]
fn test_unary_op() {
    let mut global_env = Env::new();
    let program = "(abs (- 10 2))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(8)
    );

    let program = "(list 1 2)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        List(vec![Integer(1), Integer(2),])
    );

    let program = "(null? 1)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(false)
    );

    let program = "(null? (quote ()))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(true)
    );

    let program = "(number? (quote ()))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(false)
    );

    let program = "(number? 1)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(true)
    );

    let program = "(symbol? 'a)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(true)
    );

    let program = "(symbol? 1)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(false)
    );

    let program = "(list? (quote 1))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(true)
    );

    let program = "(list? 'a)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(false)
    );

    let program = "(procedure? +)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Bool(true)
    );
}

#[test]
fn test_if() {
    let mut global_env = Env::new();
    let program = "(if (> (+ 1 1) 1) (+ (- 2 1) (* 2 4)) 5)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(9)
    );
}

#[test]
fn test_define() {
    let mut global_env = Env::new();
    let program = "(define a 1)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Nil
    );

    let program = "a";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(1)
    );
}

#[test]
fn test_lambda_op() {
    let mut global_env = Env::new();
    let program = "(define myabs
                         (lambda (x)
                          (if (< x 0)
                              (- 0 x) x)))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Nil
    );

    let program = "myabs";
    match parse(program) {
        Err(_) => assert!(false, "parse error"),
        Ok(tree) => match eval(&tree, &mut global_env) {
            Err(_) => assert!(false, "eval error"),
            Ok(val) => match val {
                LambdaOp(_) => assert!(true),
                _ => assert!(false, "defined var wrong result"),
            },
        },
    }

    let program = "(myabs -1)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(1)
    );
}

#[test]
fn test_anonymous_lambda() {
    let mut global_env = Env::new();
    let program = "((lambda (x)
                          (if (< x 0)
                              (- 0 x) x)) -1)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(1)
    );
}

#[test]
fn test_sqrt_newton() {
    let mut global_env = Env::new();
    let program = "(define square (lambda (x) (* x x)))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(define average (lambda (x y) (/ (+ x y) 2)))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(define abs (lambda (x) (if (< x 0) (- 0 x) x)))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(define good-enough? (lambda (guess x) (< (abs (- (square guess) x)) 0.001)))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(define improve (lambda (guess x) (average guess (/ x guess))))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(define sqrt-iter (lambda (guess x) (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x))))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(define sqrt (lambda (x) (sqrt-iter 1.0 x)))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    use super::op::UnaryOps;
    for i in 1..1000 {
        let r = eval(&parse(&format!("(sqrt {:?})", i)).unwrap(), &mut global_env).unwrap();
        assert!(
            (&eval(
                &parse(&format!("(square {:?})", r)).unwrap(),
                &mut global_env
            )
            .unwrap()
                - &Float(i as f32))
                .abs()
                < Float(0.001)
        )
    }
}

#[test]
fn test_fibonacci() {
    let mut global_env = Env::new();
    let program = "(define fib
                    (lambda (n)
                     (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(fib 10)";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(55)
    );
}

#[test]
fn test_fibonacci_cps() {
    let mut global_env = Env::new();
    let program = "(define id (lambda (x) x))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    // continuation passing style
    let program = "(define fib-cps
                    (lambda (n k)
                     (if (< n 2)
                      (k n)
                      (k (+ (fib-cps (- n 1) id) (fib-cps (- n 2) id)))))))";
    eval(&parse(program).unwrap(), &mut global_env).unwrap();

    let program = "(fib-cps 10 (lambda (x) x))";
    assert_eq!(
        eval(&parse(program).unwrap(), &mut global_env).unwrap(),
        Integer(55)
    );
}
