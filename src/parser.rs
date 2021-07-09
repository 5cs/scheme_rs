use super::tree::SyntaxTree;

fn tokenize(program: &str) -> Vec<String> {
    let tmp = program
        .replace("(", " ( ")
        .replace(")", " ) ")
        .replace("\n", " ")
        .replace("\t", " ");
    let stream = tmp.trim().split(" ");

    stream
        .into_iter()
        .filter(|token| token != &"")
        .map(|token| token.to_string())
        .collect()
}

fn read_from_tokens(tokens: &mut Vec<String>) -> Result<SyntaxTree, ()> {
    if tokens.len() == 0 {
        return Err(());
    }
    let token: String = String::from(tokens.get(0).unwrap());
    tokens.drain(0..1);
    if token == "(" {
        let mut l: Vec<SyntaxTree> = vec![];
        while tokens.get(0).unwrap() != ")" {
            if let Ok(s) = read_from_tokens(tokens) {
                l.push(s);
            } else {
                panic!();
            }
        }
        tokens.drain(0..1);
        Ok(SyntaxTree::List(l))
    } else if token != ")" {
        // Atom type: int, float, str
        if let Ok(v) = token.parse::<i32>() {
            Ok(SyntaxTree::Integer(v))
        } else if let Ok(v) = token.parse::<f32>() {
            Ok(SyntaxTree::Float(v))
        } else {
            Ok(SyntaxTree::Symbol(token))
        }
    } else {
        Err(())
    }
}

pub fn parse(program: &str) -> Result<SyntaxTree, ()> {
    read_from_tokens(&mut tokenize(program))
}
