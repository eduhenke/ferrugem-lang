mod lexer;

use lexer::lex;

fn main() {
    let result = lex("def foo (int a) {}");

    for token in result {
        println!("{:?}", token);
    }
}
