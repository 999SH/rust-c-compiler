mod lexer;
use crate::lexer::Lexer;
use std::env;
use std::fs;

fn main() {
    if env::args().nth(1).is_some() {
        let file: String = env::args().nth(1).unwrap();
        let content: String = fs::read_to_string(file).unwrap();

        let mut lexer = Lexer::new(&content);
        loop {
            let token = lexer.next_token();
            match token {
                Some(token) => println!("{:?}", token),
                None => break,
            }
        }
    } else {
        println!("Error when reading file! / Compiled without file")
    }
    println!("Compiled!");
}
