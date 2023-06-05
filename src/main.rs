mod lexer;
use crate::lexer::{Lexer, TokenType};
use std::env;
use std::fs;
use crate::TokenType::EOF;

fn main() {
    if env::args().nth(1).is_some() {
        let file: String = env::args().nth(1).unwrap();
        let content: String = fs::read_to_string(file).unwrap();

        let mut lexer = Lexer::new(&content);
        loop {
            let token = lexer.next_token();
            if token == Option::from(EOF) {
                println!("{:?}", EOF);
                break
            }
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
