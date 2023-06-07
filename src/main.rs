mod lexer;
mod parser;
mod generator;

use crate::lexer::{Lexer, TokenType};
use crate::parser::{Parser, print_program};
use crate::TokenType::EOF;
use std::env;
use std::fs;



fn lexer_check(content: String){
    let mut lexer = Lexer::new(&content);

    //Token check for lexer
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
}

fn main() {
    if env::args().nth(1).is_some() {
        let file: String = env::args().nth(1).unwrap();
        let content: String = fs::read_to_string(file).unwrap();
        //lexer_check(content.clone());
        let lexer = Lexer::new(&content);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        println!("{:?}", program);
        print_program(&program);
    } else {
        println!("Error when reading file! / Compiled without file")
    }
    println!("Compiled!");
}
