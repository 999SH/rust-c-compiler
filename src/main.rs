mod generator;
mod lexer;
mod parser;

use crate::generator::CodeGenerator;
use crate::lexer::{Lexer, TokenType};
use crate::parser::{Parser};
use crate::TokenType::EOF;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};

#[allow(dead_code)]
fn lexer_check(content: String) {
    let mut lexer = Lexer::new(&content);

    //Token check for lexer
    loop {
        let token = lexer.next_token();
        if token == Option::from(EOF) {
            println!("{:?}", EOF);
            break;
        }
        match token {
            Some(token) => println!("{:?}", token),
            None => break,
        }
    }
}

fn main() {
    if let Some(file) = env::args().nth(1) {
        let preprocessed_content: String = fs::read_to_string(file).unwrap();
        let lexer = Lexer::new(&preprocessed_content);
        lexer_check(preprocessed_content.clone());

        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut code_generator = CodeGenerator::new();
        let assembly = code_generator.generate(&program);

        let output_file_name = "testcode/output.asm";
        fs::write(output_file_name, assembly).expect("Unable to write to file");
        //println!("{:?}", program);
        //print_program(&program);
        //println!("{}", assembly);
    } else {
        println!("Error when reading file!")
    }
    println!("Compiled!");
}
