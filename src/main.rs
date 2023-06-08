mod generator;
mod lexer;
mod parser;
mod preprocessor;

use crate::generator::CodeGenerator;
use crate::lexer::{Lexer, TokenType};
use crate::parser::{print_program, Parser};
use crate::TokenType::EOF;
use std::env;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::collections::HashSet;

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
    if let Some(file_name) = env::args().nth(1) {
        let file_path = Path::new(&file_name);
        let include_path = vec![
            Path::new("/usr/include"),
            Path::new("/usr/include/linux"),
            Path::new("/usr/include/c++/13.1.1"),
            Path::new("/usr/include/c++/13.1.1/bits"),
            Path::new("/usr/include/c++/13.1.1/tr1"),
            Path::new("/usr/include/c++/13.1.1/x86_64-pc-linux-gnu")
        ];
        let mut processed_files = HashSet::new();
        let preprocessed_content = preprocessor::preprocess(&file_path, &include_path, &mut processed_files);

        let lexer = Lexer::new(&preprocessed_content);
        lexer_check(preprocessed_content.clone());

        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut code_generator = CodeGenerator::new();
        let assembly = code_generator.generate(&program);

        let output_file_name = "testcode/output.s";
        fs::write(output_file_name, assembly).expect("Unable to write to file");
        //println!("{:?}", program);
        //print_program(&program);
        //println!("{}", assembly);
    } else {
        println!("Error when reading file!")
    }
    println!("Compiled!");
}
