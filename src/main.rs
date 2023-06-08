mod generator;
mod lexer;
mod parser;

use crate::generator::CodeGenerator;
use crate::lexer::{Lexer, TokenType};
use crate::parser::{print_program, Parser};
use crate::TokenType::EOF;
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

fn preprocess(file: &Path, include_path: &[&Path]) -> String {
    let mut content = String::new();
    fs::File::open(file)
        .expect("Could not open file")
        .read_to_string(&mut content)
        .expect("Could not read file");

    let mut result = String::new();
    let lines = content.lines();

    for line in lines {
        if line.starts_with("#include \"") {
            let include_file_name = line.split('"').nth(1).expect("No file specified");

            let mut search_paths = vec![file.parent().unwrap()];
            search_paths.extend_from_slice(include_path);

            let include_file_path = find_file(include_file_name, &search_paths);
            let include_file_content = preprocess(&include_file_path, include_path);
            result.push_str(&include_file_content);
        } else if line.starts_with("#include <") {
            let include_file_name = line.split('<').nth(1).unwrap().split('>').nth(0).unwrap();
            let include_file_path = find_file(include_file_name, &[Path::new("/usr/include"), Path::new("/usr/include/linux"), Path::new("/usr/include/c++/13.1.1/tr1"), Path::new("./c++/13.1.1/bits")]);
            let include_file_content = preprocess(&include_file_path, include_path);
            result.push_str(&include_file_content);
        } else {
            result.push_str(line);
            result.push('\n');
        }
    }

    result
}

fn find_file(file_name: &str, paths: &[&Path]) -> PathBuf {
    for path in paths {
        let file_path = path.join(file_name);
        if file_path.exists() {
            return file_path;
        }
    }
    panic!("File not found: {}", file_name);
}

fn main() {
    if let Some(file_name) = env::args().nth(1) {
        let preprocessed_content = preprocess(&Path::new(&file_name), &[Path::new("/usr/include")]);

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
