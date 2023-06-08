use crate::lexer::TokenType;
use crate::Lexer;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Variable(String),
    FunctionCall(String, Vec<Expr>),
    Negation(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expression(Expr),
    Return(Expr),
    FunctionDeclaration(String, Vec<Parameter>, Vec<Statement>),
    VariableDeclaration(String, String, Option<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub typ: String,
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Option<TokenType>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parser = Parser {
            lexer,
            cur_token: None,
        };
        parser.next_token();
        parser
    }
    pub fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while let Some(token) = &self.cur_token {
            match token {
                TokenType::EOF => break,
                _ => {
                    let statement = self.parse_statement();
                    program.statements.push(statement)
                }
            }
        }
        program
    }

    pub fn parse_expr(&mut self) -> Expr {
        let expr = match &self.cur_token {
            Some(TokenType::IntConst(int)) => {
                let int_value = *int;
                self.next_token();
                Expr::Int(int_value)
            }
            Some(TokenType::Identifier(id)) => {
                let var_id = id.to_string();
                self.next_token();
                match &self.cur_token {
                    Some(TokenType::OpenParen) => {
                        self.next_token();
                        let args = self.parse_args();
                        match &self.cur_token {
                            Some(TokenType::CloseParen) => {
                                self.next_token();
                            }
                            _ => {
                                panic!("Expected closing parenthesis after function call arguments")
                            }
                        }
                        Expr::FunctionCall(var_id, args)
                    }
                    _ => Expr::Variable(var_id),
                }
            }
            Some(TokenType::Minus) => {
                self.next_token();
                let expr = Box::new(self.parse_expr());
                Expr::Negation(expr)
            }
            _ => panic!("Need to add this expression {:?}", self.cur_token),
        };

        match &self.cur_token {
            Some(TokenType::Plus) => {
                self.next_token();
                let right = self.parse_expr();
                Expr::BinOp(Box::new(expr), Op::Plus, Box::new(right))
            }
            Some(TokenType::Minus) => {
                self.next_token();
                let right = self.parse_expr();
                Expr::BinOp(Box::new(expr), Op::Minus, Box::new(right))
            }
            _ => expr,
        }
    }
    fn parse_args(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();

        loop {
            match &self.cur_token {
                Some(TokenType::CloseParen) => {
                    break;
                }
                Some(TokenType::Comma) => {
                    self.next_token();
                }
                _ => {
                    let arg = self.parse_expr();
                    args.push(arg);
                }
            }
        }
        args
    }
    fn parse_parameters(&mut self) -> Vec<Parameter> {
        let mut parameters = Vec::new();

        loop {
            self.next_token();

            match &self.cur_token {
                Some(TokenType::CloseParen) => {
                    self.next_token();
                    break;
                }
                Some(TokenType::Int) => {
                    self.next_token();
                    match &self.cur_token {
                        Some(TokenType::Identifier(id)) => {
                            parameters.push(Parameter {
                                name: id.to_string(),
                                typ: "int".to_string(),
                            });
                        }
                        _ => panic!("Expected identifier, Cur token: {:?}", self.cur_token),
                    }
                }
                Some(TokenType::Comma) => {}
                _ => panic!("Unexpected function type {:?}", self.cur_token),
            }
        }
        parameters
    }

    fn parse_function_body(&mut self) -> Vec<Statement> {
        let mut statements = Vec::new();

        self.next_token();

        while let Some(token) = &self.cur_token {
            match token {
                TokenType::CloseBrace => {
                    self.next_token();
                    break;
                }
                _ => {
                    let statement = self.parse_statement();
                    statements.push(statement);
                }
            }
        }
        statements
    }

    pub fn parse_statement(&mut self) -> Statement {
        match &self.cur_token {
            Some(TokenType::Int) => {
                self.next_token();
                match &self.cur_token {
                    Some(TokenType::Identifier(id)) => {
                        let str_id = id.to_string();
                        self.next_token();
                        match &self.cur_token {
                            Some(TokenType::OpenParen) => {
                                //Add function handling
                                let parameters = self.parse_parameters();
                                let body = self.parse_function_body();
                                Statement::FunctionDeclaration(str_id, parameters, body)
                            }
                            Some(TokenType::Semicolon) => {
                                Statement::VariableDeclaration(str_id, "int".to_string(), None)
                                //Init variable with no value
                            }
                            Some(TokenType::Equal) => {
                                self.next_token();
                                let initializer = self.parse_expr();
                                match &self.cur_token {
                                    Some(TokenType::Semicolon) => {
                                        self.next_token();
                                        Statement::VariableDeclaration(
                                            "int".to_string(),
                                            str_id,
                                            Some(initializer),
                                        )
                                    }
                                    _ => panic!("Expected semicolon after variable declaration"),
                                }
                            }
                            _ => panic!("Unexpected token after identifier"),
                        }
                    }
                    _ => panic!("Unsure what error is"),
                }
            }
            Some(TokenType::Return) => {
                self.next_token();
                let expr = self.parse_expr();
                match &self.cur_token {
                    Some(TokenType::Semicolon) => {
                        self.next_token();
                        Statement::Return(expr)
                    }
                    _ => panic!("Expected semicolon after return statement"),
                }
            }
            _ => {
                let expr = self.parse_expr();
                Statement::Expression(expr)
            }
        }
    }
}

// Stolen function for printing tree
fn print_expr(expr: &Expr, indent: usize) {
    let indent_str = " ".repeat(indent);

    match expr {
        Expr::Int(value) => println!("{}Int: {}", indent_str, value),
        Expr::Variable(name) => println!("{}Variable: {}", indent_str, name),
        Expr::FunctionCall(name, args) => {
            println!("{}FunctionCall: {}", indent_str, name);
            for arg in args {
                print_expr(arg, indent + 2);
            }
        }
        Expr::Negation(boxed) => println!("{}Negated: {:?}", indent_str, boxed),
        Expr::BinOp(left, op, right) => {
            println!("{}BinOp: {:?}", indent_str, op);
            print_expr(&*left, indent + 2);
            print_expr(&*right, indent + 2);
        }
    }
}

// Add this function to print parameters
fn print_parameter(param: &Parameter, indent: usize) {
    let indent_str = " ".repeat(indent);
    println!("{}Parameter: {} {}", indent_str, param.typ, param.name);
}

// Add this function to print statements
fn print_statement(stmt: &Statement, indent: usize) {
    let indent_str = " ".repeat(indent);

    match stmt {
        Statement::Expression(expr) => {
            println!("{}Expr:", indent_str);
            print_expr(expr, indent + 2);
        }
        Statement::Return(expr) => {
            println!("{}Return:", indent_str);
            print_expr(expr, indent + 2);
        }
        Statement::FunctionDeclaration(name, params, body) => {
            println!("{}FunctionDeclaration: {}", indent_str, name);
            for param in params {
                print_parameter(param, indent + 2);
            }
            for stmt in body {
                print_statement(stmt, indent + 2);
            }
        }
        Statement::VariableDeclaration(typ, name, initializer) => {
            println!("{}VariableDeclaration: {} {}", indent_str, typ, name);
            if let Some(expr) = initializer {
                print_expr(expr, indent + 2);
            }
        }
    }
}

// Add this function to print a program
pub fn print_program(program: &Program) {
    for stmt in &program.statements {
        print_statement(stmt, 0);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_int() {
        let lexer = Lexer::new("123");
        let mut parser = Parser::new(lexer);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Int(123));
    }

    #[test]
    fn test_parse_variable() {
        let lexer = Lexer::new("foo");
        let mut parser = Parser::new(lexer);
        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Variable("foo".to_string()));
    }

    #[test]
    fn test_parse_function_call() {
        let lexer = Lexer::new("bar(123, foo)");
        let mut parser = Parser::new(lexer);
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::FunctionCall(
                "bar".to_string(),
                vec![Expr::Int(123), Expr::Variable("foo".to_string())]
            )
        );
    }
    #[test]
    fn test_parse_unary_operation() {
        let lexer = Lexer::new("-foo");
        let mut parser = Parser::new(lexer);
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Negation(Box::new(Expr::Variable("foo".to_string())))
        );
    }
    #[test]
    fn test_parse_program() {
        let input = r#"
            int adder(int a, int b) {
                int c = a + b;
                return c + 5;
            }
            int main() {
                int d = adder(3, 4);
                return 0;
            }
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        assert_eq!(
            program.statements,
            vec![
                Statement::FunctionDeclaration(
                    "adder".to_string(),
                    vec![
                        Parameter {
                            name: "a".to_string(),
                            typ: "int".to_string()
                        },
                        Parameter {
                            name: "b".to_string(),
                            typ: "int".to_string()
                        }
                    ],
                    vec![
                        Statement::VariableDeclaration(
                            "int".to_string(),
                            "c".to_string(),
                            Some(Expr::BinOp(
                                Box::new(Expr::Variable("a".to_string())),
                                Op::Plus,
                                Box::new(Expr::Variable("b".to_string()))
                            ))
                        ),
                        Statement::Return(Expr::BinOp(
                            Box::new(Expr::Variable("c".to_string())),
                            Op::Plus,
                            Box::new(Expr::Int(5))
                        ))
                    ]
                ),
                Statement::FunctionDeclaration(
                    "main".to_string(),
                    vec![],
                    vec![
                        Statement::VariableDeclaration(
                            "int".to_string(),
                            "d".to_string(),
                            Some(Expr::FunctionCall(
                                "adder".to_string(),
                                vec![Expr::Int(3), Expr::Int(4)]
                            ))
                        ),
                        Statement::Return(Expr::Int(0))
                    ]
                ),
            ]
        );
    }
}
