use crate::lexer::TokenType;
use crate::Lexer;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Variable(String),
    FunctionCall(String, Vec<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expr(Expr),
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter {
    name: String,
    typ: String,
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
        match &self.cur_token {
            Some(TokenType::IntConst(int)) => {
                let int_value = *int;
                self.next_token();
                let expr = Expr::Int(int_value);

                match &self.cur_token {
                    Some(TokenType::Plus) => {
                        self.next_token();
                        let right = self.parse_expr();
                        Expr::BinOp(Box::new(expr), Op::Plus, Box::new(right))
                    }
                    _ => expr,
                }
            }
            _ => panic!("Need to add this expression or Unknown expression"),
        }
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
                                name: id.clone(),
                                typ: "int".to_string(),
                            });
                        }
                        _ => panic!("Expected identifier"),
                    }
                }
                _ => panic!("Unexpected function type"),
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
            _ => {
                let expr = self.parse_expr();
                Statement::Expr(expr)
            }
        }
    }
}
