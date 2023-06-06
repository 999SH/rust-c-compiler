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
    Declaraion(String, Expr),
    Function(String, Vec<(String, String)>, Box<Statement>),
    FunctionDeclaration(String, Vec<String>, Vec<Statement>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Plus,
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

    pub fn parse_statement(&mut self) -> Statement {
        match & self.cur_token {
            Some(TokenType::Int => {
                self.next_token();
                match &self.cur_token {
                    Some(TokenType::Identifier(id)) => {
                        self.next_token();
                        match &self.cur_token {
                            Some(TokenType::OpenParen) => {
                                //Add function handling
                            },
                            Some (TokenType::Semicolon) =>{
                                //Init variable with no value
                            },
                            Some(TokenType::Equal) => {
                                //Assign value when initing variable
                            },
                            _ => panic!("Unexpected token after identifier")
                        }
                    },
                    _ => panic!("Unsure what error is")
                }
            },
        _ => {
                let expr = self.parse_expr();
                Statement::Expr(expr)
            },
        }
    }
}
