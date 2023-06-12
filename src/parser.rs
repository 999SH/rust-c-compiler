use std::char::DecodeUtf16Error;
use crate::lexer::TokenType;
use crate::Lexer;
use crate::parser::Declaration::TypeDefDeclaration;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Variable(String),
    FunctionCall(String, Vec<Expr>),
    Negation(Box<Expr>),
    Not(Box<Expr>),
    BitNOT(Box<Expr>),
    Dereference(Box<Expr>),
    Address(Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expression(Expr),
    Return(Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructVariable {
    pub name: String,
    pub vartype: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    VariableDeclaration(String, String, Option<Expr>),
    FunctionDeclaration(String, String, Vec<Parameter>, Vec<Instruction>),
    TypeDefDeclaration(String, TypeDefType),
    StructDeclaration(String, Vec<StructVariable>)
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeDefType {
    Basic(String),
    Struct(String),
    Union(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
    Multiplication,
    Division,
    BitOR,
    BitAND,
    BitXOR,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub paramtype: String,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Declaration(Declaration),
    Statement(Statement),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub contains : Vec<Instruction>
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
        let mut program = Program { contains: vec![], };

        while let Some(token) = &self.cur_token {
            match token {
                TokenType::EOF => break,
                TokenType::Int | TokenType::Double | TokenType::Long | TokenType::Short | TokenType::Typedef => {
                    let declaration = self.parse_declaration();
                    program.contains.push(Instruction::Declaration(declaration))
                }
                _ => {
                    let statement = self.parse_statement();
                    program.contains.push(Instruction::Statement(statement))
                }
            }
        }
        program
    }

    pub fn parse_expr(&mut self) -> Expr {
        let mut expr = self.parse_unary_expr();

        while let Some(op) = self.get_binary_op() {
            self.next_token();
            let right = self.parse_unary_expr();
            expr = Expr::BinOp(Box::new(expr), op, Box::new(right))
        }

        expr
    }
    fn parse_unary_expr(&mut self) -> Expr {
        match &self.cur_token {
            //Open paren means a prioritzed expression
            Some(TokenType::OpenParen) => {
                self.next_token();
                let expr = self.parse_expr();
                match &self.cur_token {
                    Some(TokenType::CloseParen) => self.next_token(),
                    _ => panic!("Expected closing parthesis for expression"),
                }
                expr
            }
            Some(TokenType::IntConst(int)) => {
                let dint = *int;
                self.next_token();
                Expr::Int(dint)
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
            //Tokentype minus in this context means negative, not the operation
            Some(TokenType::Minus) => {
                self.next_token();
                let expr = Box::new(self.parse_expr());
                Expr::Negation(expr)
            }
            //Logical not
            Some(TokenType::Not) => {
                self.next_token();
                let expr = Box::new(self.parse_expr());
                Expr::Not(expr)
            }
            //Unary bitwise NOT
            Some(TokenType::Tilde) => {
                self.next_token();
                let expr = Box::new(self.parse_expr());
                Expr::BitNOT(expr)
            }
            //Dereference
            Some(TokenType::Star) => {
                self.next_token();
                let expr = Box::new(self.parse_expr());
                Expr::Dereference(expr)
            }
            //Adress
            Some(TokenType::Ampersand) => {
                self.next_token();
                let expr = Box::new(self.parse_expr());
                Expr::Address(expr)
            }
            _ => panic!("Need to add this expression {:?}", self.cur_token),
        }
    }

    fn get_binary_op(&self) -> Option<Op> {
        match &self.cur_token {
            Some(TokenType::Plus) => Some(Op::Plus),
            Some(TokenType::Minus) => Some(Op::Minus),
            Some(TokenType::Star) => Some(Op::Multiplication),
            Some(TokenType::Slash) => Some(Op::Division),
            Some(TokenType::Ampersand) => Some(Op::BitAND),
            Some(TokenType::Or) => Some(Op::BitOR),
            Some(TokenType::Xor) => Some(Op::BitXOR),
            _ => None,
        }
    }

    fn get_number_type(&self) -> Option<String> {
        match &self.cur_token {
            Some(TokenType::Int) => Some("int".to_string()),
            Some(TokenType::Double) => Some("double".to_string()),
            Some(TokenType::Long) => Some("long".to_string()),
            Some(TokenType::Short) => Some("short".to_string()),
            _ => None,
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
                Some(TokenType::Int) | Some(TokenType::Long) |
                Some(TokenType::Double) | Some(TokenType::Short) => {
                    let number_type = self.get_number_type();
                    self.next_token();
                    match &self.cur_token {
                        Some(TokenType::Identifier(id)) => {
                            parameters.push(Parameter {
                                name: id.to_string(),
                                paramtype: number_type.unwrap(),
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

    fn parse_function_body(&mut self) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        self.next_token();
        while let Some(token) = &self.cur_token {
            match token {
                TokenType::CloseBrace => {
                    self.next_token();
                    break;
                }
                _ => {
                        if let Some(declaration) = self.parse_declaration() {
                            instructions.push(Instruction::Declaration(declaration));
                        } else {
                            let statement = self.parse_statement();
                            instructions.push(Instruction::Statement(statement));
                        }
                }
            }
        }
        instructions
    }

    pub fn parse_declaration(&mut self) -> Declaration {
        match &self.cur_token {
            Some(TokenType::Int) | Some(TokenType::Long) |
            Some(TokenType::Double) | Some(TokenType::Short) => {
                let number_type = self.get_number_type();
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
                                Declaration::FunctionDeclaration(number_type.unwrap(), str_id, parameters, body)
                            }
                            Some(TokenType::Semicolon) => {
                                Declaration::VariableDeclaration(number_type.unwrap(), str_id,  None)
                                //Init variable with no value
                            }
                            Some(TokenType::Equal) => {
                                self.next_token();
                                let initializer = self.parse_expr();
                                match &self.cur_token {
                                    Some(TokenType::Semicolon) => {
                                        self.next_token();
                                        Declaration::VariableDeclaration(
                                            number_type.unwrap(),
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
            Some(TokenType::Typedef) => {
                self.next_token();
                match &self.cur_token {
                    Some(TokenType::Identifier(existing)) => {
                        let existing_string = existing.to_string();
                        self.next_token();
                        match &self.cur_token {
                            Some(TokenType::Identifier(alias)) => {
                                let alias_string = alias.to_string();
                                Declaration::TypeDefDeclaration(existing_string,TypeDefType::Basic("alias".to_string()),
                                )
                            }
                            _ => panic!("Expected alias in typedef, found {:?}", self.cur_token)
                        }
                    }
                    _ => panic!("Expected name in typedef, found {:?}", self.cur_token)
                }
            }
            _ => {
                panic!("Unexpected token in declaration: {:?}", self.cur_token)
            }
        }
    }

    pub fn parse_statement(&mut self) -> Statement {
        match &self.cur_token {
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
    fn test_parse_not_operation() {
        let input = r#"
            !foo
        "#;
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let expr = parser.parse_expr();
        assert_eq!(expr, Expr::Not(Box::new(Expr::Variable("foo".to_string()))));
    }

    #[test]
    fn test_typedef_alias() {
        let lexer = Lexer::new("typedef name_str alias_str");
        let mut parser = Parser::new(lexer);
        let statement = parser.parse_statement();
        assert_eq!(
            statement,
            Statement::TypeDef("name_str".to_string(), "alias_str".to_string())
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
            program.declarations,
            vec![
                Declaration::FunctionDeclaration(
                    "int".to_string(),
                    "adder".to_string(),
                    vec![
                        Parameter {
                            name: "a".to_string(),
                            paramtype: "int".to_string()
                        },
                        Parameter {
                            name: "b".to_string(),
                            paramtype: "int".to_string()
                        }
                    ],
                    vec![
                        Declaration::VariableDeclaration(
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
                Declaration::FunctionDeclaration(
                    "int".to_string(),
                    "main".to_string(),
                    vec![],
                    vec![
                        Declaration::VariableDeclaration(
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