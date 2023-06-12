use std::char::DecodeUtf16Error;
use crate::lexer::TokenType;
use crate::Lexer;
use crate::parser::Declaration::{StructDeclaration, TypeDefDeclaration};
use crate::parser::TypeDefType::{Basic, Struct};

macro_rules! expect_token {
    ($self:expr, $expected_token:expr) => {
        match &$self.cur_token {
            Some(token) if *token == $expected_token => {
                $self.next_token();
            },
            _ => panic!("Expected {:?}, found {:?}", $expected_token, $self.cur_token),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    Assignment(Box<Expr>, Box<Expr>),
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
pub enum Declaration {
    VariableDeclaration(String, String, Option<Expr>),
    FunctionDeclaration(String, String, Vec<Parameter>, Vec<Instruction>),
    TypeDefDeclaration(TypeDefType),
    StructDeclaration(Option<String>, Vec<Instruction>,Option<String>)
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructStruct {
    pub name: Option<String>,
    pub body: Vec<Instruction>,
    pub alias: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeDefType {
    Basic(String, String),
    Struct(Option<String>, Vec<Instruction>,Option<String>),
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

    fn expect_and_consume(&mut self, expected: TokenType) {
        match &self.cur_token {
            Some(t) if *t == expected => self.next_token(),
            _ => panic!("Expected {:?}, found {:?}", expected, self.cur_token),
        }
    }

    fn expect_token(&mut self, expected: TokenType) {
        match &self.cur_token {
            Some(t) if *t == expected => {},
            _ => panic!("Expected {:?}, found {:?}", expected, self.cur_token),
        }
    }

    fn consume_identifier(&mut self) -> Option<String> {
        match &self.cur_token {
            Some(TokenType::Identifier(id)) => {
                let id_str = id.to_string();
                self.next_token();
                Some(id_str)
            }
            _ => None,
        }
    }

    pub fn parse_expr(&mut self) -> Expr {
        let mut expr = self.parse_unary_expr();

        while let Some(op) = self.get_binary_op() {
            self.next_token();
            let right = self.parse_unary_expr();
            expr = Expr::BinOp(Box::new(expr), op, Box::new(right))
        }
        if let Some(TokenType::Equal) = self.cur_token{
            self.next_token();
            let right = self.parse_expr();
            expr = Expr::Assignment(Box::new(expr), Box::new(right))
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

    fn parse_struct_body(&mut self) -> (Option<String>, Vec<Instruction>, Option<String>) {
        let id_str = match &self.cur_token {
            Some(TokenType::Identifier(id)) => {
                let id_str = id.to_string();
                self.next_token();
                Some(id_str)
            },
            Some(TokenType::OpenBrace) => None,
            _ => panic!("Expected identifier or opening curly bracket")
        };

        self.expect_and_consume(TokenType::OpenBrace);
        let body = self.parse_body();

        let alias = match &self.cur_token {
            Some(TokenType::Identifier(id)) => {
                let id_str = id.to_string();
                self.next_token();
                Some(id_str)
            },
            Some(TokenType::Semicolon) => None,
            _ => panic!("Expected alias in typedef, found {:?}", self.cur_token)
        };

        (id_str, body, alias)
    }

    fn parse_body(&mut self) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        while let Some(token) = &self.cur_token {
            match token {
                TokenType::CloseBrace => {
                    self.next_token();
                    break;
                }
                TokenType::Return | TokenType::Identifier(..) => {
                    let statement = self.parse_statement();
                    instructions.push(Instruction::Statement(statement));
                }
                _ => {
                    let declaration = self.parse_declaration() ;
                    instructions.push(Instruction::Declaration(declaration));
                }
            }
        }
        instructions
    }

    fn get_type_string(&mut self) -> Option<String> {
        match &self.cur_token {
            Some(TokenType::Identifier(id)) => {
                let id_str = id.to_string();
                self.next_token();
                Some(id_str)
            },
            Some(TokenType::Int) => {
                self.next_token();
                Some("int".to_string())
            },
            Some(TokenType::Char) => {
                self.next_token();
                Some("char".to_string())
            },
            _ => None,
        }
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
                                self.next_token();
                                let body = self.parse_body();
                                Declaration::FunctionDeclaration(number_type.unwrap(), str_id, parameters, body)
                            }
                            Some(TokenType::Semicolon) => {
                                self.next_token();
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
                                TypeDefDeclaration(Basic(existing_string, alias_string))
                            }
                            _ => panic!("Expected alias in typedef, found {:?}", self.cur_token)
                        }
                    }
                    Some(TokenType::Struct) => {
                        self.next_token();
                        let (existing, body, alias) = self.parse_struct_body();
                        TypeDefDeclaration(Struct(existing, body, alias))
                    }
                    Some(TokenType::Int) => {
                        let type_string = format!("{:?}", self.cur_token.as_ref().unwrap()).to_lowercase();
                        self.next_token();
                        match &self.cur_token {
                            Some(TokenType::Identifier(alias)) => {
                                let alias_str = alias.to_string();
                                TypeDefDeclaration(Basic(type_string,alias_str))
                            }
                            _ => panic!("Expected alias in typedef, found {:?}", self.cur_token)
                        }
                    }
                    _ => panic!("Typedef type not found, found {:?}", self.cur_token)
                }
            }
            Some(TokenType::Struct) => {
                self.next_token();
                let (id_str, body, alias) = self.parse_struct_body();
                StructDeclaration(id_str, body, alias)
            },
            _ => panic!("Unexpected token in declaration: {:?}", self.cur_token)
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
    use crate::parser::Declaration::{FunctionDeclaration, VariableDeclaration};
    use crate::parser::Expr::BinOp;
    use crate::parser::Statement::Return;
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
    fn test_reassign_variable() {
        let lexer = Lexer::new("c = c + 5");
        let mut parser = Parser::new(lexer);
        let expr = parser.parse_expr();
        assert_eq!(
            expr,
            Expr::Assignment(Box::new(Expr::Variable("c".to_string())),
                             Box::new(BinOp(
                                 Box::new(Expr::Variable("c".to_string())),
                                 Op::Plus,
                                 Box::new(Expr::Int(5)))))
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
        let lexer = Lexer::new("typedef name_str alias_str;");
        let mut parser = Parser::new(lexer);
        let declaration = parser.parse_declaration();
        assert_eq!(
            declaration,
            TypeDefDeclaration(Basic("name_str".to_string(), "alias_str".to_string()))
        );
    }

    #[test]
    fn test_parse_struct_declaration() {
        let code = r#"
        struct Test {
            int field;
        };
        "#;

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let declaration = parser.parse_declaration();
        let expected_declaration = Declaration::StructDeclaration(
            Some("Test".to_string()),
            vec![
                Instruction::Declaration(Declaration::VariableDeclaration(
                    "int".to_string(),
                    "field".to_string(),
                    None))
            ],
            None
        );

        assert_eq!(declaration, expected_declaration);
    }

    #[test]
    fn test_parse_struct_alias() {
        let code = r#"
        struct Test {
            int field;
        } alias;
        "#;

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let declaration = parser.parse_declaration();
        let expected_declaration = Declaration::StructDeclaration(
            Some("Test".to_string()),
            vec![
                Instruction::Declaration(Declaration::VariableDeclaration(
                    "int".to_string(),
                    "field".to_string(),
                    None))
            ],
            Some("alias".to_string())
        );

        assert_eq!(declaration, expected_declaration);
    }

    #[test]
    fn test_parse_typedef_basic_declaration() {
        let code = r#"
        typedef int INTEGER;
        "#;

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let declaration = parser.parse_declaration();
        let expected_declaration = TypeDefDeclaration(Basic("int".to_string(), "INTEGER".to_string())
        );

        assert_eq!(declaration, expected_declaration);
    }

    #[test]
    fn test_parse_typedef_struct_declaration() {
        let code = r#"
        typedef struct Test {
            int field;
        } TestAlias;
        "#;

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let declaration = parser.parse_declaration();
        let expected_declaration = Declaration::TypeDefDeclaration(
            TypeDefType::Struct(
                Some("Test".to_string()),
                vec![
                    Instruction::Declaration(Declaration::VariableDeclaration(
                        "int".to_string(),
                        "field".to_string(),
                        None))
                ],
                Some("TestAlias".to_string())
            )
        );

        assert_eq!(declaration, expected_declaration);
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
            program.contains,
            vec![
                Instruction::Declaration(FunctionDeclaration(
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
                        Instruction::Declaration(VariableDeclaration(
                            "int".to_string(),
                            "c".to_string(),
                            Some(Expr::BinOp(
                                Box::new(Expr::Variable("a".to_string())),
                                Op::Plus,
                                Box::new(Expr::Variable("b".to_string()))
                            ))
                        )),
                        Instruction::Statement(Return(Expr::BinOp(
                            Box::new(Expr::Variable("c".to_string())),
                            Op::Plus,
                            Box::new(Expr::Int(5))
                        )))
                    ]
                )),
                Instruction::Declaration(FunctionDeclaration(
                    "int".to_string(),
                    "main".to_string(),
                    vec![],
                    vec![
                        Instruction::Declaration(VariableDeclaration(
                            "int".to_string(),
                            "d".to_string(),
                            Some(Expr::FunctionCall(
                                "adder".to_string(),
                                vec![Expr::Int(3), Expr::Int(4)]
                            ))
                        )),
                        Instruction::Statement(Return(Expr::Int(0)))
                    ]
                )),
            ]
        );
    }
    #[test]
    #[should_panic]
    fn test_parse_invalid_typedef_struct_declaration() {
        let code = r#"
    typedef struct Test {
        int field;
    }  // missing alias
    "#;

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_declaration();
    }
    #[test]
    #[should_panic]
    fn test_parse_invalid_int() {
        let lexer = Lexer::new("12a3"); // contains non-numeric character
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_expr();
    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_variable() {
        let lexer = Lexer::new("foo123");
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_expr();
    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_function_call() {
        let lexer = Lexer::new("bar(123, foo"); // missing closing parenthesis
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_expr();
    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_struct_declaration() {
        let code = r#"
    struct Test {
        int field
    };  // missing semicolon after field declaration
    "#;

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_declaration();
    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_typedef_declaration() {
        let code = r#"
    typedef int  // missing alias
    "#;

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_declaration();
    }

}