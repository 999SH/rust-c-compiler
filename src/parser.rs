use crate::lexer::TokenType;
use crate::parser::Declaration::{StructDeclaration, TypeDefDeclaration};
use crate::parser::TypeDefType::{Basic, Struct};
use crate::Lexer;

macro_rules! expect_token {
    ($self:expr, $expected_token:expr) => {
        match &$self.cur_token {
            Some(token) if *token == $expected_token => {
                $self.next_token();
            }
            _ => panic!(
                "Expected {:?}, found {:?}",
                $expected_token, $self.cur_token
            ),
        }
    };
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
/*
Statements: Expresion, Return: Self explanatory
If statement, First box is conditions, Vec contains body of the statement,
the optional Box contains an instruction which either is either an (else if) or else statement
While loop: First box contains conditional, Vec contains body
 */
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Expression(Expr),
    Return(Expr),
    IfStatement(Box<Expr>, Vec<Instruction>, Option<Box<Instruction>>),
    Else(Vec<Instruction>),
    WhileLoop(Box<Expr>, Vec<Instruction>)
}
/*
Declarations:
Variable: Type, Name and optional assignment. Int x; or int x = 5; are both valid
Function: Type, Name, Parameters and body
Typedef, refers to typedeftype enum where all of the typedef info is stored
Struct: Optional name, Body/initializations, Optional alias
 */
#[derive(Debug, PartialEq, Eq)]
pub enum Declaration {
    VariableDeclaration(String, String, Option<Expr>),
    FunctionDeclaration(String, String, Vec<Parameter>, Vec<Instruction>),
    TypeDefDeclaration(TypeDefType),
    StructDeclaration(Option<String>, Vec<Instruction>, Option<String>),
}
/*
Possibly unused code
 */
#[derive(Debug, PartialEq, Eq)]
pub struct StructStruct {
    pub name: Option<String>,
    pub body: Vec<Instruction>,
    pub alias: Option<String>,
}
/*
Typedef types,
Normal: Name, alias
Struct, Same as above
Union TODO
 */
#[derive(Debug, PartialEq, Eq)]
pub enum TypeDefType {
    Basic(String, String),
    Struct(Option<String>, Vec<Instruction>, Option<String>),
    Union(String),
}
/*
Bitwise operators are single characters such as &, where as && is AndAnd (Logical and)
 */
#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Plus,
    Minus,
    Multiplication,
    Division,
    BitOR,
    BitAND,
    BitXOR,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    AndAnd,
    OrOr,
    Assign,
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
    pub contains: Vec<Instruction>,
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
        let mut program = Program { contains: vec![] };

        while let Some(token) = &self.cur_token {
            match token {
                TokenType::EOF => break,
                TokenType::Int
                | TokenType::Double
                | TokenType::Long
                | TokenType::Short
                | TokenType::Typedef
                | TokenType::Char
                | TokenType::Struct
                | TokenType::Unsigned
                | TokenType::Void => {
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
            Some(TokenType::AndAnd) => Some(Op::AndAnd),
            Some(TokenType::OrOr) => Some(Op::OrOr),
            Some(TokenType::Less) => Some(Op::Less),
            Some(TokenType::LessEqual) => Some(Op::LessEqual),
            Some(TokenType::Greater) => Some(Op::Greater),
            Some(TokenType::GreaterEqual) => Some(Op::GreaterEqual),
            Some(TokenType::Equal) => Some(Op::Assign),
            _ => None,
        }
    }

    fn token_to_string(&self) -> String {
        match self.cur_token.as_ref().unwrap() {
            TokenType::Unsigned => "unsigned".to_string(),
            TokenType::Long => "long".to_string(),
            TokenType::Short => "short".to_string(),
            TokenType::Int => "int".to_string(),
            TokenType::Double => "double".to_string(),
            TokenType::Char => "char".to_string(),
            _ => unreachable!(),
        }
    }
    fn expect_and_consume(&mut self, expected: TokenType) {
        match &self.cur_token {
            Some(t) if *t == expected => self.next_token(),
            _ => panic!("Expected {:?}, found {:?}", expected, self.cur_token),
        }
    }

    fn parse_type(&mut self) -> String {
        let mut type_specifiers = Vec::new();
        loop {
            match self.cur_token {
                Some(TokenType::Unsigned)
                | Some(TokenType::Long)
                | Some(TokenType::Short)
                | Some(TokenType::Int)
                | Some(TokenType::Double)
                | Some(TokenType::Char) => {
                    type_specifiers.push(self.token_to_string());
                    self.next_token();
                }
                _ => break,
            }
        }
        if type_specifiers.is_empty() {
            "".to_string()
        } else {
            type_specifiers.join(" ")
        }
    }

    fn parse_unary_expr(&mut self) -> Expr {
        match &self.cur_token {
            //Open paren means a prioritzed expression
            Some(TokenType::OpenParen) => {
                self.next_token();
                let expr = self.parse_expr();
                match &self.cur_token {
                    Some(TokenType::CloseParen) => self.next_token(),
                    _ => panic!("Expected closing parenthesis for expression"),
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
                    Some(TokenType::Identifier(_)) => {
                        panic!(" {} is an invalid type!", var_id)
                    }
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

    fn get_precedence(&self) -> i32 {
        match &self.cur_token {
            Some(TokenType::Equal) => 0,
            Some(TokenType::Minus) => 1,
            Some(TokenType::Plus) => 2,
            Some(TokenType::Slash) => 3,
            Some(TokenType::Star) => 4,
            Some(TokenType::Xor) => 5,
            Some(TokenType::OpenParen) => 7,
            _ => 0,
        }
    }

    fn parse_binop(&mut self, unary: Expr, min_precedence: i32) -> Expr {
        //let mut expr = self.parse_unary_expr();
        let mut expr = unary;

        while let Some(op) = self.get_binary_op() {
            let op_precedence = self.get_precedence();

            if op_precedence < min_precedence {
                break
            }
            self.next_token();
            let right = self.parse_unary_expr();
            let right = self.parse_binop(right,op_precedence + 1);

            expr = Expr::BinOp(Box::new(expr), op, Box::new(right))
        }
        expr
    }
    /*
    Parse expression, check if binary operation
     */
    pub fn parse_expr(&mut self) -> Expr {
        let mut expr = self.parse_unary_expr();
        self.parse_binop(expr, 0)
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
                Some(TokenType::Int)
                | Some(TokenType::Long)
                | Some(TokenType::Double)
                | Some(TokenType::Short) => {
                    let number_type = self.token_to_string();
                    self.next_token();
                    match &self.cur_token {
                        Some(TokenType::Identifier(id)) => {
                            parameters.push(Parameter {
                                name: id.to_string(),
                                paramtype: number_type,
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

    fn parse_body(&mut self) -> Vec<Instruction> {
        let mut instructions = Vec::new();
        while let Some(token) = &self.cur_token {
            match token {
                TokenType::EOF => break,
                TokenType::CloseBrace => {
                    break;
                }
                TokenType::Return |
                TokenType::Identifier(..) |
                TokenType::If |
                TokenType::IntConst(..) |
                TokenType::While => {
                    let statement = self.parse_statement();
                    instructions.push(Instruction::Statement(statement));
                }
                _ => {
                    let declaration = self.parse_declaration();
                    instructions.push(Instruction::Declaration(declaration));
                }
            }
        }
        expect_token!(self, TokenType::CloseBrace);
        instructions
    }

    fn parse_struct_body(&mut self) -> (Option<String>, Vec<Instruction>, Option<String>) {
        let id_str = match &self.cur_token {
            Some(TokenType::Identifier(id)) => {
                let id_str = id.to_string();
                self.next_token();
                Some(id_str)
            }
            Some(TokenType::OpenBrace) => None,
            _ => panic!("Expected identifier or opening curly bracket"),
        };

        self.expect_and_consume(TokenType::OpenBrace);
        let body = self.parse_body();

        let alias = match &self.cur_token {
            Some(TokenType::Identifier(id)) => {
                let id_str = id.to_string();
                self.next_token();
                Some(id_str)
            }
            Some(TokenType::Semicolon) => None,
            _ => panic!("Expected alias in typedef, found {:?}", self.cur_token),
        };

        (id_str, body, alias)
    }

    pub fn parse_declaration(&mut self) -> Declaration {
        match &self.cur_token {
            Some(TokenType::Int)
            | Some(TokenType::Long)
            | Some(TokenType::Double)
            | Some(TokenType::Short)
            | Some(TokenType::Unsigned)
            | Some(TokenType::Char) => {
                let number_type = self.parse_type();
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
                                Declaration::FunctionDeclaration(
                                    number_type,
                                    str_id,
                                    parameters,
                                    body,
                                )
                            }
                            Some(TokenType::Semicolon) => {
                                self.next_token();
                                Declaration::VariableDeclaration(number_type, str_id, None)
                                //Init variable with no value
                            }
                            Some(TokenType::Equal) => {
                                self.next_token();
                                let initializer = self.parse_expr();
                                match &self.cur_token {
                                    Some(TokenType::Semicolon) => {
                                        self.next_token();
                                        Declaration::VariableDeclaration(
                                            number_type,
                                            str_id,
                                            Some(initializer),
                                        )
                                    }
                                    _ => panic!("Expected semicolon after variable declaration"),
                                }
                            }
                            _ => panic!("Expected semicolon after variable declaration"),
                        }
                    }
                    _ => panic!("Expected Identifier after variable declaration"),
                }
            }
            Some(TokenType::Typedef) => {
                self.next_token();
                let specifiers = self.parse_type();
                match &self.cur_token {
                    Some(TokenType::Identifier(existing_or_alias)) => {
                        let cloned_str = existing_or_alias.to_string();
                        self.next_token();
                        if specifiers.is_empty() {
                            match &self.cur_token {
                                Some(TokenType::Identifier(alias)) => {
                                    let alias_string = alias.to_string();
                                    TypeDefDeclaration(Basic(cloned_str, alias_string))
                                }
                                _ => {
                                    panic!("Expected alias in typedef, found {:?}", self.cur_token)
                                }
                            }
                        } else {
                            let type_string = format!("{}", specifiers);
                            TypeDefDeclaration(Basic(type_string, cloned_str))
                        }
                    }
                    Some(TokenType::Struct) => {
                        self.next_token();
                        let (existing, body, alias) = self.parse_struct_body();
                        TypeDefDeclaration(Struct(existing, body, alias))
                    }
                    Some(TokenType::Int)
                    | Some(TokenType::Unsigned)
                    | Some(TokenType::Long)
                    | Some(TokenType::Short)
                    | Some(TokenType::Double)
                    | Some(TokenType::Char) => {
                        let type_string = self.token_to_string();
                        self.next_token();
                        match &self.cur_token {
                            Some(TokenType::Identifier(alias)) => {
                                let alias_str = alias.to_string();
                                TypeDefDeclaration(Basic(type_string, alias_str))
                            }
                            _ => panic!("Expected alias in typedef, found {:?}", self.cur_token),
                        }
                    }
                    _ => panic!("Expected alias in typedef"),
                }
            }
            Some(TokenType::Struct) => {
                self.next_token();
                let (id_str, body, alias) = self.parse_struct_body();
                StructDeclaration(id_str, body, alias)
            }
            _ => panic!("Unexpected token in declaration: {:?}", self.cur_token),
        }
    }

    fn parse_if_statement(&mut self) -> Statement {
        self.next_token(); // consume the `if` keyword
        let condition = Box::new(self.parse_expr());

        match &self.cur_token {
            Some(TokenType::OpenBrace) => {
                self.next_token();
                let if_body = self.parse_body();

                let else_body = match &self.cur_token {
                    Some(TokenType::Else) => {
                        self.next_token();
                        match &self.cur_token {
                            Some(TokenType::If) => {
                                Some(Box::new(Instruction::Statement(self.parse_if_statement())))
                            }
                            _ => {
                                expect_token!(self, TokenType::OpenBrace);
                                Some(Box::new(Instruction::Statement(Statement::Else(
                                    self.parse_body(),
                                ))))
                            },
                        }
                    }
                    _ => None,
                };
                Statement::IfStatement(condition, if_body, else_body)
            }
            _ => panic!("Expected opening curly brackets after if statement"),
        }
    }

    fn parse_while_loop(&mut self) -> Statement {
        self.next_token();
        let condition = Box::new(self.parse_expr());
        expect_token!(self, TokenType::OpenBrace);
        let loop_body = self.parse_body();
        Statement::WhileLoop(condition,loop_body)
    }

    pub fn parse_statement(&mut self) -> Statement {
        match &self.cur_token {
            Some(TokenType::If) => self.parse_if_statement(),
            Some(TokenType::While) => self.parse_while_loop(),
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
                expect_token!(self, TokenType::Semicolon);
                self.next_token();
                Statement::Expression(expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs::read;
    use super::*;
    use crate::parser::Declaration::{FunctionDeclaration, VariableDeclaration};
    use crate::parser::Expr::{BinOp, Int, Variable};
    use crate::parser::Statement::{Else, Expression, IfStatement, Return, WhileLoop};

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
            Expr::BinOp(
                Box::new(Expr::Variable("c".to_string())),
                Op::Assign,
                Box::new(BinOp(
                    Box::new(Expr::Variable("c".to_string())),
                    Op::Plus,
                    Box::new(Expr::Int(5))
                ))
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
            vec![Instruction::Declaration(Declaration::VariableDeclaration(
                "int".to_string(),
                "field".to_string(),
                None,
            ))],
            None,
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
            vec![Instruction::Declaration(Declaration::VariableDeclaration(
                "int".to_string(),
                "field".to_string(),
                None,
            ))],
            Some("alias".to_string()),
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
        let expected_declaration =
            TypeDefDeclaration(Basic("int".to_string(), "INTEGER".to_string()));

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
        let expected_declaration = Declaration::TypeDefDeclaration(TypeDefType::Struct(
            Some("Test".to_string()),
            vec![Instruction::Declaration(Declaration::VariableDeclaration(
                "int".to_string(),
                "field".to_string(),
                None,
            ))],
            Some("TestAlias".to_string()),
        ));

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
        let lexer = Lexer::new("lonm _int_32_;");
        let mut parser = Parser::new(lexer);
        let _ = parser.parse_expr();
    }

    #[test]
    fn test_parse_assignment() {
        let code = "x = x + 5;";

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let assignment = parser.parse();

        let assignmentvec = vec![Instruction::Statement(Expression(
            BinOp(
                Box::new(Variable("x".to_string())),
                Op::Assign,
                Box::new(
                    BinOp(
                        Box::new(Variable("x".to_string())),
                        Op::Plus,
                        Box::new(Int(5))
                    )
                )
            )
        )
        )];
        assert_eq!(assignment.contains, assignmentvec);
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
    fn test_parse_type_specifiers() {
        let code = "unsigned long long x;";

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let declaration = parser.parse_declaration();
        let expected_declaration = Declaration::VariableDeclaration(
            "unsigned long long".to_string(),
            "x".to_string(),
            None,
        );

        assert_eq!(declaration, expected_declaration);
    }

    #[test]
    #[should_panic(expected = "Expected semicolon after variable declaration")]
    fn test_parse_type_specifiers_without_semicolon() {
        let code = "unsigned long long x";

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        parser.parse_declaration();
    }

    #[test]
    fn test_typedef_unsigned_long_long() {
        let code = "typedef unsigned long long BigNum;";

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        let declaration = parser.parse_declaration();
        let expected_declaration = TypeDefDeclaration(Basic(
            "unsigned long long".to_string(),
            "BigNum".to_string(),
        ));

        assert_eq!(declaration, expected_declaration);
    }

    #[test]
    #[should_panic(expected = "Expected alias in typedef")]
    fn test_typedef_without_alias() {
        let code = "typedef unsigned long long;";

        let lexer = Lexer::new(code);
        let mut parser = Parser::new(lexer);
        parser.parse_declaration();
    }
    #[test]
    fn test_parse_if_statement() {
        let input = r#"
        int main() {
            if (x < 5) {
                return 1;
            } else if (x > 10) {
                return 2;
            } else {
                return 0;
            }
        }
    "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        let contains = vec![Instruction::Declaration(FunctionDeclaration(
                "int".to_string(),
                "main".to_string(),
                vec![],
                vec![Instruction::Statement(
                    IfStatement(
                        Box::new(
                            BinOp(
                                Box::new(Variable("x".to_string())),
                                Op::Less,
                                Box::new(Int(5)))),

                        vec![Instruction::Statement(Return(Int(1)))],

                        Some(Box::new(Instruction::Statement(IfStatement(
                            Box::new(BinOp(
                                Box::new(Variable("x".to_string())),
                                Op::Greater,
                                Box::new(Int(10)))),

                            vec![Instruction::Statement(Return(Int(2)))],

                            Some(Box::new(Instruction::Statement(Statement::Else(vec![Instruction::Statement(Return(Int(0)))]))))
                        ),
                    )))
                    )
                )]
            ))];

        assert_eq!(program.contains, contains);

    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_if_statement() {
        let input = r#"
        int main() {
            if (x < 5) {
                return 1;
            else if (x > 10) { // Missing closing '}' for the first if statement
                return 2;
            } else {
                return 0;
            }
        }
    "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse();
    }

    #[test]
    fn test_parse_while_statement() {
        let input = r#"
    int main() {
        int x = 0;
        while (x < 10) {
            x = x + 1;
        }
    }
    "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();

        let contains = vec![Instruction::Declaration(FunctionDeclaration(
            "int".to_string(),
            "main".to_string(),
            vec![],
            vec![
                Instruction::Declaration(VariableDeclaration(
                    "int".to_string(),
                    "x".to_string(),
                    Some(Int(0))
                )),
                Instruction::Statement(
                    WhileLoop(
                        Box::new(
                            BinOp(
                                Box::new(Variable("x".to_string())),
                                Op::Less,
                                Box::new(Int(10))
                            )
                        ),
                        vec![Instruction::Statement(Expression(
                            BinOp(
                                Box::new(Variable("x".to_string())),
                                Op::Assign,
                                Box::new(
                                    BinOp(
                                        Box::new(Variable("x".to_string())),
                                        Op::Plus,
                                        Box::new(Int(1))
                                    )
                                )
                            )
                        ))]
                    )
                )
            ]
        ))];

        assert_eq!(program.contains, contains);
    }

    #[test]
    #[should_panic]
    fn test_parse_invalid_while_statement() {
        let input = r#"
    int main() {
        int x = 0;
        while (x < 10) {
            x = x + 1;
        }
    "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse();
    }
}
