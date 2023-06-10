use std::collections::HashMap;

#[allow(unused_imports)]
use crate::parser::{Expr, Op, Parameter, Program, Statement};

pub struct CodeGenerator {
    code: String,
    symbol_table: HashMap<String, isize>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            code: String::new(),
            symbol_table: HashMap::new(),
        }
    }
    pub fn generate(&mut self, program: &Program) -> &str {
        self.code += ".intel_syntax noprefix\n";
        self.code += ".global main\n";
        self.code += "main:\n";

        if let Some(Statement::FunctionDeclaration(name, _, _)) = program.statements.first() {
            self.code += &format!("call {}_entry\n", name)
        }
        self.code += "mov eax, 0x60\n";
        self.code += "xor edi, edi\n";
        self.code += "syscall\n";

        for statement in &program.statements {
            self.visit_statement(statement);
        }
        &self.code
    }

    fn visit_function_call(&mut self, name: &str, args: &[Expr]) {
        for (i, arg) in args.iter().rev().enumerate() {
            self.visit_expr(arg);
            match i {
                0 => self.code += "mov rdi, rax\n",
                1 => self.code += "mov rsi, rax\n",
                2 => self.code += "mov rdx, rax\n",
                3 => self.code += "mov rcx, rax\n",
                4 => self.code += "mov r8, rax\n",
                5 => self.code += "mov r9, rax\n",
                _ => {
                    // If there are more than 6 arguments, push them onto the stack.
                    self.code += "push rax\n";
                }
            }
        }
        self.code += &format!("call {}\n", name)
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::FunctionDeclaration(name, parameters, body) => {
                self.code += &format!("{}_entry:\n", name);
                self.code += "push rbp\n";
                self.code += "mov rbp, rsp\n";

                for (i, parameter) in parameters.iter().enumerate() {
                    let offset = ((i + 1) * 8) as isize;
                    // This example assumes that there are enough registers and uses them in order: rdi, rsi, rdx, rcx,

                    self.symbol_table.insert(parameter.name.clone(), -offset);
                    let register = match i {
                        0 => "rdi",
                        1 => "rsi",
                        2 => "rdx",
                        3 => "rcx",
                        4 => "r8",
                        5 => "r9",
                        _ => panic!("Too many parameters for right now"),
                    };
                    self.code += &format!("mov [rbp-{}], {}\n", (i + 1) * 8, register);
                }

                for statement in body {
                    self.visit_statement(statement);
                }

                self.code += "mov rsp, rbp\n";
                self.code += "pop rbp\n";
                self.code += "ret\n";
            }
            Statement::Return(expr) => {
                self.visit_expr(expr);
            }
            Statement::Expression(expr) => match expr {
                Expr::FunctionCall(name, args) => {
                    self.visit_function_call(name, args);
                }
                _ => self.visit_expr(expr),
            },
            _ => (),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Int(i) => self.code += &format!("mov rax, {}\n", i),
            Expr::Variable(var) => {
                if let Some(offset) = self.symbol_table.get(var) {
                    self.code += &format!("mov rax, [{}]\n", offset);
                } else {
                    let offset = -((self.symbol_table.len() + 1) as isize) * 8;
                    self.symbol_table.insert(var.clone(), offset);
                    self.code += &format!("mov rax, [{}]\n", offset);
                }
            }
            Expr::FunctionCall(name, args) => {
                for (i, arg) in args.iter().enumerate() {
                    self.visit_expr(arg);
                    self.code += &format!("mov [rsp-{}], rax\n", (i + 1) * 8);
                }
                self.code += &format!("call {}\n", name);
            }
            Expr::Negation(expr) => {
                self.visit_expr(expr);
                self.code += "neg rax\n";
            }
            Expr::Not(expr) => {
                self.visit_expr(expr);
                self.code += "not rax\n"
            }
            Expr::Dereference(expr) => {
                self.visit_expr(expr);
                self.code += "mov rax, [rax]\n";
            }
            Expr::Address(expr) => match &**expr {
                Expr::Variable(var) => self.code += &format!("lea rax, [{}]\n", var),
                _ => panic!("Can only take address of variables"),
            },
            Expr::BinOp(left, op, right) => {
                self.visit_expr(&*left);
                self.code += "push rax\n";

                self.visit_expr(&*right);
                self.code += "pop rcx\n";

                match op {
                    Op::Plus => self.code += "add rax, rcx\n",
                    Op::Minus => self.code += "sub rax, rcx\n",
                    Op::Multiplication => self.code += "imul rax, rcx\n",
                    //Op::Division => self.code += "idiv rcx\n",
                    Op::BitAND => self.code += "and rax, rcx\n",
                    Op::BitOR => self.code += "or rax, rcx\n",
                    Op::BitXOR => self.code += "xor rax, rcx\n",
                    _ => todo!("Need to add this binary operator {:?}", op),
                }
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generator_contains() {
        let mut codegen = CodeGenerator::new();
        let program = Program {
            statements: vec![
                Statement::FunctionDeclaration(
                    "adder".to_string(),
                    vec![
                        Parameter {
                            name: "a".to_string(),
                            typ: "int".to_string(),
                        },
                        Parameter {
                            name: "b".to_string(),
                            typ: "int".to_string(),
                        },
                    ],
                    vec![
                        Statement::VariableDeclaration(
                            "int".to_string(),
                            "c".to_string(),
                            Some(Expr::BinOp(
                                Box::new(Expr::Variable("a".to_string())),
                                Op::Plus,
                                Box::new(Expr::Variable("b".to_string())),
                            )),
                        ),
                        Statement::Return(Expr::BinOp(
                            Box::new(Expr::Variable("c".to_string())),
                            Op::Plus,
                            Box::new(Expr::Int(5)),
                        )),
                    ],
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
                                vec![Expr::Int(3), Expr::Int(4)],
                            )),
                        ),
                        Statement::Return(Expr::Int(0)),
                    ],
                ),
            ],
        };
        let asm = codegen.generate(&program);
    }
    #[test]
    fn test_assembly_return() {
        let program = Program {
            statements: vec![Statement::FunctionDeclaration(
                "test".to_string(),
                vec![],
                vec![Statement::Return(Expr::Int(42))],
            )],
        };
        let mut generator = CodeGenerator::new();
        let asm = generator.generate(&program);
        assert_eq!(
            asm,
            ".intel_syntax noprefix\n.global main\nmain:\ncall test_entry\nmov eax, 0x60\nxor edi, edi\nsyscall\n\
            test_entry:\npush rbp\nmov rbp, rsp\nmov rax, 42\nmov rsp, rbp\npop rbp\nret\n"
        );
    }
}
