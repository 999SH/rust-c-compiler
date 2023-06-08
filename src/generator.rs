use crate::parser::{Expr, Op, Parameter, Program, Statement};

pub struct CodeGenerator {
    code: String,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self {
            code: String::new(),
        }
    }
    pub fn generate(&mut self, program: &Program) -> &str {
        for statement in &program.statements {
            self.visit_statement(statement);
        }
        &self.code
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::FunctionDeclaration(name, parameters, body) => {
                self.code += &format!("{}_entry:\n", name);
                self.code += "push rbp\n";
                self.code += "mov rbp, rsp\n";

                for (i, _parameter) in parameters.iter().enumerate() {
                    // This example assumes that there are enough registers and uses them in order: rdi, rsi, rdx, rcx, r8, r9.
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
            _ => (),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Int(i) => self.code += &format!("mov rax, {}\n", i),
            Expr::BinOp(left, op, right) => {
                self.visit_expr(&*left);
                self.code += "push rax\n";

                self.visit_expr(&*right);
                self.code += "pop rcx\n";

                match op {
                    Op::Plus => self.code += "add rax, rcx\n",
                    _ => (),
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
    fn test_simple_function() {
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

        assert!(asm.contains("push rbp"));
        assert!(asm.contains("mov rbp, rsp"));
        assert!(asm.contains("pop rbp"));
        assert!(asm.contains("ret"));
    }
}
