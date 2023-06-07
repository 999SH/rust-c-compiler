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
                self.code += "push ebp\n";
                self.code += "mov ebp, esp\n";

                for parameter in parameters {}

                for statements in body {
                    self.visit_statement(statement);
                }

                self.code += "mov esp, ebp\n";
                self.code += "pop ebp\n";
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
            Expr::Int(i) => self.code += &format!("mov eax, {}\n", i),
            Expr::BinOp(left, op, right) => {
                self.visit_expr(&*left);
                self.code += "push eax\n";

                self.visit_expr(&*right);
                self.code += "pop ecx\n";

                match op {
                    Op::Plus => self.code += "add eax, ecx\n",
                    _ => (),
                }
            }
            _ => (),
        }
    }
}
