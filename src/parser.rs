#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Int(i64),
    BinOp(Box<Expr>, Op, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}