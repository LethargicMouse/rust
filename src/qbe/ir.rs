use std::fmt::Display;

pub struct IR {
    pub stmts: Vec<Stmt>,
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "export function w $main() {{\n@start")?;
        for stmt in &self.stmts {
            write!(f, "\n  {stmt}")?;
        }
        write!(f, "\n}}")
    }
}

pub type Tmp = u32;

pub enum Stmt {
    Ret(Tmp),
    Copy(Tmp, i32),
    Call(Tmp, String, Tmp),
    Bin(Tmp, BinOp, Tmp, Tmp),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Ret(t) => write!(f, "ret %t{t}"),
            Stmt::Copy(t, n) => write!(f, "%t{t} =w copy {n}"),
            Stmt::Call(t, n, a) => write!(f, "%t{t} =w call ${n}(w %t{a})"),
            Stmt::Bin(t, bin_op, l, r) => write!(f, "%t{t} =w {bin_op} %t{l}, %t{r}"),
        }
    }
}

pub enum BinOp {
    Add,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
        }
    }
}
