use std::fmt::Display;

pub struct IR {
    pub consts: Vec<String>,
    pub funs: Vec<Fun>,
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, c) in self.consts.iter().enumerate() {
            writeln!(f, "\ndata $s{} = {{ b \"{c}\" 0 }}", i + 1)?;
        }
        for fun in &self.funs {
            write!(f, "{fun}")?;
        }
        Ok(())
    }
}

pub struct Fun {
    pub name: String,
    pub params: Vec<Tmp>,
    pub stmts: Vec<Stmt>,
}

impl Display for Fun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "export function w ${}(", self.name)?;
        for param in &self.params {
            write!(f, "w %t{param},")?;
        }
        write!(f, ") {{\n@start")?;
        for stmt in &self.stmts {
            write!(f, "\n  {stmt}")?;
        }
        write!(f, "\n}}")
    }
}

pub type Tmp = u32;

pub enum Stmt {
    Ret(Tmp),
    Copy(Tmp, Value),
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

pub enum Value {
    Int(i32),
    Const(u16),
}

impl From<i32> for Value {
    fn from(v: i32) -> Self {
        Self::Int(v)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Const(n) => write!(f, "$s{n}"),
        }
    }
}
