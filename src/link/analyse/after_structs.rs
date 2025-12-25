use std::collections::HashMap;

use crate::{
    Location,
    link::{
        Asg,
        analyse::{
            State, Struct, Typed,
            error::{CheckError, Fail, NoCall, NoField, NoIndex, NotDeclared},
            typed,
        },
        asg,
        ast::{
            Ast, BinOp, Binary, Block, Call, Expr, Field, Fun, Get, If, Let, Literal, Type, Var,
        },
    },
    mem::{map_box, update},
};

pub struct Analyse<'a, 'b> {
    structs: &'b HashMap<&'a str, Struct<'a>>,
    sup: &'b mut State<'a>,
}

impl<'a, 'b> Analyse<'a, 'b> {
    pub fn new(sup: &'b mut super::Analyse<'a>) -> Self {
        Self {
            structs: &sup.structs,
            sup: &mut sup.sup,
        }
    }

    pub fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        for extrn in ast.externs {
            self.sup.context.insert(extrn.name, extrn.typ);
        }
        for fun in &ast.funs {
            self.sup
                .context
                .insert(fun.header.name, fun.header.typ.clone().into());
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|fun| (fun.header.name, self.fun(fun)))
            .collect();
        Asg { funs }
    }

    fn expr(&mut self, expr: Expr<'a>) -> Typed<'a, asg::Expr<'a>> {
        match expr {
            Expr::Call(call) => self.call(call).map_into(),
            Expr::Binary(binary) => self.binary(*binary).map_into(),
            Expr::Literal(literal, _) => self.literal(literal).map_into(),
            Expr::Var(name) => self.var(name).map_into(),
            Expr::If(if_expr) => self.if_expr(*if_expr).map_into(),
            Expr::Get(get) => self.get(*get),
            Expr::Block(block) => self.block(*block).map_into(),
            Expr::Let(let_expr) => self.let_expr(*let_expr).map_into(),
            Expr::Field(field) => match self.field(*field) {
                Ok(f) => f.map_into(),
                Err(_) => self.fake_expr(),
            },
        }
    }

    fn fake_expr(&self) -> Typed<'a, asg::Expr<'a>> {
        typed(asg::Literal::Int(0).into(), Type::Error)
    }

    fn field(&mut self, field: Field<'a>) -> Result<Typed<'a, asg::Field<'a>>, Fail> {
        let (from, typ) = self.expr(field.from).into();
        if matches!(typ, Type::Error) {
            return Err(Fail);
        }
        let name = typ.name().ok_or_else(|| {
            self.fail(NoField {
                location: field.name_location,
                name: field.name,
                typ: typ.clone(),
            })
        })?;
        let r#struct = self.structs.get(name).unwrap();
        let field = r#struct.fields.get(field.name).ok_or_else(|| {
            self.sup.errors.push(
                NoField {
                    location: field.name_location,
                    name: field.name,
                    typ,
                }
                .into(),
            );
            Fail
        })?;
        let offset = field.offset;
        let typ = field.typ.clone();
        Ok(Typed {
            sup: asg::Field { from, offset },
            typ,
        })
    }

    fn fail(&mut self, error: impl Into<CheckError<'a>>) -> Fail {
        self.sup.errors.push(error.into());
        Fail
    }

    fn let_expr(&mut self, let_expr: Let<'a>) -> Typed<'a, asg::Let<'a>> {
        let name = let_expr.name;
        let Typed { sup: expr, typ } = self.expr(let_expr.expr);
        self.sup.context.insert(name, typ);
        Typed {
            sup: asg::Let { name, expr },
            typ: Type::Unit,
        }
    }

    fn block(&mut self, block: Block<'a>) -> Typed<'a, asg::Block<'a>> {
        let stmts: Vec<_> = block.stmts.into_iter().map(|e| self.expr(e).sup).collect();
        let Typed { sup: ret, typ } = self.expr(block.ret);
        Typed {
            sup: asg::Block { stmts, ret },
            typ,
        }
    }

    fn get(&mut self, get: Get<'a>) -> Typed<'a, asg::Expr<'a>> {
        let location = get.from.location();
        let from = self.expr(get.from);
        let typ = self.index_type(from.typ, location);
        let index = self.expr(get.index);
        Typed {
            sup: asg::Expr::Deref(Box::new(
                asg::Binary {
                    left: from.sup,
                    op: asg::BinOp::Add,
                    right: asg::Binary {
                        left: index.sup,
                        op: asg::BinOp::Multiply,
                        right: asg::Literal::Int(8).into(),
                    }
                    .into(),
                }
                .into(),
            )),
            typ,
        }
    }

    fn index_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Ptr(t) => *t,
            Type::Error => Type::Error,
            _ => {
                self.sup.errors.push(NoIndex { location, typ }.into());
                Type::Error
            }
        }
    }

    fn if_expr(&mut self, if_expr: If<'a>) -> Typed<'a, asg::If<'a>> {
        let condition = self.expr(if_expr.condition).sup;
        let Typed {
            sup: then_expr,
            typ,
        } = self.expr(if_expr.then_expr);
        let else_expr = self.expr(if_expr.else_expr).sup;
        Typed {
            sup: asg::If {
                condition,
                then_expr,
                else_expr,
            },
            typ,
        }
    }

    fn call(&mut self, call: Call<'a>) -> Typed<'a, asg::Call<'a>> {
        let name = call.var.name;
        let location = call.var.location;
        let fun_typ = self.var(call.var).typ;
        let typ = self.ret_type(fun_typ, location);
        let args = call.args.into_iter().map(|e| self.expr(e).sup).collect();
        Typed {
            sup: asg::Call { name, args },
            typ,
        }
    }

    fn ret_type(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Fun(fun_type) => fun_type.ret_type,
            Type::Error => Type::Error,
            _ => {
                self.sup.errors.push(NoCall { location, typ }.into());
                Type::Error
            }
        }
    }

    fn var(&mut self, var: Var<'a>) -> Typed<'a, &'a str> {
        match self.sup.context.get(var.name) {
            Some(typ) => Typed {
                sup: var.name,
                typ: typ.clone(),
            },
            None => {
                self.sup.errors.push(
                    NotDeclared {
                        location: var.location,
                        kind: "item",
                        name: var.name,
                    }
                    .into(),
                );
                Typed {
                    sup: var.name,
                    typ: Type::Error,
                }
            }
        }
    }

    fn binary(&mut self, binary: Binary<'a>) -> Typed<'a, asg::Binary<'a>> {
        let Typed { sup: left, typ } = self.expr(binary.left);
        let right = self.expr(binary.right).sup;
        let typ = match binary.op {
            BinOp::Plus => typ,
            BinOp::Equal => Type::Name("bool"),
            BinOp::Less => Type::Name("bool"),
        };
        let op = self.bin_op(binary.op);
        Typed {
            sup: asg::Binary { left, op, right },
            typ,
        }
    }

    fn bin_op(&self, bin_op: BinOp) -> asg::BinOp {
        match bin_op {
            BinOp::Plus => asg::BinOp::Add,
            BinOp::Equal => asg::BinOp::Equal,
            BinOp::Less => asg::BinOp::Less,
        }
    }

    fn literal(&self, literal: Literal<'a>) -> Typed<'a, asg::Literal<'a>> {
        match literal {
            Literal::Unit => Typed {
                sup: asg::Literal::Int(0),
                typ: Type::Unit,
            },
            Literal::Int(i) => Typed {
                sup: asg::Literal::Int(i),
                typ: Type::Name("i32"),
            },
            Literal::RawStr(s) => Typed {
                sup: asg::Literal::Str(s),
                typ: Type::Ptr(Box::new(Type::Name("u8"))),
            },
        }
    }

    fn r#struct(&mut self, name: &'a str, location: Location<'a>) -> Type<'a> {
        if self.structs.contains_key(name) {
            Type::Name(name)
        } else if self.sup.corrupt.contains(name) {
            Type::Error
        } else {
            self.sup.corrupt.insert(name);
            self.sup.errors.push(
                NotDeclared {
                    location,
                    kind: "struct",
                    name,
                }
                .into(),
            );
            Type::Error
        }
    }

    fn typ(&mut self, typ: Type<'a>, location: Location<'a>) -> Type<'a> {
        match typ {
            Type::Ptr(t) => Type::Ptr(map_box(t, |t| self.typ(t, location))),
            Type::Name(name) => self.r#struct(name, location),
            Type::Fun(mut fun_type) => {
                for typ in &mut fun_type.params {
                    update(typ, |t| self.typ(t, location))
                }
                fun_type.ret_type = self.typ(fun_type.ret_type, location);
                Type::Fun(fun_type)
            }
            Type::Unit => todo!(),
            Type::Error => todo!(),
        }
    }

    fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        self.sup.context.new_layer();
        for ((param, typ), location) in fun
            .header
            .params
            .iter()
            .zip(fun.header.typ.params)
            .zip(fun.header.type_params_locations)
        {
            let typ = self.typ(typ, location);
            self.sup.context.insert(param, typ);
        }
        let params = fun.header.params;
        let body = self.expr(fun.body).sup;
        asg::Fun { params, body }
    }
}
