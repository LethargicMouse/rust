use std::{
    env::{self, args},
    fmt::Display,
};

use russ::{
    die::{Mortal, die},
    display::colors::{Blue, Red, Reset},
    file::{self},
    link::{analyse, generate, lex, parse},
    process::{self, call},
    qbe::ir::IR,
    source::{Source, read_source},
};

fn main() {
    let args = get_args();
    match args.command {
        Command::Run(path) => run(path, args.rest),
        Command::Clean => clean(),
    }
}

fn clean() {
    call("rm", &[OUT, OUT_ASM, OUT_IR]).unwrap_or(());
}

fn run(path: String, args: env::Args) {
    compile(path);
    run_out(args);
}

fn compile(path: String) {
    let source = read_source(path);
    let ir = process(source);
    dump(ir);
    postcompile();
}

fn get_args() -> Args {
    let mut args = args();
    args.next();
    let command = get_command(&mut args);
    let rest = args;
    Args { command, rest }
}

fn get_command(args: &mut env::Args) -> Command {
    match args.next() {
        Some(command) => match command.as_str() {
            "run" => Command::Run(get_path(args)),
            "clean" => Command::Clean,
            _ => die(Unexpected("command", command)),
        },
        None => die(Expected("command")),
    }
}

fn get_path(args: &mut env::Args) -> String {
    args.next().or_die_with(|_| Expected("path"))
}

struct Expected(&'static str);

impl Display for Expected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{ArgsError} expected {Reset}{}", self.0)
    }
}

struct Unexpected(&'static str, String);

impl Display for Unexpected {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{ArgsError} unexpected {Reset}{}{Red}:{Reset} `{}`",
            self.0, self.1
        )
    }
}

struct ArgsError;

impl Display for ArgsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{Red}! error reading {Blue}args{Red}:")
    }
}

struct Args {
    command: Command,
    rest: env::Args,
}

enum Command {
    Run(String),
    Clean,
}

fn postcompile() {
    call("qbe", &["-o", OUT_ASM, OUT_IR]).or_die();
    call("cc", &["-o", OUT, OUT_ASM]).or_die();
}

fn run_out(args: env::Args) {
    process::run(OUT, args);
}

fn process(source: Source) -> IR {
    let tokens = lex(&source);
    let ast = parse(tokens);
    let asg = analyse(ast);
    generate(&asg)
}

fn dump(ir: IR) {
    file::dump(ir, OUT_IR);
}

const OUT_IR: &str = "out.qbe";
const OUT: &str = "./out";
const OUT_ASM: &str = "out.s";
