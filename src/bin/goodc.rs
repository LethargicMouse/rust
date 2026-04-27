use std::{
    env::{self, args},
    fmt::Display,
    fs::{create_dir, exists},
};

use russ::{
    die::{Mortal, die},
    display::colors::{Blue, Red, Reset},
    file::{self, create},
    good::{analyse, generate, lex, parse},
    process::{self, call},
    qbe::ir::IR,
    source::{Source, read_source},
};

fn main() {
    let args = get_args();
    match args.command {
        Command::Run(path) => run(path, args.options),
        Command::Clean => clean(),
    }
}

fn clean() {
    call("rm", &[OUT, OUT_ASM, OUT_IR]).unwrap_or(());
}

#[derive(Default)]
struct Options {
    compile_options: CompileOptions,
    rest: Vec<String>,
}

fn run(path: String, options: Options) {
    compile(path, options.compile_options);
    run_out(options.rest);
}

#[derive(Default)]
struct CompileOptions {
    cc_args: Vec<String>,
    debug: bool,
}

fn compile(path: String, options: CompileOptions) {
    let source = read_source(path);
    let ir = process(source, options.debug);
    create_build_dir();
    dump(ir);
    postcompile(options.cc_args);
}

fn create_build_dir() {
    if !exists(".build/").unwrap() {
        create_dir(".build/").or_die_with(|e| create::Error(".build/", e))
    }
}

fn get_args() -> Args {
    let mut args = args();
    args.next();
    let command = get_command(&mut args);
    let mut options = Options::default();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--" => break,
            "-cc" => {
                for arg in args.by_ref() {
                    if &arg == "cc-" {
                        break;
                    }
                    options.compile_options.cc_args.push(arg);
                }
            }
            "-debug" => options.compile_options.debug = true,
            _ => die(Unexpected("argument", arg)),
        }
    }
    options.rest = args.collect();
    Args { command, options }
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
        write!(f, "{ArgsError} unexpected {}:{Reset} `{}`", self.0, self.1)
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
    options: Options,
}

enum Command {
    Run(String),
    Clean,
}

fn postcompile(cc_args: Vec<String>) {
    call("qbe", &["-o", OUT_ASM, OUT_IR]).or_die();
    let mut args = vec!["-g".into(), "-o".into(), OUT.into(), OUT_ASM.into()];
    args.extend(cc_args);
    call("cc", &args).or_die();
}

fn run_out(args: Vec<String>) {
    process::run(OUT, args);
}

fn process(source: Source, debug: bool) -> IR {
    let tokens = lex(&source);
    let ast = parse(tokens);
    let asg = analyse(ast, debug);
    generate(&asg, debug)
}

fn dump(ir: IR) {
    file::dump(ir, OUT_IR);
}

const OUT_IR: &str = ".build/out.qbe";
const OUT: &str = "./.build/out";
const OUT_ASM: &str = ".build/out.s";
