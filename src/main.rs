use sprocket::SprocketResult;

mod ast;
mod callstack;
mod cli;
mod interpretter;
mod lexer;
mod parser;
mod repl;
mod semantics;
mod sprocket;
mod symbol;
mod token;

fn main() -> SprocketResult<()> {
    cli::Cli::run()
}
