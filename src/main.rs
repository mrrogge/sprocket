use sprocket::SprocketResult;

mod ast;
mod callstack;
mod cli;
mod interpretter;
mod lexer;
mod parser;
mod semantics;
mod sprocket;
mod symbol;
mod token;
mod repl;

fn main() -> SprocketResult<()> {
    cli::Cli::run()
}
