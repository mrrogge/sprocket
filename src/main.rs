use cli::CliResult;

mod ast;
mod cli;
mod interpretter;
mod lexer;
mod parser;
mod semantics;
mod symbol;
mod token;
mod callstack;

fn main() -> CliResult<()> {
    cli::Cli::run()
}
