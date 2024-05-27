
use std::io::{self, Write};

use crate::{ast::{AstPrgPart, AstStatement}, callstack::CallStack, parser::SprocketParser, semantics::SemanticAnalyzer, sprocket::{SprocketError, SprocketResult}, symbol::SymbolKind};

pub struct SprocketRepl {
    parser: SprocketParser,
    callstack: CallStack,
}

impl SprocketRepl {
    pub fn new() -> Self {
        let mut callstack = CallStack::new();
        callstack.push(None);
        Self {
            parser: SprocketParser::new(),
            callstack
        }
    }

    pub fn run(mut self) -> SprocketResult<()> {
        let mut buf = String::new();
        let analyzer = SemanticAnalyzer::new();
        loop {
            match self.update(&mut buf, &analyzer) {
                Ok(true) => {
                    break
                },
                Ok(false) => {}
                Err(err) => {
                    println!("Error: {}", err);
                    buf.clear();
                }
            }
        }
        Ok(())
    }

    fn update(&mut self, buf: &mut String, analyzer:&SemanticAnalyzer) -> SprocketResult<bool> {
        let mut stdout = io::stdout();
        if buf.len() > 0 {
            stdout.write_all(b"..>").unwrap();
        }
        else {
            stdout.write_all(b"\xE2\x9A\x99 > ").unwrap();
        }
        stdout.flush().unwrap();

        io::stdin().read_line(buf).unwrap();
        *buf = buf.trim().to_string();
        if buf == "exit" {
            return Ok(true)
        }
        match self.parser.parse(&buf) {
            Ok(ast) => {
                analyzer.analyze(&ast, &mut self.callstack)?;
                for ast_node in ast {
                    self.callstack.exe_prg_part(&ast_node)?;
                    if let AstPrgPart::Statement(AstStatement::ExprStatement(expr)) = ast_node {
                        let val = self.callstack.val_from_expr(&expr, true)?;
                        println!("{}", val);
                    }
                }
            }
            Err(SprocketError::UnexpectedEOF) => {
                buf.push(' ');
                return Ok(false)
            }
            Err(err) => {
                println!("{:?}", err);
            }
        }
        buf.clear();
        Ok(false)
    }
}
