use crate::{ast::AST, callstack::CallStack, parser::SprocketParser, semantics::SemanticAnalyzer, sprocket::SprocketResult};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn parse(source:&str) -> String {
    match parse_inner(source) {
        Ok(ast) => format!("OK: {}", ast), 
        Err(err) => format!("ERROR: {:?}", err)
    }
}

fn parse_inner(source:&str) -> SprocketResult<String> {
    let mut parser = SprocketParser::new();
    let ast = parser.parse(source)?;
    let analyzer = SemanticAnalyzer::new();
    let mut callstack = CallStack::new();
    callstack.push(None);
    callstack.load_globals()?;
    analyzer.analyze(&ast, &mut callstack)?;
    Ok(format!("{:?}", ast))
}

