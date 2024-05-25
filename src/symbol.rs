use std::collections::HashMap;

use crate::ast::{AstFnDecl, SpkType, AST};

pub type SymbolTable = HashMap<String, SymbolKind>;

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Var(SpkType),
    Type(SpkType),
    FunctionDef(AstFnDecl),
    Task(AST),
}
