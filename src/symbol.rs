use core::fmt;
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

impl fmt::Display for SymbolKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolKind::Var(type_) => write!(f, "tag:{}", type_),
            SymbolKind::Type(type_) => write!(f, "type:{}", type_),
            SymbolKind::FunctionDef(decl) => write!(f, "fnDef:{}", decl.id),
            SymbolKind::Task(_) => write!(f, "task"),
        }
    }
}
