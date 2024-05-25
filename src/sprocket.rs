use std::fmt;

use crate::{ast::SpkType, symbol::SymbolKind, token::Token};

#[derive(Clone, Debug)]
pub enum SprocketError {
    UnexpectedToken(Token, Option<Token>),
    UnexpectedEOF,
    DupArgForNamedParam(String),
    // duplicate tag declaration
    DupTagDecl(String),
    // reference to an undefined type
    TypeUndef(String),
    // expected a type reference
    ExpectedType(String),
    // expected a var reference
    ExpectedVar(String),
    // expected an expression but got a type reference
    ExpectedExprGotType(String),
    // reference to an undeclared var
    VarNotDecl(String),
    // expected an expr of type _, but got type _
    ExpectedExprTypeOf(SpkType, SpkType),
    // invalid type
    InvalidType(SpkType),
    // tag initialization is required for type _
    TagInitReq(SpkType),
    // type cannot be dereferenced
    DerefInvalid(SpkType),
    // Return statements not allowed outside of function declarations
    RetStmtNotInFnDecl,
    // reference to an undeclared function
    FnNotDecl(String),
    // symbol refers to uncallable object
    SymbolNotCallable(String),
    SymbolNotDefined(String),
    SymbolNotAVar(String, SymbolKind),
    SymbolMissingVal(String),
    EmptyCallStack,
    UndefFunction(String),
    RuntimeTypeError,
}

impl fmt::Display for SprocketError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DerefInvalid(type_) => write!(f, "Cannot dereference type {}", type_),
            Self::UnexpectedToken(unexpected, Some(expected)) => write!(f, "Unexpected token {}; expected {}", unexpected, expected),
            Self::UnexpectedToken(unexpected, None) => write!(f, "unexpected token: {}", unexpected),
            Self::UnexpectedEOF => write!(f, "unexpected end-of-file"),
            Self::DupArgForNamedParam(name) => write!(f, "duplicate argument for named parameter: {}", name),
            Self::DupTagDecl(id) => write!(f, "duplicate tag declaration: {}", id),
            Self::TypeUndef(id) => write!(f, "undefined type: {}", id),
            Self::ExpectedType(_) => todo!(),
            Self::ExpectedVar(_) => todo!(),
            Self::ExpectedExprGotType(_) => todo!(),
            Self::VarNotDecl(_) => todo!(),
            Self::ExpectedExprTypeOf(_, _) => todo!(),
            Self::InvalidType(_) => todo!(),
            Self::TagInitReq(_) => todo!(),
            Self::RetStmtNotInFnDecl => todo!(),
            Self::FnNotDecl(_) => todo!(),
            Self::SymbolNotCallable(_) => todo!(),
            Self::SymbolNotDefined(_) => todo!(),
            Self::SymbolNotAVar(_, _) => todo!(),
            Self::SymbolMissingVal(_) => todo!(),
            Self::EmptyCallStack => todo!(),
            Self::UndefFunction(_) => todo!(),
            Self::RuntimeTypeError => todo!(),
        }
    }
}

pub type SprocketResult<T> = Result<T, SprocketError>;