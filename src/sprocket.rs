use std::{error::Error, fmt};

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
    MiscError,
}

impl fmt::Display for SprocketError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DerefInvalid(type_) => write!(f, "Cannot dereference type {}", type_),
            Self::UnexpectedToken(unexpected, Some(expected)) => {
                write!(f, "Unexpected token {}; expected {}", unexpected, expected)
            }
            Self::UnexpectedToken(unexpected, None) => {
                write!(f, "unexpected token: {}", unexpected)
            }
            Self::UnexpectedEOF => write!(f, "unexpected end-of-file"),
            Self::DupArgForNamedParam(name) => {
                write!(f, "duplicate argument for named parameter: {}", name)
            }
            Self::DupTagDecl(id) => write!(f, "duplicate tag declaration: {}", id),
            Self::TypeUndef(id) => write!(f, "undefined type: {}", id),
            Self::ExpectedType(id) => write!(f, "expected a type but got {}", id),
            Self::ExpectedVar(id) => write!(f, "expected a variable but got {}", id),
            Self::ExpectedExprGotType(id) => {
                write!(f, "expected an expression but got type {}", id)
            }
            Self::VarNotDecl(id) => write!(f, "variable {} not declared", id),
            Self::ExpectedExprTypeOf(expected, unexpected) => write!(
                f,
                "expected an expression of type {} but got {}",
                expected, unexpected
            ),
            Self::InvalidType(type_) => write!(f, "invalid type: {}", type_),
            Self::TagInitReq(type_) => write!(f, "initial value required for tag type {}", type_),
            Self::RetStmtNotInFnDecl => write!(
                f,
                "Return statement not allowed outside of function declaration"
            ),
            Self::FnNotDecl(id) => write!(f, "function {} not declared", id),
            Self::SymbolNotCallable(id) => write!(f, "{} is not callable", id),
            Self::SymbolNotDefined(id) => write!(f, "{} is not defined", id),
            Self::SymbolNotAVar(id, kind) => {
                write!(f, "symbol {} is a {} but should be a variable", id, kind)
            }
            Self::SymbolMissingVal(id) => write!(f, "symbol {} does not have a value", id),
            Self::EmptyCallStack => write!(f, "callstack is empty"),
            Self::UndefFunction(id) => write!(f, "function {} is undefined", id),
            Self::RuntimeTypeError => write!(f, "unexpected type error at runtime"),
            Self::MiscError => write!(f, "unknown error"),
        }
    }
}

impl Error for SprocketError {}

pub type SprocketResult<T> = Result<T, SprocketError>;
