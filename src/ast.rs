use core::fmt;
use std::collections::HashMap;

pub type AST = Vec<AstPrgPart>;

#[derive(Clone, Debug)]
pub enum AstPrgPart {
    TagDecl(AstTagDecl),
    FnDecl(AstFnDecl),
    Statement(AstStatement),
    Comment(String),
}

#[derive(Clone, Debug)]
pub struct AstTagDecl {
    pub id: String,
    pub type_: SpkType,
    pub expr: Option<AstExpr>,
    pub is_global: bool,
}

#[derive(Clone, Debug)]
pub enum AstStatement {
    TableStatement(Vec<Vec<TableCell>>),
    AssignStatement { target_id: String, expr: AstExpr },
    ExprStatement(AstExpr),
    ReturnStatement(Option<AstExpr>),
    StmtBlock(Vec<AstPrgPart>),
}

#[derive(Clone, Debug)]
pub enum AstExpr {
    IdExpr(String),
    BinopExpr {
        left: Box<AstExpr>,
        op: AstBinop,
        right: Box<AstExpr>,
    },
    UnopExpr(AstUnop, Box<AstExpr>),
    BoolLiteralExpr(bool),
    IntLiteralExpr(i32),
    CallExpr {
        id: String,
        pos_args: Vec<AstExpr>,
        named_args: HashMap<String, AstExpr>,
    },
}

#[derive(Clone, Debug)]
pub enum TableCell {
    Expr(AstExpr),
    Empty,
    Continued,
}

#[derive(Clone, Debug)]
pub enum AstBinop {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    And,
    Or,
    XOr,
    Exponent,
    Mod,
}

impl AstBinop {
    pub fn priority(&self) -> u32 {
        match self {
            AstBinop::And => 1,
            AstBinop::Divide => 2,
            AstBinop::Equal => 1,
            AstBinop::Exponent => 1,
            AstBinop::GreaterThan => 1,
            AstBinop::GreaterThanOrEqual => 1,
            AstBinop::LessThan => 1,
            AstBinop::LessThanOrEqual => 1,
            AstBinop::Minus => 1,
            AstBinop::Mod => 1,
            AstBinop::Multiply => 2,
            AstBinop::NotEqual => 1,
            AstBinop::Or => 1,
            AstBinop::Plus => 1,
            AstBinop::XOr => 1,
        }
    }
}

#[derive(Clone, Debug)]
pub enum AstUnop {
    Not,
    Ref,
    Deref,
}

#[derive(Clone, Debug)]
pub struct AstFnDecl {
    pub id: String,
    pub params: Vec<AstParamDecl>,
    pub ret_type: SpkType,
    pub body: Vec<AstPrgPart>,
}

#[derive(Clone, Debug)]
pub struct AstParamDecl {
    pub id: String,
    pub type_: SpkType,
    pub kind: AstParamKind,
}

#[derive(Clone, Debug)]
pub enum AstParamKind {
    In,
    Out,
    InOut,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpkType {
    Bool,
    Int32,
    Ref(Box<SpkType>),
    Unresolved(String),
    Void,
}

impl fmt::Display for SpkType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SpkType::Bool => write!(f, "Bool"),
            SpkType::Int32 => write!(f, "i32"),
            SpkType::Ref(type_) => write!(f, "&{}", type_),
            SpkType::Unresolved(id) => write!(f, "unresolved({})", id),
            SpkType::Void => write!(f, "void"),
        }
    }
}
