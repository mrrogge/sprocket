use crate::{
    ast::{AstBinop, AstExpr, AstPrgPart, AstStatement, AstUnop, SpkType, TableCell, AST},
    callstack::CallStack,
    sprocket::{SprocketError, SprocketResult},
    symbol::SymbolKind,
};

pub struct SemanticAnalyzer {}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {}
    }

    pub fn analyze(&self, ast: &AST, callstack: &mut CallStack) -> SprocketResult<()> {
        for part in ast {
            match &part {
                AstPrgPart::TagDecl(decl) => {
                    if !decl.is_global {
                        todo!()
                    }
                    match callstack.lookup_symbol_kind(&decl.id) {
                        Some(_) => return Err(SprocketError::DupTagDecl(decl.id.clone())),
                        None => {}
                    }
                    let type_ = Self::eval_typeref(&decl.type_, callstack)?;
                    callstack.insert_symbol(&decl.id, SymbolKind::Var(type_.clone()))?;
                    match &decl.expr {
                        Some(expr) => {
                            let expr_type = Self::eval_expr_type(expr, callstack)?;
                            if type_ != expr_type {
                                return Err(SprocketError::ExpectedExprTypeOf(type_, expr_type));
                            }
                        }
                        None => match type_ {
                            SpkType::Ref(_) => return Err(SprocketError::TagInitReq(type_)),
                            _ => {}
                        },
                    }
                }
                AstPrgPart::Comment(_) => {}
                AstPrgPart::FnDecl(decl) => {
                    callstack.insert_symbol(&decl.id, SymbolKind::FunctionDef(decl.clone()))?;
                }
                AstPrgPart::Statement(AstStatement::AssignStatement { target_id, expr }) => {
                    let var_type = match callstack.lookup_symbol_kind(target_id) {
                        Some(SymbolKind::Var(var_type)) => var_type.clone(),
                        Some(_) => return Err(SprocketError::ExpectedVar(target_id.clone())),
                        None => return Err(SprocketError::VarNotDecl(target_id.clone())),
                    };
                    let expr_type = Self::eval_expr_type(&expr, callstack)?;
                    if var_type != expr_type {
                        return Err(SprocketError::ExpectedExprTypeOf(var_type, expr_type));
                    }
                }
                AstPrgPart::Statement(AstStatement::ExprStatement(expr)) => {
                    Self::eval_expr_type(&expr, callstack)?;
                }
                AstPrgPart::Statement(AstStatement::TableStatement(rows)) => {
                    for row in rows {
                        for cell in row {
                            match cell {
                                TableCell::Empty | TableCell::Continued => continue,
                                TableCell::Expr(expr) => {
                                    match Self::eval_expr_type(&expr, callstack)? {
                                        SpkType::Bool => {}
                                        type_ => {
                                            return Err(SprocketError::ExpectedExprTypeOf(
                                                SpkType::Bool,
                                                type_,
                                            ))
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                AstPrgPart::Statement(AstStatement::ReturnStatement(_)) => {
                    return Err(SprocketError::RetStmtNotInFnDecl)
                }
                AstPrgPart::Statement(AstStatement::StmtBlock(block)) => {
                    callstack.push(None);
                    self.analyze(block, callstack)?;
                    callstack.pop();
                }
            }
        }
        Ok(())
    }

    pub fn eval_expr_type(expr: &AstExpr, callstack: &CallStack) -> SprocketResult<SpkType> {
        match expr {
            AstExpr::BoolLiteralExpr(_) => Ok(SpkType::Bool),
            AstExpr::IntLiteralExpr(_) => Ok(SpkType::Int32),
            AstExpr::IdExpr(id) => {
                let type_ = match callstack.lookup_symbol_kind(id) {
                    Some(SymbolKind::Var(type_)) => type_,
                    Some(SymbolKind::Type(_)) => {
                        return Err(SprocketError::ExpectedExprGotType(id.clone()))
                    }
                    Some(SymbolKind::FunctionDef(_)) => {
                        todo!()
                    }
                    Some(SymbolKind::Task(_)) => {
                        todo!()
                    }
                    None => return Err(SprocketError::VarNotDecl(id.clone())),
                };
                Ok(type_.clone())
            }
            AstExpr::UnopExpr(op, inner_expr) => {
                let inner_type = Self::eval_expr_type(inner_expr, callstack)?;
                match (op, inner_type) {
                    (AstUnop::Not, SpkType::Bool) => Ok(SpkType::Bool),
                    (AstUnop::Not, inner_type) => Err(SprocketError::ExpectedExprTypeOf(
                        SpkType::Bool,
                        inner_type.clone(),
                    )),
                    (AstUnop::Ref, inner_type) => Ok(SpkType::Ref(Box::new(inner_type))),
                    (AstUnop::Deref, SpkType::Ref(inner_type)) => Ok(*inner_type),
                    (AstUnop::Deref, inner_type) => Err(SprocketError::DerefInvalid(inner_type)),
                }
            }
            AstExpr::BinopExpr { left, op, right } => match op {
                AstBinop::And | AstBinop::Or | AstBinop::XOr => {
                    let left_type = Self::eval_expr_type(&left, callstack)?;
                    let right_type = Self::eval_expr_type(&right, callstack)?;
                    match (left_type, right_type) {
                        (SpkType::Bool, SpkType::Bool) => Ok(SpkType::Bool),
                        (SpkType::Bool, right_type) => Err(SprocketError::ExpectedExprTypeOf(
                            SpkType::Bool,
                            right_type.clone(),
                        )),
                        (SpkType::Int32, SpkType::Int32) => Ok(SpkType::Int32),
                        (SpkType::Int32, right_type) => Err(SprocketError::ExpectedExprTypeOf(
                            SpkType::Int32,
                            right_type.clone(),
                        )),
                        (left_type, _) => Err(SprocketError::InvalidType(left_type.clone())),
                    }
                }
                AstBinop::Divide
                | AstBinop::Exponent
                | AstBinop::Minus
                | AstBinop::Mod
                | AstBinop::Multiply
                | AstBinop::Plus => {
                    let left_type = Self::eval_expr_type(&left, callstack)?;
                    let right_type = Self::eval_expr_type(&right, callstack)?;
                    match (left_type, right_type) {
                        (SpkType::Int32, SpkType::Int32) => Ok(SpkType::Int32),
                        (SpkType::Int32, right_type) => Err(SprocketError::ExpectedExprTypeOf(
                            SpkType::Int32,
                            right_type.clone(),
                        )),
                        (left_type, _) => Err(SprocketError::InvalidType(left_type.clone())),
                    }
                }
                AstBinop::GreaterThan
                | AstBinop::GreaterThanOrEqual
                | AstBinop::LessThan
                | AstBinop::LessThanOrEqual => {
                    let left_type = Self::eval_expr_type(&left, callstack)?;
                    let right_type = Self::eval_expr_type(&right, callstack)?;
                    match (left_type, right_type) {
                        (SpkType::Int32, SpkType::Int32) => Ok(SpkType::Bool),
                        (SpkType::Int32, right_type) => Err(SprocketError::ExpectedExprTypeOf(
                            SpkType::Int32,
                            right_type.clone(),
                        )),
                        (left_type, _) => Err(SprocketError::InvalidType(left_type.clone())),
                    }
                }
                AstBinop::Equal | AstBinop::NotEqual => {
                    let left_type = Self::eval_expr_type(&left, callstack)?;
                    let right_type = Self::eval_expr_type(&right, callstack)?;
                    match (left_type, right_type) {
                        (SpkType::Int32, SpkType::Int32) | (SpkType::Bool, SpkType::Bool) => {
                            Ok(SpkType::Bool)
                        }
                        (SpkType::Int32, right_type) => Err(SprocketError::ExpectedExprTypeOf(
                            SpkType::Int32,
                            right_type.clone(),
                        )),
                        (SpkType::Bool, right_type) => Err(SprocketError::ExpectedExprTypeOf(
                            SpkType::Bool,
                            right_type.clone(),
                        )),
                        (left_type, _) => Err(SprocketError::InvalidType(left_type.clone())),
                    }
                }
            },
            AstExpr::CallExpr {
                id,
                pos_args: _,
                named_args: _,
            } => {
                let fn_def = match callstack.lookup_symbol_kind(id) {
                    Some(fn_def) => match fn_def {
                        SymbolKind::FunctionDef(fn_def) => fn_def,
                        _ => return Err(SprocketError::SymbolNotCallable(id.clone())),
                    },
                    None => return Err(SprocketError::FnNotDecl(id.clone())),
                };
                // TODO: verify args match param types
                Ok(fn_def.ret_type.clone())
            }
        }
    }

    pub fn eval_typeref(typeref: &SpkType, callstack: &CallStack) -> SprocketResult<SpkType> {
        match typeref {
            SpkType::Unresolved(id) => {
                let type_ = match callstack.lookup_symbol_kind(id) {
                    Some(SymbolKind::Type(type_)) => type_,
                    Some(SymbolKind::Var(_)) => {
                        return Err(SprocketError::ExpectedType(id.clone()))
                    }
                    Some(SymbolKind::FunctionDef(_)) => {
                        todo!()
                    }
                    Some(SymbolKind::Task(_)) => {
                        todo!()
                    }
                    None => return Err(SprocketError::TypeUndef(id.clone())),
                };
                Ok(type_.clone())
            }
            SpkType::Ref(type_) => Ok(SpkType::Ref(Box::new(Self::eval_typeref(
                type_, callstack,
            )?))),
            SpkType::Bool | SpkType::Int32 | SpkType::Void => Ok(typeref.clone()),
        }
    }
}
