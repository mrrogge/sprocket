use std::collections::HashMap;

use crate::{
    ast::{AstBinop, AstExpr, AstPrgPart, AstStatement, AstUnop, SpkType, TableCell, AST},
    sprocket::{SprocketError, SprocketResult},
    symbol::{SymbolKind, SymbolTable},
};

pub struct SemanticAnalyzer {}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {}
    }

    pub fn analyze(&mut self, ast: AST) -> SprocketResult<SymbolTable> {
        let mut table: HashMap<String, SymbolKind> = HashMap::new();
        table.insert("bool".to_string(), SymbolKind::Type(SpkType::Bool));
        table.insert("i32".to_string(), SymbolKind::Type(SpkType::Int32));

        let mut main_task = AST::new();

        for part in ast {
            match &part {
                AstPrgPart::TagDecl(decl) => {
                    if !decl.is_global {
                        todo!()
                    }
                    if table.contains_key(&decl.id) {
                        return Err(SprocketError::DupTagDecl(decl.id.clone()));
                    }
                    let type_ = Self::eval_typeref(&decl.type_, &table)?;
                    table.insert(decl.id.clone(), SymbolKind::Var(type_.clone()));
                    match &decl.expr {
                        Some(expr) => {
                            let expr_type = Self::eval_expr_type(expr, &table)?;
                            if type_ != expr_type {
                                return Err(SprocketError::ExpectedExprTypeOf(type_, expr_type));
                            }
                            main_task.push(part.clone());
                        }
                        None => match type_ {
                            SpkType::Ref(_) => return Err(SprocketError::TagInitReq(type_)),
                            _ => {}
                        },
                    }
                }
                AstPrgPart::Comment(_) => {}
                AstPrgPart::FnDecl(decl) => {
                    table.insert(decl.id.clone(), SymbolKind::FunctionDef(decl.clone()));
                }
                AstPrgPart::Statement(AstStatement::AssignStatement { target_id, expr }) => {
                    let var_type = match table.get(target_id) {
                        Some(SymbolKind::Var(var_type)) => var_type.clone(),
                        Some(_) => return Err(SprocketError::ExpectedVar(target_id.clone())),
                        None => return Err(SprocketError::VarNotDecl(target_id.clone())),
                    };
                    let expr_type = Self::eval_expr_type(&expr, &table)?;
                    if var_type != expr_type {
                        return Err(SprocketError::ExpectedExprTypeOf(var_type, expr_type));
                    }
                    main_task.push(part.clone());
                }
                AstPrgPart::Statement(AstStatement::ExprStatement(expr)) => {
                    Self::eval_expr_type(&expr, &table)?;
                    main_task.push(part.clone());
                }
                AstPrgPart::Statement(AstStatement::TableStatement(rows)) => {
                    for row in rows {
                        for cell in row {
                            match cell {
                                TableCell::Empty | TableCell::Continued => continue,
                                TableCell::Expr(expr) => {
                                    match Self::eval_expr_type(&expr, &table)? {
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
                    main_task.push(part.clone());
                }
                AstPrgPart::Statement(AstStatement::ReturnStatement(_)) => {
                    return Err(SprocketError::RetStmtNotInFnDecl)
                }
            }
        }

        table.insert("__main__".to_string(), SymbolKind::Task(main_task));
        Ok(table)
    }

    pub fn eval_expr_type(expr: &AstExpr, global_symbols: &SymbolTable) -> SprocketResult<SpkType> {
        match expr {
            AstExpr::BoolLiteralExpr(_) => Ok(SpkType::Bool),
            AstExpr::IntLiteralExpr(_) => Ok(SpkType::Int32),
            AstExpr::IdExpr(id) => {
                let type_ = match global_symbols.get(id) {
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
                let inner_type = Self::eval_expr_type(inner_expr, &global_symbols)?;
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
                    let left_type = Self::eval_expr_type(&left, global_symbols)?;
                    let right_type = Self::eval_expr_type(&right, global_symbols)?;
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
                    let left_type = Self::eval_expr_type(&left, global_symbols)?;
                    let right_type = Self::eval_expr_type(&right, global_symbols)?;
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
                    let left_type = Self::eval_expr_type(&left, global_symbols)?;
                    let right_type = Self::eval_expr_type(&right, global_symbols)?;
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
                    let left_type = Self::eval_expr_type(&left, global_symbols)?;
                    let right_type = Self::eval_expr_type(&right, global_symbols)?;
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
                let fn_def = match global_symbols.get(id) {
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

    pub fn eval_typeref(
        typeref: &SpkType,
        global_symbols: &SymbolTable,
    ) -> SprocketResult<SpkType> {
        match typeref {
            SpkType::Unresolved(id) => {
                let type_ = match global_symbols.get(id) {
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
                type_,
                global_symbols,
            )?))),
            SpkType::Bool | SpkType::Int32 | SpkType::Void => Ok(typeref.clone()),
        }
    }
}
