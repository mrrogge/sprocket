use core::fmt;
use std::collections::HashMap;

use crate::{
    ast::{
        AstBinop, AstExpr, AstFnDecl, AstParamKind, AstPrgPart, AstStatement, AstUnop, SpkType,
        TableCell,
    },
    semantics::SemanticAnalyzer,
    sprocket::{SprocketError, SprocketResult},
    symbol::{SymbolKind, SymbolTable},
};

#[derive(Debug, Clone)]
pub struct CallStack {
    ars: Vec<ActivationRecord>,
}

impl CallStack {
    pub fn new() -> Self {
        CallStack { ars: vec![] }
    }

    pub fn push(&mut self, ar: Option<ActivationRecord>) -> &Self {
        self.ars.push(ar.unwrap_or(ActivationRecord::new()));
        self
    }

    pub fn pop(&mut self) -> Option<ActivationRecord> {
        self.ars.pop()
    }

    fn ar_slice_for_scope_kind(&self, scope:ScopeKind) -> &[ActivationRecord] {
        match scope {
            ScopeKind::Any => &self.ars[..],
            ScopeKind::Current => &self.ars[self.ars.len()-1..],
            ScopeKind::Global => &self.ars[0..1],
        }
    }

    fn ar_mut_slice_for_scope_kind(&mut self, scope:ScopeKind) -> &mut [ActivationRecord] {
        match scope {
            ScopeKind::Any => &mut self.ars[..],
            ScopeKind::Current => {
                let len = self.ars.len();
                &mut self.ars[len-1..]}
            ScopeKind::Global => &mut self.ars[0..1],
        }
    }

    pub fn lookup_symbol_kind(&self, symbol: &str, scope:ScopeKind) -> Option<SymbolKind> {
        for ar in self.ar_slice_for_scope_kind(scope).iter().rev() {
            match ar.symbols.get(symbol) {
                Some(kind) => return Some(kind.clone()),
                None => continue,
            }
        }
        None
    }

    pub fn lookup_task(&self, symbol: &str, scope:ScopeKind) -> SprocketResult<Option<&Vec<AstPrgPart>>> {
        for ar in self.ar_slice_for_scope_kind(scope).iter().rev() {
            match ar.symbols.get(symbol) {
                Some(SymbolKind::Task(task)) => return Ok(Some(task)),
                Some(_) => return Err(SprocketError::RuntimeTypeError),
                None => continue,
            }
        }
        Ok(None)
    }

    pub fn current_ar(&self) -> Option<&ActivationRecord> {
        self.ars.last()
    }

    fn current_ar_mut(&mut self) -> Option<&mut ActivationRecord> {
        self.ars.last_mut()
    }

    pub fn insert_symbol(
        &mut self,
        symbol: &str,
        kind: SymbolKind,
    ) -> SprocketResult<Option<SymbolKind>> {
        match self.current_ar_mut() {
            Some(ar) => Ok(ar.symbols.insert(symbol.to_string(), kind)),
            None => Err(SprocketError::EmptyCallStack),
        }
    }

    pub fn lookup_symbol_val(&self, symbol: &str, scope:ScopeKind) -> SprocketResult<Option<MemTableVal>> {
        for ar in self.ar_slice_for_scope_kind(scope).iter().rev() {
            match ar.symbols.get(symbol) {
                Some(SymbolKind::Var(_type_)) => match ar.mem.get(symbol) {
                    Some(val) => return Ok(Some(val.clone())),
                    None => return Err(SprocketError::SymbolMissingVal(symbol.to_string())),
                },
                Some(kind) => {
                    return Err(SprocketError::SymbolNotAVar(
                        symbol.to_string(),
                        kind.clone(),
                    ))
                }
                None => continue,
            }
        }
        Ok(None)
    }

    pub fn set_symbol_val(
        &mut self,
        symbol: &str,
        val: MemTableVal, scope:ScopeKind,
    ) -> SprocketResult<Option<MemTableVal>> {
        for ar in self.ar_mut_slice_for_scope_kind(scope).iter_mut().rev() {
            match ar.symbols.get(symbol) {
                Some(SymbolKind::Var(type_)) => match (type_, val) {
                    (SpkType::Bool, val @ MemTableVal::Bool(_))
                    | (SpkType::Int32, val @ MemTableVal::Int32(_))
                    | (SpkType::String, val @ MemTableVal::String(_))
                    | (SpkType::Ref(_), val @ MemTableVal::_RefTo(_)) => {
                        return Ok(ar.mem.insert(symbol.to_string(), val))
                    }
                    _ => return Err(SprocketError::RuntimeTypeError),
                },
                Some(kind) => {
                    return Err(SprocketError::SymbolNotAVar(
                        symbol.to_string(),
                        kind.clone(),
                    ))
                }
                None => continue,
            }
        }
        Err(SprocketError::SymbolNotDefined(symbol.to_string()))
    }

    pub fn _lookup_symbol_bool_val(&self, symbol: &str, scope:ScopeKind) -> SprocketResult<Option<bool>> {
        match self.lookup_symbol_val(symbol, scope)? {
            Some(MemTableVal::Bool(val)) => Ok(Some(val)),
            Some(_) => Err(SprocketError::RuntimeTypeError),
            None => Ok(None),
        }
    }

    pub fn _lookup_symbol_i32_val(&self, symbol: &str, scope:ScopeKind) -> SprocketResult<Option<i32>> {
        match self.lookup_symbol_val(symbol, scope)? {
            Some(MemTableVal::Int32(val)) => Ok(Some(val)),
            Some(_) => Err(SprocketError::RuntimeTypeError),
            None => Ok(None),
        }
    }

    pub fn lookup_symbol_ref_val(&self, symbol: &str, scope:ScopeKind) -> SprocketResult<Option<String>> {
        match self.lookup_symbol_val(symbol, scope)? {
            Some(MemTableVal::_RefTo(id)) => Ok(Some(id)),
            Some(_) => Err(SprocketError::RuntimeTypeError),
            None => Ok(None),
        }
    }

    pub fn bool_val_from_expr(&mut self, expr: &AstExpr, en: bool) -> SprocketResult<bool> {
        match self.val_from_expr(expr, en)? {
            MemTableVal::Bool(val) => Ok(val),
            _ => Err(SprocketError::RuntimeTypeError),
        }
    }

    pub fn int_val_from_expr(&mut self, expr: &AstExpr, en: bool) -> SprocketResult<i32> {
        match self.val_from_expr(expr, en)? {
            MemTableVal::Int32(val) => Ok(val),
            _ => Err(SprocketError::RuntimeTypeError),
        }
    }

    pub fn _ref_id_from_expr(expr: &AstExpr) -> SprocketResult<(String, i32)> {
        Self::_ref_id_from_expr_inner(expr, 0)
    }

    fn _ref_id_from_expr_inner(expr: &AstExpr, ref_count: i32) -> SprocketResult<(String, i32)> {
        match expr {
            AstExpr::UnopExpr(AstUnop::Ref, inner_expr) => {
                Self::_ref_id_from_expr_inner(inner_expr.as_ref(), ref_count + 1)
            }
            AstExpr::UnopExpr(AstUnop::Deref, inner_expr) => {
                Self::_ref_id_from_expr_inner(inner_expr.as_ref(), ref_count - 1)
            }
            AstExpr::IdExpr(id) => Ok((id.clone(), ref_count)),
            _ => Err(SprocketError::RuntimeTypeError),
        }
    }

    pub fn lookup_fn_decl(&self, symbol: &str, scope:ScopeKind) -> SprocketResult<Option<AstFnDecl>> {
        match self.lookup_symbol_kind(symbol, scope) {
            Some(SymbolKind::FunctionDef(fn_def)) => Ok(Some(fn_def.clone())),
            Some(_) => Err(SprocketError::RuntimeTypeError),
            None => Ok(None),
        }
    }

    pub fn val_from_expr(&mut self, expr: &AstExpr, en: bool) -> SprocketResult<MemTableVal> {
        match expr {
            AstExpr::BoolLiteralExpr(val) => Ok(MemTableVal::Bool(*val)),
            AstExpr::IntLiteralExpr(val) => Ok(MemTableVal::Int32(*val)),
            AstExpr::StringLiteralExpr(val) => Ok(MemTableVal::String(val.to_string())),
            AstExpr::IdExpr(id) => match self.lookup_symbol_val(id, ScopeKind::Any)? {
                Some(val) => Ok(val),
                None => Err(SprocketError::SymbolNotDefined(id.to_string())),
            },
            AstExpr::UnopExpr(op, expr) => match op {
                AstUnop::Not => {
                    let val = self.bool_val_from_expr(expr, en)?;
                    return Ok(MemTableVal::Bool(!val));
                }
                AstUnop::Ref => {
                    todo!()
                }
                AstUnop::Deref => match expr.as_ref() {
                    AstExpr::IdExpr(id) => {
                        let deref_id = self.lookup_symbol_ref_val(id, ScopeKind::Any)?.unwrap();
                        let val = self.lookup_symbol_val(&deref_id, ScopeKind::Any)?.unwrap();
                        return Ok(val);
                    }
                    AstExpr::UnopExpr(AstUnop::Ref | AstUnop::Deref, _) => {
                        todo!()
                    }
                    _ => return Err(SprocketError::RuntimeTypeError),
                },
            },
            AstExpr::BinopExpr { left, op, right } => {
                let left_type = match SemanticAnalyzer::eval_expr_type(&left, &self) {
                    Ok(left_type) => left_type,
                    Err(_) => return Err(SprocketError::RuntimeTypeError),
                };
                let right_type = match SemanticAnalyzer::eval_expr_type(&right, &self) {
                    Ok(right_type) => right_type,
                    Err(_) => return Err(SprocketError::RuntimeTypeError),
                };
                match (left_type, right_type) {
                    (SpkType::Bool, SpkType::Bool) => {
                        let left_val = self.bool_val_from_expr(left, true)?;
                        let right_val = self.bool_val_from_expr(right, true)?;
                        let val = match op {
                            AstBinop::And => left_val && right_val,
                            AstBinop::Equal => left_val == right_val,
                            AstBinop::NotEqual => left_val != right_val,
                            AstBinop::Or => left_val || right_val,
                            AstBinop::XOr => left_val ^ right_val,
                            _ => return Err(SprocketError::RuntimeTypeError),
                        };
                        return Ok(MemTableVal::Bool(val));
                    }
                    (SpkType::Bool, _) => return Err(SprocketError::RuntimeTypeError),
                    (SpkType::Int32, SpkType::Int32) => {
                        let left_val = self.int_val_from_expr(left, en)?;
                        let right_val = self.int_val_from_expr(right, en)?;
                        let val = match op {
                            AstBinop::Equal => MemTableVal::Bool(left_val == right_val),
                            AstBinop::GreaterThan => MemTableVal::Bool(left_val > right_val),
                            AstBinop::GreaterThanOrEqual => {
                                MemTableVal::Bool(left_val >= right_val)
                            }
                            AstBinop::LessThan => MemTableVal::Bool(left_val < right_val),
                            AstBinop::LessThanOrEqual => MemTableVal::Bool(left_val <= right_val),
                            AstBinop::NotEqual => MemTableVal::Bool(left_val != right_val),
                            AstBinop::And => MemTableVal::Int32(left_val & right_val),
                            AstBinop::Divide => MemTableVal::Int32(left_val / right_val),
                            AstBinop::Exponent => todo!(),
                            AstBinop::Minus => MemTableVal::Int32(left_val - right_val),
                            AstBinop::Mod => MemTableVal::Int32(left_val % right_val),
                            AstBinop::Multiply => MemTableVal::Int32(left_val * right_val),
                            AstBinop::Plus => MemTableVal::Int32(left_val + right_val),
                            AstBinop::Or => MemTableVal::Int32(left_val | right_val),
                            AstBinop::XOr => MemTableVal::Int32(left_val ^ right_val),
                        };
                        return Ok(val);
                    }
                    _ => return Err(SprocketError::RuntimeTypeError),
                }
            }
            AstExpr::CallExpr {
                id,
                pos_args,
                named_args,
            } => {
                self.push(None);
                self.insert_symbol("__en__", SymbolKind::Var(SpkType::Bool))?;
                self.set_symbol_val("__en__", MemTableVal::Bool(en), ScopeKind::Current)?;
                let fn_def = match self.lookup_fn_decl(id, ScopeKind::Any)? {
                    Some(fn_def) => fn_def,
                    None => return Err(SprocketError::UndefFunction(id.clone())),
                };
                match &fn_def.ret_type {
                    SpkType::Void => {}
                    SpkType::Unresolved(_) => return Err(SprocketError::RuntimeTypeError),
                    type_ => {
                        self.insert_symbol("__ret__", SymbolKind::Var(type_.clone()))?;
                    }
                }
                for (idx, param) in fn_def.params.iter().enumerate() {
                    self.insert_symbol(&param.id, SymbolKind::Var(param.type_.clone()))?;
                    let arg_val = match named_args.get(&param.id) {
                        Some(arg_val) => arg_val,
                        None => &pos_args[idx],
                    };
                    match &param.kind {
                        AstParamKind::In | AstParamKind::InOut => {
                            let val = self.val_from_expr(arg_val, en)?;
                            self.set_symbol_val(&param.id, val, ScopeKind::Current)?;
                        }
                        AstParamKind::Out => {}
                    }
                }
                for part in &fn_def.body {
                    self.exe_prg_part(part)?;
                    if let AstPrgPart::Statement(AstStatement::ReturnStatement(_)) = part {
                        break;
                    }
                }
                let mut out_vals: HashMap<String, MemTableVal> = HashMap::new();
                for (idx, param) in fn_def.params.iter().enumerate() {
                    if let AstParamKind::In = &param.kind {
                        continue;
                    }
                    let dest_id = match named_args.get(&param.id) {
                        Some(AstExpr::IdExpr(id)) => id,
                        Some(_) => return Err(SprocketError::RuntimeTypeError),
                        None => match &pos_args[idx] {
                            AstExpr::IdExpr(id) => id,
                            _ => return Err(SprocketError::RuntimeTypeError),
                        },
                    };
                    let dest_val = self.lookup_symbol_val(&param.id, ScopeKind::Current)?.unwrap();
                    out_vals.insert(dest_id.clone(), dest_val);
                }
                let ret_val = self.lookup_symbol_val("__ret__", ScopeKind::Current)?.unwrap();
                self.pop();
                for (id, val) in out_vals {
                    self.set_symbol_val(&id, val, ScopeKind::Any)?;
                }
                Ok(ret_val)
            }
        }
    }

    pub fn exe_prg_part(&mut self, part: &AstPrgPart) -> SprocketResult<()> {
        match part {
            AstPrgPart::Comment(_) => Ok(()),
            AstPrgPart::TagDecl(tag_decl) => {
                self.insert_symbol(&tag_decl.id, SymbolKind::Var(tag_decl.type_.clone()))?;
                let init_val = match &tag_decl.expr {
                    Some(expr) => self.val_from_expr(&expr, true)?,
                    None => match &tag_decl.type_ {
                        SpkType::Bool => MemTableVal::Bool(false),
                        SpkType::Int32 => MemTableVal::Int32(0),
                        SpkType::String => MemTableVal::String("".to_string()),
                        _ => return Err(SprocketError::RuntimeTypeError),
                    },
                };
                self.set_symbol_val(&tag_decl.id, init_val, ScopeKind::Current)?;
                Ok(())
            }
            AstPrgPart::FnDecl(fn_decl) => {
                self.insert_symbol(&fn_decl.id, SymbolKind::FunctionDef(fn_decl.clone()))?;
                Ok(())
            }
            AstPrgPart::Statement(AstStatement::AssignStatement { target_id, expr }) => {
                let val = self.val_from_expr(expr, true)?;
                self.set_symbol_val(&target_id, val, ScopeKind::Any)?;
                Ok(())
            }
            AstPrgPart::Statement(AstStatement::ExprStatement(expr)) => {
                self.val_from_expr(expr, true)?;
                Ok(())
            }
            AstPrgPart::Statement(AstStatement::TableStatement(rows)) => {
                let mut col = 0;
                let mut prev_nodes = vec![true];
                let mut new_nodes = vec![true];
                let row_count = rows.len();
                'outer: loop {
                    new_nodes.clear();
                    for row_idx in 0..row_count {
                        let prev_node = match prev_nodes.get(row_idx) {
                            Some(node) => node,
                            None => match prev_nodes.last() {
                                Some(node) => node,
                                None => break 'outer,
                            },
                        };
                        if let Some(cell) = rows[row_idx].get(col) {
                            match cell {
                                TableCell::Continued => {
                                    new_nodes.push(*prev_node);
                                }
                                TableCell::Empty => break,
                                TableCell::Expr(expr) => {
                                    let val = self.bool_val_from_expr(expr, *prev_node)?;
                                    new_nodes.push(val && *prev_node);
                                }
                            }
                        } else {
                            break;
                        }
                    }
                    for row_idx in (0..row_count).rev() {
                        if let None | Some(TableCell::Empty) = rows[row_idx].get(col + 1) {
                            if row_idx == 0 {
                                new_nodes.pop();
                                break;
                            }
                            match (new_nodes.get(row_idx), new_nodes.get(row_idx - 1)) {
                                (Some(_), Some(_)) => {
                                    new_nodes[row_idx - 1] =
                                        new_nodes[row_idx - 1] || new_nodes[row_idx];
                                    new_nodes.pop();
                                }
                                (None, Some(_)) => {}
                                (Some(_), None) => {}
                                (None, None) => {
                                    break;
                                }
                            }
                        }
                    }
                    col += 1;
                    std::mem::swap(&mut prev_nodes, &mut new_nodes);
                }
                Ok(())
            }
            AstPrgPart::Statement(AstStatement::ReturnStatement(expr)) => {
                let ret_val = match expr {
                    Some(expr) => Some(self.val_from_expr(expr, true)?),
                    None => None,
                };
                match self.current_ar().unwrap().symbols.get("__ret__") {
                    Some(_) => {
                        self.set_symbol_val("__ret__", ret_val.unwrap(), ScopeKind::Current)?;
                    }
                    None => {}
                }
                Ok(())
            }
            AstPrgPart::Statement(AstStatement::StmtBlock(block)) => {
                self.push(None);
                for part in block {
                    self.exe_prg_part(part)?;
                }
                self.pop();
                Ok(())
            }
            AstPrgPart::Statement(AstStatement::IfStatement { cond, block, else_block }) => {
                let cond_val = self.bool_val_from_expr(cond, true)?;
                if cond_val {
                    self.push(None);
                    for part in block {
                        self.exe_prg_part(part)?;
                    }
                    self.pop();
                }
                else {
                    match else_block {
                        Some(block) => {
                            self.push(None);
                            for part in block {
                                self.exe_prg_part(part)?;
                            }
                            self.pop();
                        }
                        None => {}
                    }
                }
                Ok(())
            }
            AstPrgPart::Task(_) => {
                todo!()
            }
        }
    }

    pub fn load_globals(&mut self) -> SprocketResult<()> {
        self.insert_symbol("bool", SymbolKind::Type(SpkType::Bool))?;
        self.insert_symbol("i32", SymbolKind::Type(SpkType::Int32))?;
        self.insert_symbol("string", SymbolKind::Type(SpkType::String))?;
        Ok(())
    }

    pub fn load_default_vals(&mut self) -> &Self {
        for ar in &mut self.ars {
            for symbol_entry in &mut ar.symbols {
                match symbol_entry {
                    (symbol, SymbolKind::Var(SpkType::Bool)) => {
                        if let None = ar.mem.get(symbol) {
                            ar.mem.insert(symbol.clone(), MemTableVal::Bool(false));
                        }
                    }
                    (symbol, SymbolKind::Var(SpkType::Int32)) => {
                        if let None = ar.mem.get(symbol) {
                            ar.mem.insert(symbol.clone(), MemTableVal::Int32(0));
                        }
                    }
                    (symbol, SymbolKind::Var(SpkType::String)) => {
                        if let None = ar.mem.get(symbol) {
                            ar.mem.insert(symbol.clone(), MemTableVal::String("".to_string()));
                        }
                    }
                    (_, _) => {}
                }
            }
        }
        self
    }
}

#[derive(Debug, Clone)]
pub struct ActivationRecord {
    pub symbols: SymbolTable,
    pub mem: MemTable,
}

impl ActivationRecord {
    pub fn new() -> Self {
        Self {
            symbols: SymbolTable::new(),
            mem: MemTable::new(),
        }
    }
}

pub type MemTable = HashMap<String, MemTableVal>;

#[derive(Debug, Clone)]
pub enum MemTableVal {
    Bool(bool),
    Int32(i32),
    _RefTo(String),
    String(String),
}

impl fmt::Display for MemTableVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MemTableVal::Bool(val) => write!(f, "{}", val),
            MemTableVal::Int32(val) => write!(f, "{}", val),
            MemTableVal::_RefTo(val) => write!(f, "&{}", val),
            MemTableVal::String(val) => write!(f, "\"{}\"", val),
        }
    }
}

pub enum ScopeKind {
    Current,
    Global,
    Any,
}
