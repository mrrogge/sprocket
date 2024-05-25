use std::collections::HashMap;
use std::fmt;

use crate::ast::{
    AstBinop, AstExpr, AstFnDecl, AstParamDecl, AstParamKind, AstPrgPart,
    AstStatement, AstTagDecl, AstUnop, SpkType, TableCell, AST,
};
use crate::lexer::Lexer;
use crate::token::Token;
use crate::token::TokenKeyword;

pub struct SprocketParser {
    lexer: Lexer,
    next_token: Option<Token>,
    pub strip_comments: bool,
}

impl SprocketParser {
    pub fn new() -> Self {
        Self {
            lexer: Lexer::new(),
            next_token: None,
            strip_comments: true,
        }
    }

    pub fn parse(&mut self, source: &str) -> ParserResult<AST> {
        let mut ast: AST = vec![];
        self.lexer.init(source);
        self.next_token = None;
        self.advance();
        loop {
            match &self.next_token {
                Some(Token::Comment(_)) => {
                    if self.strip_comments {
                        self.eat(Token::Comment("".to_string()))?;
                    } else {
                        let comment = self.process_comment()?;
                        ast.push(AstPrgPart::Comment(comment));
                    }
                }
                Some(Token::Newline) => {
                    self.eat(Token::Newline)?;
                }
                Some(Token::Keyword(TokenKeyword::Global | TokenKeyword::Tag)) => {
                    let tag_decl = self.process_tag_decl()?;
                    ast.push(AstPrgPart::TagDecl(tag_decl));
                }
                Some(Token::Keyword(TokenKeyword::Function)) => {
                    let fn_decl = self.process_fn_decl()?;
                    ast.push(AstPrgPart::FnDecl(fn_decl));
                }
                Some(_) => {
                    let statement = self.process_statement()?;
                    ast.push(AstPrgPart::Statement(statement));
                }
                None => {
                    break;
                }
            }
        }
        Ok(ast)
    }

    pub fn reset(&mut self) {
        self.lexer.init("");
        self.next_token = None;
    }

    fn advance(&mut self) {
        self.next_token = self.lexer.next();
    }

    fn eat(&mut self, token: Token) -> ParserResult<()> {
        match (&self.next_token, token) {
            (Some(Token::Comment(_)), Token::Comment(_))
            | (Some(Token::Id(_)), Token::Id(_))
            | (Some(Token::IntegerLiteral(_)), Token::IntegerLiteral(_))
            | (Some(Token::_RealNumLiteral(_)), Token::_RealNumLiteral(_))
            | (Some(Token::StringLiteral(_)), Token::StringLiteral(_)) => {
                self.advance();
                Ok(())
            }
            (Some(Token::Keyword(kw0)), Token::Keyword(kw1)) if *kw0 == kw1 => {
                self.advance();
                Ok(())
            }
            (Some(token0), token1) if *token0 == token1 => {
                self.advance();
                Ok(())
            }
            (Some(token0), token1) => Err(ParserError::InvalidToken(token0.clone(), token1)),
            (None, _) => Err(ParserError::UnexpectedEOF),
        }
    }

    fn _eat_id(&mut self) -> ParserResult<String> {
        match &self.next_token {
            Some(Token::Id(id)) => {
                let the_id = id.clone();
                self.advance();
                Ok(the_id)
            }
            Some(token) => Err(ParserError::UnexpectedToken(token.clone())),
            None => Err(ParserError::UnexpectedEOF),
        }
    }

    // (GLOBAL)? TAG id:id (:= expr)?;
    fn process_tag_decl(&mut self) -> ParserResult<AstTagDecl> {
        let mut is_global = false;
        if let Ok(_) = self.eat(Token::Keyword(TokenKeyword::Global)) {
            is_global = true;
        }
        self.eat(Token::Keyword(TokenKeyword::Tag))?;
        let id = match &self.next_token {
            Some(Token::Id(id)) => id.to_string(),
            Some(token) => {
                return Err(ParserError::InvalidToken(
                    token.clone(),
                    Token::Id(String::new()),
                ))
            }
            None => return Err(ParserError::UnexpectedEOF),
        };
        self.advance();
        self.eat(Token::Colon)?;
        let type_ = self.process_type()?;
        let expr = match &self.next_token {
            Some(Token::Assign) => {
                self.eat(Token::Assign)?;
                Some(self.process_expr()?)
            }
            Some(_) | None => None,
        };
        self.eat(Token::Semicolon)?;

        Ok(AstTagDecl {
            id,
            type_,
            expr,
            is_global,
        })
    }

    fn process_fn_decl(&mut self) -> ParserResult<AstFnDecl> {
        self.eat(Token::Keyword(TokenKeyword::Function))?;
        let id = match &self.next_token {
            Some(Token::Id(id)) => id.to_string(),
            Some(token) => {
                return Err(ParserError::InvalidToken(
                    token.clone(),
                    Token::Id(String::new()),
                ))
            }
            None => return Err(ParserError::UnexpectedEOF),
        };
        self.advance();
        self.eat(Token::LParen)?;
        let params: Vec<AstParamDecl> = match &self.next_token {
            Some(Token::RParen) => {
                vec![]
            }
            _ => self.process_fn_param_decls()?,
        };
        self.eat(Token::RParen)?;
        let ret_type = match &self.next_token {
            Some(Token::Colon) => {
                self.eat(Token::Colon)?;
                self.process_type()?
            }
            Some(Token::LCurly) => SpkType::Void,
            Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
            None => return Err(ParserError::UnexpectedEOF),
        };
        self.eat(Token::LCurly)?;
        let body = self.process_fn_body()?;
        self.eat(Token::RCurly)?;

        Ok(AstFnDecl {
            id,
            params,
            ret_type,
            body,
        })
    }

    fn process_fn_param_decls(&mut self) -> ParserResult<Vec<AstParamDecl>> {
        let mut params: Vec<AstParamDecl> = vec![];
        match &self.next_token {
            Some(Token::RParen) => return Ok(params),
            _ => {
                params.push(self.process_fn_param_decl()?);
            }
        }
        loop {
            match &self.next_token {
                Some(Token::RParen) => {
                    break;
                }
                Some(Token::Comma) => {
                    self.eat(Token::Comma)?;
                    params.push(self.process_fn_param_decl()?);
                }
                _ => {}
            }
        }
        Ok(params)
    }

    fn process_fn_param_decl(&mut self) -> ParserResult<AstParamDecl> {
        let kind = self.process_param_kind()?;

        let id = match &self.next_token {
            Some(Token::Id(id)) => {
                let result = id.clone();
                self.advance();
                result
            }
            Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
            None => return Err(ParserError::UnexpectedEOF),
        };
        self.eat(Token::Colon)?;
        let type_ = self.process_type()?;

        Ok(AstParamDecl { id, type_, kind })
    }

    fn _is_param_kind_token(token: &Token) -> bool {
        match token {
            Token::GreaterThan | Token::LessThan => true,
            _ => false,
        }
    }

    fn process_param_kind(&mut self) -> ParserResult<AstParamKind> {
        match &self.next_token {
            Some(Token::GreaterThan) => {
                self.eat(Token::GreaterThan)?;
                Ok(AstParamKind::Out)
            }
            Some(Token::LessThan) => {
                self.eat(Token::LessThan)?;
                match &self.next_token {
                    Some(Token::GreaterThan) => {
                        self.eat(Token::GreaterThan)?;
                        Ok(AstParamKind::InOut)
                    }
                    _ => Ok(AstParamKind::In),
                }
            }
            Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
            None => return Err(ParserError::UnexpectedEOF),
        }
    }

    fn process_fn_body(&mut self) -> ParserResult<Vec<AstPrgPart>> {
        let mut parts: Vec<AstPrgPart> = vec![];
        loop {
            match &self.next_token {
                Some(Token::RCurly) => break,
                Some(Token::Newline) => {
                    self.eat(Token::Newline)?;
                    continue;
                }
                _ => {
                    parts.push(self.process_fn_body_part()?);
                }
            }
        }
        Ok(parts)
    }

    fn process_fn_body_part(&mut self) -> ParserResult<AstPrgPart> {
        match &self.next_token {
            Some(Token::Comment(_)) => Ok(AstPrgPart::Comment(self.process_comment()?)),
            Some(Token::Keyword(TokenKeyword::Tag)) => {
                Ok(AstPrgPart::TagDecl(self.process_tag_decl()?))
            }
            _ => Ok(AstPrgPart::Statement(self.process_statement()?)),
        }
    }

    // comment
    fn process_comment(&mut self) -> ParserResult<String> {
        let comment = match &self.next_token {
            Some(Token::Comment(comment)) => comment.to_string(),
            Some(token) => {
                return Err(ParserError::InvalidToken(
                    token.clone(),
                    Token::Comment(String::new()),
                ))
            }
            None => return Err(ParserError::UnexpectedEOF),
        };
        self.advance();
        return Ok(comment);
    }

    // (table_statement_row)*\n
    fn process_table_statement(&mut self) -> ParserResult<AstStatement> {
        let mut rows: Vec<Vec<TableCell>> = vec![];
        loop {
            match &self.next_token {
                Some(Token::Pipe) => {
                    rows.push(self.process_table_statement_row()?);
                }
                Some(Token::Newline) => {
                    break;
                }
                Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
                None => {
                    break;
                }
            }
        }
        Ok(AstStatement::TableStatement(rows))
    }

    fn process_table_statement_row(&mut self) -> ParserResult<Vec<TableCell>> {
        self.eat(Token::Pipe)?;
        let mut row: Vec<TableCell> = vec![];
        loop {
            match &self.next_token {
                Some(Token::Pipe) => {
                    row.push(TableCell::Empty);
                }
                Some(Token::Newline) => {
                    self.eat(Token::Newline)?;
                    return Ok(row);
                }
                Some(Token::Tilde) => {
                    row.push(TableCell::Continued);
                    self.eat(Token::Tilde)?;
                }
                Some(_) => {
                    row.push(TableCell::Expr(self.process_expr()?));
                }
                None => return Err(ParserError::UnexpectedEOF),
            }
            match &self.next_token {
                Some(Token::Pipe) => {
                    self.eat(Token::Pipe)?;
                }
                Some(Token::Newline) => {
                    self.eat(Token::Newline)?;
                    return Ok(row);
                }
                Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
                None => return Err(ParserError::UnexpectedEOF),
            }
        }
    }

    fn process_statement(&mut self) -> ParserResult<AstStatement> {
        match &self.next_token {
            Some(Token::Pipe) => self.process_table_statement(),
            Some(Token::Keyword(TokenKeyword::Return)) => {
                Ok(AstStatement::ReturnStatement(self.process_return_stmt()?))
            }
            Some(Token::Id(id)) => {
                let left = id.clone();
                self.advance();
                match &self.next_token {
                    Some(Token::Assign) => self.process_assign_statement(left),
                    Some(Token::Semicolon) => {
                        self.eat(Token::Semicolon)?;
                        Ok(AstStatement::ExprStatement(AstExpr::IdExpr(left)))
                    }
                    Some(Token::LParen) => {
                        let call_expr = self.process_call_expr(left)?;
                        self.eat(Token::Semicolon)?;
                        Ok(AstStatement::ExprStatement(call_expr))
                    }
                    Some(_) => {
                        let binop_expr = self.process_binop(AstExpr::IdExpr(left))?;
                        self.eat(Token::Semicolon)?;
                        Ok(AstStatement::ExprStatement(binop_expr))
                    }
                    None => Err(ParserError::UnexpectedEOF),
                }
            }
            Some(_) => self.process_expr_stmt(),
            None => Err(ParserError::UnexpectedEOF),
        }
    }

    fn process_return_stmt(&mut self) -> ParserResult<Option<AstExpr>> {
        self.eat(Token::Keyword(TokenKeyword::Return))?;
        match &self.next_token {
            Some(Token::Semicolon) => {
                self.eat(Token::Semicolon)?;
                Ok(None)
            }
            _ => {
                let expr = self.process_expr()?;
                self.eat(Token::Semicolon)?;
                Ok(Some(expr))
            }
        }
    }

    fn process_expr_stmt(&mut self) -> ParserResult<AstStatement> {
        let expr = self.process_expr()?;
        self.eat(Token::Semicolon)?;
        Ok(AstStatement::ExprStatement(expr))
    }

    fn process_expr(&mut self) -> ParserResult<AstExpr> {
        match &self.next_token {
            Some(Token::Not | Token::Keyword(TokenKeyword::Not)) => {
                let op = AstUnop::Not;
                self.advance();
                if let Some(Token::LParen) = &self.next_token {
                    let expr = self.process_paren_expr()?;
                    return Ok(AstExpr::UnopExpr(op, Box::new(expr)));
                }
                let expr = self.process_expr()?;
                match expr {
                    AstExpr::BinopExpr {
                        left,
                        op: binop,
                        right,
                    } => Ok(AstExpr::BinopExpr {
                        left: Box::new(AstExpr::UnopExpr(op, left)),
                        op: binop,
                        right,
                    }),
                    _ => Ok(AstExpr::UnopExpr(op, Box::new(expr))),
                }
            }
            Some(Token::And) => {
                self.eat(Token::And)?;
                match &self.next_token {
                    Some(Token::And) | Some(Token::Asterisk) | Some(Token::Id(_)) => {
                        let expr = self.process_expr()?;
                        Ok(AstExpr::UnopExpr(AstUnop::Ref, Box::new(expr)))
                    }
                    Some(token) => Err(ParserError::UnexpectedToken(token.clone())),
                    None => Err(ParserError::UnexpectedEOF),
                }
            }
            Some(Token::Asterisk) => {
                self.eat(Token::Asterisk)?;
                match &self.next_token {
                    Some(Token::And) | Some(Token::Asterisk) | Some(Token::Id(_)) => {
                        let expr = self.process_expr()?;
                        Ok(AstExpr::UnopExpr(AstUnop::Deref, Box::new(expr)))
                    }
                    Some(token) => Err(ParserError::UnexpectedToken(token.clone())),
                    None => Err(ParserError::UnexpectedEOF),
                }
            }
            Some(Token::LParen) => Ok(self.process_paren_expr()?),
            Some(Token::Id(id)) => {
                let id = id.clone();
                self.advance();
                match &self.next_token {
                    Some(Token::LParen) => Ok(self.process_call_expr(id)?),
                    Some(token) if Self::is_token_binop(token) => {
                        Ok(self.process_binop(AstExpr::IdExpr(id))?)
                    }
                    Some(_) | None => Ok(AstExpr::IdExpr(id)),
                }
            }
            Some(token) => {
                let left = match token {
                    Token::Keyword(TokenKeyword::True) => AstExpr::BoolLiteralExpr(true),
                    Token::Keyword(TokenKeyword::False) => AstExpr::BoolLiteralExpr(false),
                    Token::IntegerLiteral(val) => AstExpr::IntLiteralExpr(*val),
                    _ => return Err(ParserError::UnexpectedToken(token.clone())),
                };
                self.advance();
                match &self.next_token {
                    Some(token) if Self::is_token_binop(token) => Ok(self.process_binop(left)?),
                    Some(_) | None => Ok(left),
                }
            }
            None => Err(ParserError::UnexpectedEOF),
        }
    }

    fn process_binop(&mut self, left: AstExpr) -> ParserResult<AstExpr> {
        let binop1 = match &self.next_token {
            Some(Token::And | Token::Keyword(TokenKeyword::And)) => {
                self.advance();
                AstBinop::And
            }
            Some(Token::Divide) => {
                self.eat(Token::Divide)?;
                AstBinop::Divide
            }
            Some(Token::Equal) => {
                self.eat(Token::Equal)?;
                AstBinop::Equal
            }
            Some(Token::GreaterThan) => {
                self.eat(Token::GreaterThan)?;
                AstBinop::GreaterThan
            }
            Some(Token::GreaterThanOrEqual) => {
                self.eat(Token::GreaterThanOrEqual)?;
                AstBinop::GreaterThanOrEqual
            }
            Some(Token::_Mod | Token::Keyword(TokenKeyword::Mod)) => {
                self.advance();
                AstBinop::Mod
            }
            Some(Token::_Or | Token::Keyword(TokenKeyword::Or)) => {
                self.advance();
                AstBinop::Or
            }
            Some(Token::_XOr | Token::Keyword(TokenKeyword::XOr)) => {
                self.advance();
                AstBinop::XOr
            }
            Some(Token::LessThan) => {
                self.eat(Token::LessThan)?;
                AstBinop::LessThan
            }
            Some(Token::LessThanOrEqual) => {
                self.eat(Token::LessThanOrEqual)?;
                AstBinop::LessThanOrEqual
            }
            Some(Token::Minus) => {
                self.eat(Token::Minus)?;
                AstBinop::Minus
            }
            Some(Token::Asterisk) => {
                self.eat(Token::Asterisk)?;
                if let Some(Token::Asterisk) = &self.next_token {
                    self.eat(Token::Asterisk)?;
                    AstBinop::Exponent
                } else {
                    AstBinop::Multiply
                }
            }
            Some(Token::NotEqual) => {
                self.eat(Token::NotEqual)?;
                AstBinop::NotEqual
            }
            Some(Token::Plus) => {
                self.eat(Token::Plus)?;
                AstBinop::Plus
            }
            Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
            None => return Err(ParserError::UnexpectedEOF),
        };

        if let Some(Token::LParen) = &self.next_token {
            let right = self.process_paren_expr()?;
            return Ok(AstExpr::BinopExpr {
                left: Box::new(left),
                op: binop1,
                right: Box::new(right),
            });
        }

        let right = self.process_expr()?;
        if let AstExpr::BinopExpr {
            left: middle,
            op: binop2,
            right,
        } = right
        {
            if binop1.priority() > binop2.priority() {
                Ok(AstExpr::BinopExpr {
                    left: Box::new(AstExpr::BinopExpr {
                        left: Box::new(left),
                        op: binop1,
                        right: middle,
                    }),
                    op: binop2,
                    right,
                })
            } else {
                Ok(AstExpr::BinopExpr {
                    left: Box::new(left),
                    op: binop1,
                    right: Box::new(AstExpr::BinopExpr {
                        left: middle,
                        op: binop2,
                        right,
                    }),
                })
            }
        } else {
            Ok(AstExpr::BinopExpr {
                left: Box::new(left),
                op: binop1,
                right: Box::new(right),
            })
        }
    }

    fn process_call_expr(&mut self, id: String) -> ParserResult<AstExpr> {
        self.eat(Token::LParen)?;
        let mut pos_args: Vec<AstExpr> = vec![];
        let mut named_args: HashMap<String, AstExpr> = HashMap::new();

        match &self.next_token {
            Some(Token::RParen) => {
                self.eat(Token::RParen)?;
                return Ok(AstExpr::CallExpr {
                    id,
                    pos_args,
                    named_args,
                });
            }
            Some(_) => {
                let (arg_expr, name) = self.process_call_arg()?;
                match name {
                    Some(name) => match named_args.insert(name, arg_expr) {
                        Some(_) => return Err(ParserError::DupArgForNamedParam(id.clone())),
                        None => {}
                    },
                    None => {
                        pos_args.push(arg_expr);
                    }
                }
            }
            None => return Err(ParserError::UnexpectedEOF),
        }
        loop {
            match &self.next_token {
                Some(Token::RParen) => {
                    self.eat(Token::RParen)?;
                    break;
                }
                Some(Token::Comma) => {
                    self.eat(Token::Comma)?;
                    let (arg_expr, name) = self.process_call_arg()?;
                    match name {
                        Some(name) => match named_args.insert(name, arg_expr) {
                            Some(_) => return Err(ParserError::DupArgForNamedParam(id.clone())),
                            None => {}
                        },
                        None => {
                            pos_args.push(arg_expr);
                        }
                    }
                }
                Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
                None => return Err(ParserError::UnexpectedEOF),
            }
        }
        Ok(AstExpr::CallExpr {
            id,
            pos_args,
            named_args,
        })
    }

    fn process_call_arg(&mut self) -> ParserResult<(AstExpr, Option<String>)> {
        let expr = self.process_expr()?;
        if let AstExpr::IdExpr(id) = &expr {
            match &self.next_token {
                Some(Token::Colon) => {
                    self.eat(Token::Colon)?;
                    let arg_expr = self.process_expr()?;
                    return Ok((arg_expr, Some(id.clone())));
                }
                Some(Token::Comma | Token::RParen) => {}
                Some(token) => return Err(ParserError::UnexpectedToken(token.clone())),
                None => return Err(ParserError::UnexpectedEOF),
            }
        }
        Ok((expr, None))
    }

    fn process_paren_expr(&mut self) -> ParserResult<AstExpr> {
        self.eat(Token::LParen)?;
        let expr = self.process_expr()?;
        self.eat(Token::RParen)?;
        Ok(expr)
    }

    fn is_token_binop(token: &Token) -> bool {
        match token {
            Token::And
            | Token::Divide
            | Token::Equal
            | Token::GreaterThan
            | Token::GreaterThanOrEqual
            | Token::Keyword(TokenKeyword::And)
            | Token::Keyword(TokenKeyword::Mod)
            | Token::Keyword(TokenKeyword::Or)
            | Token::Keyword(TokenKeyword::XOr)
            | Token::LessThan
            | Token::LessThanOrEqual
            | Token::Minus
            | Token::_Mod
            | Token::Asterisk
            | Token::NotEqual
            | Token::_Or
            | Token::Plus
            | Token::_XOr => true,
            _ => false,
        }
    }

    // ID := expr;
    fn process_assign_statement(&mut self, target: String) -> ParserResult<AstStatement> {
        self.eat(Token::Assign)?;
        let expr = self.process_expr()?;
        self.eat(Token::Semicolon)?;
        Ok(AstStatement::AssignStatement {
            target_id: target,
            expr,
        })
    }

    fn process_type(&mut self) -> ParserResult<SpkType> {
        match &self.next_token {
            Some(Token::And) => {
                self.eat(Token::And)?;
                Ok(SpkType::Ref(Box::new(self.process_type()?)))
            }
            Some(Token::Id(id)) => {
                let id = id.clone();
                self.advance();
                Ok(match id.as_str() {
                    "bool" => SpkType::Bool,
                    "i32" => SpkType::Int32,
                    id => SpkType::Unresolved(id.to_string()),
                })
            }
            Some(token) => Err(ParserError::UnexpectedToken(token.clone())),
            None => Err(ParserError::UnexpectedEOF),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParserError {
    InvalidToken(Token, Token),
    UnexpectedToken(Token),
    UnexpectedEOF,
    DupArgForNamedParam(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ParserError::InvalidToken(actual, expected) => {
                write!(f, "invalid token: {:?}; expected {:?}", actual, expected)
            }
            ParserError::UnexpectedToken(token) => {
                write!(f, "unexpected token: {:?}", token)
            }
            ParserError::UnexpectedEOF => {
                write!(f, "unexpected EOF")
            }
            ParserError::DupArgForNamedParam(name) => {
                write!(f, "duplicate arg passed for named parameter \"{}\"", name)
            }
        }
    }
}

pub type ParserResult<T> = Result<T, ParserError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn strips_comments() {
        let mut parser = SprocketParser::new();
        parser.strip_comments = true;
        assert!(matches!(&parser.parse("// comment"), Ok(ast) if ast.len() == 0));
    }

    #[test]
    fn handles_comments_if_elected() {
        let mut parser = SprocketParser::new();
        parser.strip_comments = false;
        assert!(
            matches!(&parser.parse("//comment"), Ok(ast) if matches!(&ast[0], AstPrgPart::Comment(comment) if comment == "comment"))
        )
    }

    #[test]
    fn detects_tag_name_in_tag_defs() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("tag test:bool;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::TagDecl(AstTagDecl { id, type_: _, expr: _, is_global: _ }) if id == "test"))
        )
    }

    #[test]
    fn detects_simple_type_in_tag_defs() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("tag test:bool;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::TagDecl(AstTagDecl { id: _, type_, expr: _, is_global: _ }) if matches!(type_, SpkType::Bool)))
        )
    }

    #[test]
    fn handles_ref_types_in_tag_defs() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("tag test:&bool;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::TagDecl(AstTagDecl { id: _, type_, expr: _, is_global: _ }) if matches!(type_, SpkType::Ref(type_) if matches!(type_.as_ref(), SpkType::Bool))))
        )
    }

    #[test]
    fn detects_global_tag_defs() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("global tag test:bool;");
        println!("{:?}", &result);
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::TagDecl(AstTagDecl { id: _, type_: _, expr: _, is_global: true })))
        )
    }

    #[test]
    fn handles_tags_with_bool_lit_defaults() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("tag test:bool := true;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::TagDecl(AstTagDecl { id: _, type_: _, expr, is_global: _ }) if matches!(&expr, Some(AstExpr::BoolLiteralExpr(true)))))
        )
    }

    #[test]
    fn handles_assign_stmts() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("test := true;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::Statement(AstStatement::AssignStatement { target_id: _, expr: _ })))
        )
    }

    #[test]
    #[ignore]
    fn handles_assign_stmt_with_deref() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("*test := true;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::Statement(AstStatement::AssignStatement { target_id: _, expr: _ })))
        )
    }

    #[test]
    fn handles_binops() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("1 + 1;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::Statement(AstStatement::ExprStatement(AstExpr::BinopExpr { left: _, op, right: _ })) if matches!(op, AstBinop::Plus)) )
        )
    }

    #[test]
    fn handles_unops() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("!test;");
        assert!(
            matches!(result, Ok(ast) if matches!(&ast[0], AstPrgPart::Statement(AstStatement::ExprStatement(AstExpr::UnopExpr(op, _))) if matches!(op, AstUnop::Not)))
        )
    }

    #[test]
    fn gives_unops_priority_over_binops() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("!test and test;");
        assert!(matches!(
            &result.unwrap()[0],
            AstPrgPart::Statement(AstStatement::ExprStatement(AstExpr::BinopExpr { left: _, op, right: _ })) if matches!(op, AstBinop::And)
        ))
    }

    #[test]
    fn handles_parens() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("(test);");
        assert!(matches!(
            result.unwrap()[0],
            AstPrgPart::Statement(AstStatement::ExprStatement(AstExpr::IdExpr(_)))
        ));
    }

    #[test]
    fn parens_group_binops_before_unops() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("!(test and test);");
        assert!(matches!(
            result.unwrap()[0],
            AstPrgPart::Statement(AstStatement::ExprStatement(AstExpr::UnopExpr(
                AstUnop::Not,
                _
            )))
        ));
    }

    #[test]
    fn prioritizes_mult_before_plus() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("1 * 2 + 3;");
        assert!(matches!(
            &result.unwrap()[0],
            AstPrgPart::Statement(
                AstStatement::ExprStatement(
                    AstExpr::BinopExpr { left, op: AstBinop::Plus, right: _ }
                )
            )
            if matches!(
                **left,
                AstExpr::BinopExpr { left: _, op: AstBinop::Multiply, right: _ }
            )
        ))
    }

    #[test]
    fn parens_group_lower_binops_before_higher() {
        let mut parser = SprocketParser::new();
        let result = parser.parse("1 * (2 + 3);");
        assert!(matches!(
            &result.unwrap()[0],
            AstPrgPart::Statement(
                AstStatement::ExprStatement(
                    AstExpr::BinopExpr { left: _, op: AstBinop::Multiply, right }
                )
             ) if matches!(**right, AstExpr::BinopExpr { left: _, op: AstBinop::Plus, right: _ })
        ));
    }
}
