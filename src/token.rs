use core::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Keyword(TokenKeyword),
    Id(String),
    Colon,
    StringLiteral(String),
    IntegerLiteral(i32),
    _RealNumLiteral(f32),
    Period,
    LCurly,
    RCurly,
    LParen,
    RParen,
    Comment(String),
    Newline,
    Semicolon,
    Pound,
    Comma,
    LBracket,
    RBracket,
    Percent,
    Pipe,
    Assign,
    AssignOut,
    Plus,
    Minus,
    Asterisk,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    And,
    _Or,
    _XOr,
    _Mod,
    Not,
    Tilde,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Keyword(kw) => write!(f, "Keyword({})", kw),
            Token::Id(id) => write!(f, "Id({})", id),
            Token::Colon => write!(f, "\":\""),
            Token::StringLiteral(val) => write!(f, "\"{}\"", val),
            Token::IntegerLiteral(val) => write!(f, "{}", val),
            Token::_RealNumLiteral(val) => write!(f, "{}", val),
            Token::Period => write!(f, "\".\""),
            Token::LCurly => todo!(),
            Token::RCurly => todo!(),
            Token::LParen => todo!(),
            Token::RParen => todo!(),
            Token::Comment(_) => todo!(),
            Token::Newline => todo!(),
            Token::Semicolon => todo!(),
            Token::Pound => todo!(),
            Token::Comma => todo!(),
            Token::LBracket => todo!(),
            Token::RBracket => todo!(),
            Token::Percent => todo!(),
            Token::Pipe => todo!(),
            Token::Assign => todo!(),
            Token::AssignOut => todo!(),
            Token::Plus => todo!(),
            Token::Minus => todo!(),
            Token::Asterisk => todo!(),
            Token::Divide => todo!(),
            Token::Equal => todo!(),
            Token::NotEqual => todo!(),
            Token::LessThan => todo!(),
            Token::GreaterThan => todo!(),
            Token::LessThanOrEqual => todo!(),
            Token::GreaterThanOrEqual => todo!(),
            Token::And => todo!(),
            Token::_Or => todo!(),
            Token::_XOr => todo!(),
            Token::_Mod => todo!(),
            Token::Not => todo!(),
            Token::Tilde => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKeyword {
    Global,
    Tag,
    FunctionBlock,
    Function,
    And,
    Or,
    XOr,
    Mod,
    True,
    False,
    Not,
    Use,
    Return,
}

impl fmt::Display for TokenKeyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub fn keyword_token_from_str(s: &str) -> Option<Token> {
    match s {
        "fb" => Some(Token::Keyword(TokenKeyword::FunctionBlock)),
        "fn" => Some(Token::Keyword(TokenKeyword::Function)),
        "global" => Some(Token::Keyword(TokenKeyword::Global)),
        "tag" => Some(Token::Keyword(TokenKeyword::Tag)),
        "and" => Some(Token::Keyword(TokenKeyword::And)),
        "or" => Some(Token::Keyword(TokenKeyword::Or)),
        "xor" => Some(Token::Keyword(TokenKeyword::XOr)),
        "mod" => Some(Token::Keyword(TokenKeyword::Mod)),
        "true" => Some(Token::Keyword(TokenKeyword::True)),
        "false" => Some(Token::Keyword(TokenKeyword::False)),
        "not" => Some(Token::Keyword(TokenKeyword::Not)),
        "use" => Some(Token::Keyword(TokenKeyword::Use)),
        "return" => Some(Token::Keyword(TokenKeyword::Return)),
        _ => None,
    }
}
