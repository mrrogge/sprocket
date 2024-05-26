use crate::token;
use crate::token::Token;

pub struct Lexer {
    idx: usize,
    chars: Vec<char>,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            idx: 0,
            chars: vec![],
        }
    }

    pub fn init(&mut self, source: &str) -> &Self {
        self.idx = 0;
        self.chars = source.chars().collect();
        self
    }

    fn advance(&mut self, amount: usize) {
        self.idx += amount;
    }

    fn peek(&self, pos: usize, len: usize) -> Option<String> {
        match self.chars.get(self.idx + pos..self.idx + pos + len) {
            Some(chars) => Some(String::from_iter(chars)),
            None => None,
        }
    }

    fn peek_char(&self, pos: usize) -> Option<char> {
        match self.chars.get(self.idx + pos) {
            Some(c) => Some(*c),
            None => None,
        }
    }

    #[inline]
    fn peek_one(&self) -> Option<char> {
        self.peek_char(0)
    }

    fn _peek_until_whitespace(&self) -> Option<String> {
        let mut s = String::new();
        let mut i: usize = 1;
        loop {
            match self.chars.get(self.idx + i) {
                Some(c) => {
                    if Self::is_whitespace(c) {
                        return Some(s);
                    } else {
                        s.push(*c);
                        i += 1;
                        continue;
                    }
                }
                None => {
                    if s.len() > 0 {
                        return Some(s);
                    } else {
                        return None;
                    }
                }
            }
        }
    }

    #[inline]
    fn is_whitespace(c: &char) -> bool {
        c == &' ' || c == &'\t' || c == &'\u{feff}'
    }

    fn try_whitespace(&mut self) -> Option<char> {
        match self.peek_one() {
            Some(c) => {
                if Lexer::is_whitespace(&c) {
                    self.advance(1);
                    Some(c)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    fn try_keyword(&mut self) -> Option<Token> {
        let mut kw_string = String::new();
        let mut peek_pos: usize = 0;
        loop {
            if let Some(c) = self.peek_char(peek_pos) {
                if c.is_ascii_alphabetic() || c == '_' {
                    kw_string.push(c);
                    peek_pos += 1;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        if kw_string.len() > 0 {
            match token::keyword_token_from_str(&kw_string) {
                Some(token) => {
                    self.advance(peek_pos);
                    Some(token)
                }
                None => None,
            }
        } else {
            None
        }
    }

    fn try_id(&mut self) -> Option<Token> {
        let mut id_string = String::new();
        let mut peek_pos: usize = 0;
        match self.peek_char(peek_pos) {
            Some(c) => {
                if c == '_' || c.is_ascii_alphabetic() {
                    id_string.push(c);
                    peek_pos += 1;
                    loop {
                        if let Some(c) = self.peek_char(peek_pos) {
                            if c == '_' || c.is_ascii_alphanumeric() {
                                id_string.push(c);
                                peek_pos += 1;
                                continue;
                            } else {
                                self.advance(peek_pos);
                                return Some(Token::Id(id_string));
                            }
                        } else {
                            self.advance(peek_pos);
                            return Some(Token::Id(id_string));
                        }
                    }
                } else {
                    return None;
                }
            }
            None => return None,
        }
    }

    fn try_string_value(&mut self) -> Option<Token> {
        let mut val_string = String::new();
        let mut peek_pos: usize = 0;
        match self.peek_char(peek_pos) {
            Some(c) => {
                if c == '\'' {
                    peek_pos += 1;
                    loop {
                        if let Some(c) = self.peek_char(peek_pos) {
                            if c == '\'' {
                                self.advance(val_string.len() + 2);
                                return Some(Token::StringLiteral(val_string));
                            } else {
                                val_string.push(c);
                                peek_pos += 1;
                                continue;
                            }
                        } else {
                            return None;
                        }
                    }
                } else {
                    return None;
                }
            }
            None => return None,
        }
    }

    fn try_integer_literal(&mut self) -> Option<Token> {
        if let Some(dec) = self.try_dec_digit_string() {
            return Some(Token::IntegerLiteral(dec));
        }
        match self.try_digit_string() {
            Some((digits, radix)) => match i32::from_str_radix(&digits, radix) {
                Ok(val) => Some(Token::IntegerLiteral(val)),
                Err(_) => None,
            },
            None => None,
        }
    }

    fn _try_real_num_literal(&mut self) -> Option<Token> {
        todo!();
    }

    fn try_dec_digit_string(&mut self) -> Option<i32> {
        if let Some(c) = self.peek_one() {
            if c.is_ascii_digit() {
                let mut digit_string = c.to_string();
                self.advance(1);

                while let Some(c) = self.peek_one() {
                    if c.is_ascii_digit() || c == '_' {
                        digit_string.push(c);
                        self.advance(1);
                    } else {
                        break;
                    }
                }
                match i32::from_str_radix(&digit_string, 10) {
                    Ok(dec) => Some(dec),
                    Err(_) => None,
                }
            } else {
                None
            }
        } else {
            None
        }
    }

    fn try_digit_string(&mut self) -> Option<(String, u32)> {
        let mut digit_string = "".to_string();
        let mut radix_string = "".to_string();
        match self.peek_one() {
            Some(c) if c.is_digit(10) => {
                radix_string.push(c);
            }
            Some(_) | None => return None,
        }
        loop {
            match self.peek_one() {
                Some(c) if c.is_digit(10) => {
                    radix_string.push(c);
                }
                Some(c) if c == '#' => {
                    break;
                }
                Some(_) | None => return None,
            }
        }
        let radix = match u32::from_str_radix(&radix_string, 10) {
            Ok(radix) => radix,
            Err(_) => return None,
        };
        if let Some(c) = self.peek_char(radix_string.len() + 1) {
            if c.is_digit(radix) {
                digit_string.push(c);
                self.advance(radix_string.len() + 2);
            } else {
                return None;
            }
        } else {
            return None;
        }

        while let Some(c) = self.peek_one() {
            if c.is_digit(radix) || c == '_' {
                digit_string.push(c);
                self.advance(1);
            } else {
                break;
            }
        }

        Some((digit_string, radix))
    }

    fn try_simple(&mut self) -> Option<Token> {
        if let Some(s) = self.peek(0, 2) {
            if s == ":=" {
                self.advance(2);
                return Some(Token::Assign);
            } else if s == "=>" {
                self.advance(2);
                return Some(Token::AssignOut);
            } else if s == "!=" {
                self.advance(2);
                return Some(Token::NotEqual);
            } else if s == "<=" {
                self.advance(2);
                return Some(Token::LessThanOrEqual);
            } else if s == ">=" {
                self.advance(2);
                return Some(Token::GreaterThanOrEqual);
            } else {
            }
        } else {
        }

        if let Some(c) = self.peek_one() {
            if c == ':' {
                self.advance(1);
                return Some(Token::Colon);
            } else if c == '{' {
                self.advance(1);
                return Some(Token::LCurly);
            } else if c == '}' {
                self.advance(1);
                return Some(Token::RCurly);
            } else if c == ';' {
                self.advance(1);
                return Some(Token::Semicolon);
            } else if c == '.' {
                self.advance(1);
                return Some(Token::Period);
            } else if c == '(' {
                self.advance(1);
                return Some(Token::LParen);
            } else if c == ')' {
                self.advance(1);
                return Some(Token::RParen);
            } else if c == '#' {
                self.advance(1);
                return Some(Token::Pound);
            } else if c == '+' {
                self.advance(1);
                return Some(Token::Plus);
            } else if c == '-' {
                self.advance(1);
                return Some(Token::Minus);
            } else if c == '*' {
                self.advance(1);
                return Some(Token::Asterisk);
            } else if c == '/' {
                self.advance(1);
                return Some(Token::Divide);
            } else if c == ',' {
                self.advance(1);
                return Some(Token::Comma);
            } else if c == '<' {
                self.advance(1);
                return Some(Token::LessThan);
            } else if c == '>' {
                self.advance(1);
                return Some(Token::GreaterThan);
            } else if c == '=' {
                self.advance(1);
                return Some(Token::Equal);
            } else if c == '&' {
                self.advance(1);
                return Some(Token::And);
            } else if c == '[' {
                self.advance(1);
                return Some(Token::LBracket);
            } else if c == ']' {
                self.advance(1);
                return Some(Token::RBracket);
            } else if c == '%' {
                self.advance(1);
                return Some(Token::Percent);
            } else if c == '|' {
                self.advance(1);
                return Some(Token::Pipe);
            } else if c == ';' {
                self.advance(1);
                return Some(Token::Semicolon);
            } else if c == '!' {
                self.advance(1);
                return Some(Token::Not);
            } else if c == '~' {
                self.advance(1);
                return Some(Token::Tilde);
            } else {
            }
        } else {
        }

        return None;
    }

    fn try_newline(&mut self) -> Option<Token> {
        return if let Some(c) = self.peek_one() {
            if c == '\n' {
                self.advance(1);
                Some(Token::Newline)
            } else if c == '\r' {
                self.advance(1);
                if let Some(c) = self.peek_one() {
                    if c == '\n' {
                        self.advance(1);
                    }
                }
                Some(Token::Newline)
            } else {
                None
            }
        } else {
            None
        };
    }

    fn try_comment(&mut self) -> Option<Token> {
        if let Some(s) = self.peek(0, 2) {
            if s == "//" {
                self.advance(2);
                let mut comment = "".to_string();
                loop {
                    if let Some(_) = self.try_newline() {
                        return Some(Token::Comment(comment.trim().to_string()));
                    } else {
                        if let Some(c) = self.peek_one() {
                            comment.push(c);
                            self.advance(1);
                        } else {
                            return Some(Token::Comment(comment.trim().to_string()));
                        }
                    }
                }
            } else if s == "(*" {
                self.advance(2);
                let mut comment = "".to_string();
                loop {
                    if let Some(s) = self.peek(0, 2) {
                        if s == "*)" {
                            self.advance(2);
                            return Some(Token::Comment(comment.trim().to_string()));
                        }
                    }
                    if let Some(c) = self.peek_one() {
                        comment.push(c);
                        self.advance(1);
                    } else {
                        return None;
                    }
                }
            } else {
                return None;
            }
        } else {
            return None;
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(_) = self.try_whitespace() {
                continue;
            }
            if let Some(token) = self.try_newline() {
                return Some(token);
            }
            if let Some(token) = self.try_comment() {
                return Some(token);
            } else if let Some(token) = self.try_simple() {
                return Some(token);
            } else if let Some(token) = self.try_keyword() {
                return Some(token);
            } else if let Some(token) = self.try_string_value() {
                return Some(token);
            } else if let Some(token) = self.try_integer_literal() {
                return Some(token);
            } else if let Some(token) = self.try_id() {
                return Some(token);
            } else {
                return None;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenKeyword;

    use super::*;

    #[test]
    fn ignores_whitespace() {
        let mut lexer = Lexer::new();
        lexer.init("  \t");
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn handles_empty() {
        let mut lexer = Lexer::new();
        lexer.init("");
        assert!(matches!(lexer.next(), None))
    }

    #[test]
    fn handles_colon() {
        let mut lexer = Lexer::new();
        lexer.init(":");
        assert!(matches!(lexer.next(), Some(Token::Colon)));
    }

    #[test]
    fn handles_assign() {
        let mut lexer = Lexer::new();
        lexer.init(":=");
        assert!(matches!(lexer.next(), Some(Token::Assign)));
    }

    #[test]
    fn handles_lcurly() {
        let mut lexer = Lexer::new();
        lexer.init("{");
        assert!(matches!(lexer.next(), Some(Token::LCurly)));
    }

    #[test]
    fn handles_rcurly() {
        let mut lexer = Lexer::new();
        lexer.init("}");
        assert!(matches!(lexer.next(), Some(Token::RCurly)));
    }

    #[test]
    fn handles_semicolon() {
        let mut lexer = Lexer::new();
        lexer.init(";");
        assert!(matches!(lexer.next(), Some(Token::Semicolon)));
    }

    #[test]
    fn handles_newline() {
        let mut lexer = Lexer::new();
        lexer.init("\n\r\r\n");
        assert!(matches!(lexer.next(), Some(Token::Newline)));
        assert!(matches!(lexer.next(), Some(Token::Newline)));
        assert!(matches!(lexer.next(), Some(Token::Newline)));
    }

    #[test]
    fn lexes_normal_id() {
        let mut lexer = Lexer::new();
        lexer.init("identifier");
        assert!(matches!(lexer.next(), Some(Token::Id(s)) if s == "identifier"));
    }

    #[test]
    fn lexes_id_with_leading_underscore() {
        let mut lexer = Lexer::new();
        lexer.init("_identifier");
        assert!(matches!(lexer.next(), Some(Token::Id(s)) if s == "_identifier"));
    }

    #[test]
    fn lexes_id_with_nums() {
        let mut lexer = Lexer::new();
        lexer.init("abc123");
        assert!(matches!(lexer.next(), Some(Token::Id(s)) if s == "abc123"));
    }

    #[test]
    fn lexes_ids_separately() {
        let mut lexer = Lexer::new();
        lexer.init("id1 id2");
        assert!(matches!(lexer.next(), Some(Token::Id(s)) if s == "id1"));
        assert!(matches!(lexer.next(), Some(Token::Id(s)) if s == "id2"));
    }

    #[test]
    fn lexes_keywords() {
        let mut lexer = Lexer::new();
        lexer.init("fn fb");
        assert!(matches!(
            lexer.next(),
            Some(Token::Keyword(TokenKeyword::Function))
        ));
        assert!(matches!(
            lexer.next(),
            Some(Token::Keyword(TokenKeyword::FunctionBlock))
        ));
    }

    #[test]
    fn ignores_utf8_bom() {
        let mut lexer = Lexer::new();
        lexer.init("\u{feff}:");
        assert!(matches!(lexer.next(), Some(Token::Colon)));
    }

    #[test]
    fn lexes_string_values() {
        let mut lexer = Lexer::new();
        lexer.init("'string value'");
        assert!(matches!(lexer.next(), Some(Token::StringLiteral(s)) if s == "string value"));
    }

    #[test]
    fn does_not_panic_when_advancing_past_end() {
        let mut lexer = Lexer::new();
        lexer.init("");
        lexer.next();
    }

    #[test]
    fn lexes_id_that_starts_with_keyword() {
        let mut lexer = Lexer::new();
        let tag_name = "modIsAKeywordButThisIsATag";
        lexer.init(tag_name);
        assert!(matches!(lexer.next(), Some(Token::Id(id)) if id == tag_name));
    }

    #[test]
    fn lexes_nots() {
        let mut lexer = Lexer::new();
        lexer.init("! not");
        assert!(matches!(lexer.next(), Some(Token::Not)));
        assert!(matches!(
            lexer.next(),
            Some(Token::Keyword(TokenKeyword::Not))
        ));
    }
}
