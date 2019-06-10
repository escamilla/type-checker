#[derive(Debug, PartialEq)]
enum Token {
    Identifier(String),
    Integer(i32),
}

pub struct Tokenizer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer { input, position: 0 }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.next_token() {
            tokens.push(token);
        }
        tokens
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        if self.is_eof() {
            None
        } else if self.next_char().is_alphabetic() {
            Some(self.read_identifier())
        } else if self.next_char().is_numeric() || self.next_char() == '-' {
            Some(self.read_integer())
        } else {
            panic!("unexpected character: {}", self.next_char())
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_eof() && self.next_char().is_whitespace() {
            self.position += 1;
        }
    }

    fn is_eof(&self) -> bool {
        self.position >= self.input.len()
    }

    fn next_char(&self) -> char {
        self.input[self.position..].chars().next().unwrap()
    }

    fn read_identifier(&mut self) -> Token {
        let mut buffer = String::new();
        while !self.is_eof() && self.next_char().is_alphabetic() {
            buffer.push(self.next_char());
            self.position += 1;
        }
        Token::Identifier(buffer)
    }

    fn read_integer(&mut self) -> Token {
        let mut buffer = String::new();
        if self.next_char() == '-' {
            buffer.push(self.next_char());
            self.position += 1;
        }
        while !self.is_eof() && self.next_char().is_numeric() {
            buffer.push(self.next_char());
            self.position += 1;
        }
        let value: i32 = buffer.parse().unwrap();
        Token::Integer(value)
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{Token, Tokenizer};

    #[test]
    fn test_tokenize_single_character_identifier() {
        let mut tokenizer = Tokenizer::new("p");
        assert_eq!(
            tokenizer.tokenize(),
            vec![Token::Identifier(String::from("p"))]
        );
    }

    #[test]
    fn test_tokenize_multi_character_identifier() {
        let mut tokenizer = Tokenizer::new("pi");
        assert_eq!(
            tokenizer.tokenize(),
            vec![Token::Identifier(String::from("pi"))]
        );
    }

    #[test]
    fn test_tokenize_single_digit_integer() {
        let mut tokenizer = Tokenizer::new("4");
        assert_eq!(tokenizer.tokenize(), vec![Token::Integer(4)]);
    }

    #[test]
    fn test_tokenize_multi_digit_integer() {
        let mut tokenizer = Tokenizer::new("42");
        assert_eq!(tokenizer.tokenize(), vec![Token::Integer(42)]);
    }

    #[test]
    fn test_tokenize_negative_integer() {
        let mut tokenizer = Tokenizer::new("-42");
        assert_eq!(tokenizer.tokenize(), vec![Token::Integer(-42)]);
    }

    #[test]
    fn test_tokenize_with_leading_and_trailing_whitespace() {
        let mut tokenizer = Tokenizer::new(" 42 ");
        assert_eq!(tokenizer.tokenize(), vec![Token::Integer(42)]);
    }
}
