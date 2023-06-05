#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    // Keywords
    Int,
    Char,
    If,
    Else,
    While,
    For,
    Return,
    Void,
    //Switch,
    //Case,
    //Struct,
    //Union,

    // Identifiers
    Identifier(String),
    // Constants / Literals
    IntConst(i64),
    //FloatConst(f64),
    CharConst(char),
    StringLiteral(String),

    // Operators
    Plus,         // "+"
    Minus,        // "-"
    Star,         // "*"
    Slash,        // "/"
    Percent,      // "%"
    Equal,        // "="
    Less,         // "<"
    Greater,      // ">"
    EqualEqual,   // "=="
    NotEqual,     // "!="
    LessEqual,    // "<="
    GreaterEqual, // ">="
    AndAnd,       // "&&"
    OrOr,         // "||"
    Not,          // "!"
    And,          // "&"
    Or,           // "|"
    Xor,          // "^"
    Tilde,        // "~"
    ShiftLeft,    // "<<"
    ShiftRight,   // ">>"
    PlusPlus,     // "++"
    MinusMinus,   // "--"
    Sizeof,       // "sizeof"

    // Punctuation
    Semicolon,    // ";"
    Comma,        // ","
    OpenBrace,    // "{"
    CloseBrace,   // "}"
    OpenParen,    // "("
    CloseParen,   // ")"
    OpenBracket,  // "["
    CloseBracket, // "]"
}

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    cur_line: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new<'b>(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input,
            pos: 0,
            cur_line: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }
    pub fn next_token(&mut self) -> Option<TokenType> {
        self.skip_whitespace();
        let token = match self.ch {
            ';' => TokenType::Semicolon,
            '{' => TokenType::OpenBrace,
            '}' => TokenType::CloseBrace,
            '(' => TokenType::OpenParen,
            ')' => TokenType::CloseParen,
            '[' => TokenType::OpenBrace,
            ']' => TokenType::CloseBrace,

            //Keywords
            'a'..='z' | 'A'..='Z' | '_' => {
                let identifier = self.get_keyword();
                match identifier.as_str() {
                    "int" => TokenType::Int,
                    "if" => TokenType::If,
                    "return" => TokenType::Return,
                    "else" => TokenType::Else,
                    _ => TokenType::Identifier(identifier),
                }
            }
            _ => todo!("we need to implement this...."),
        };
        self.read_char();
        Some(token)
    }

    fn read_char(&mut self) {
        self.ch = if self.pos >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.pos).unwrap()
        };
        self.pos += 1;
    }
    fn peek_char(self) -> char {
        if self.pos >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.pos).unwrap()
        }
    }
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
    fn get_keyword(&mut self) -> String {
        let start: usize = self.pos - 1;
        while self.ch.is_ascii_alphabetic() || self.ch == '_' {
            self.read_char();
        }
        self.input[start..self.pos].to_string()
    }
}
