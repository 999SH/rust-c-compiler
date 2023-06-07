#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    EOF,
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
    Newline,
    Tab,
    EscapeChar(char),

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
            cur_line: 1,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }
    pub fn next_token(&mut self) -> Option<TokenType> {
        self.skip_whitespace();
        while self.ch == '\n' {
            self.cur_line += 1;
            self.read_char();
            self.skip_whitespace();
        }
        let token = match self.ch {
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '{' => TokenType::OpenBrace,
            '}' => TokenType::CloseBrace,
            '(' => TokenType::OpenParen,
            ')' => TokenType::CloseParen,
            '[' => TokenType::OpenBrace,
            ']' => TokenType::CloseBrace,

            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Star,
            '/' => TokenType::Slash,
            '=' => TokenType::Equal,

            //Keywords
            'a'..='z' | 'A'..='Z' | '_' => {
                let identifier: String = self.get_keyword();
                match identifier.as_str() {
                    "int" => TokenType::Int,
                    "if" => TokenType::If,
                    "return" => TokenType::Return,
                    "else" => TokenType::Else,
                    _ => TokenType::Identifier(identifier),
                }
            }
            //Numbers
            '0'..='9' => {
                let int: i64 = self.get_int();
                TokenType::IntConst(int)
            }
            //Newline


            //End of file
            '\0' => TokenType::EOF,

            _ => todo!("need to implement this...."),
        };
        self.read_char();
        Some(token)
    }

    fn read_char(&mut self) {
        if self.pos >= self.input.len() {
            self.ch = '\0'
        } else {
            self.ch = self.input.chars().nth(self.pos).unwrap()
        };
        self.pos += 1;
    }
    fn peek_char(&self) -> char {
        if self.pos >= self.input.len() {
            '\0'
        } else {
            self.input.chars().nth(self.pos).unwrap()
        }
    }
    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' {
            self.read_char();
        }
    }
    fn get_keyword(&mut self) -> String {
        let start: usize = self.pos - 1;
        while self.peek_char().is_ascii_alphabetic() || self.peek_char() == '_' {
            self.read_char();
        }
        self.input[start..self.pos].to_string()
    }
    fn get_int(&mut self) -> i64 {
        let start: usize = self.pos - 1;
        while self.peek_char().is_ascii_digit() || self.peek_char() == '_' {
            self.read_char();
        }
        self.input[start..self.pos]
            .parse()
            .expect("Failed to parse integer")
    }
}
