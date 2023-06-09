#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    EOF,
    // Keywords
    Int,
    Long,
    Double,
    Unsigned,
    Typedef,
    Short,
    Const,
    Extern,
    Restrict,
    Char,
    If,
    Else,
    While,
    For,
    Return,
    Void,
    Switch,
    Case,
    Struct,
    Union,
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
    LineDirective(i64, String, Vec<i64>),

    // Operators
    Plus,         // "+" X
    Minus,        // "-" X
    Star,         // "*" X
    Slash,        // "/" X
    SlashSlash,   // "//" X
    Percent,      // "%" X
    Equal,        // "=" X
    Less,         // "<" X
    Greater,      // ">" X
    EqualEqual,   // "==" X
    NotEqual,     // "!=" X
    LessEqual,    // "<=" X
    GreaterEqual, // ">=" X
    AndAnd,       // "&&"
    OrOr,         // "||"
    Not,          // "!"
    Ampersand,    // "&"
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
    Hashtag,      // "#"
    Dot,          // "."
    Quote,        // """
    SingleQuote,  // "'"
    Colon,        // ":"
    ForwardSlash, // "\"
    QuestionMark, // "?"
    SlashEquals,  // "/="
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
        while self.ch == '/' || self.ch == '\n' {
            if self.peek_char() != '/' && self.peek_char() != '*' && self.ch != '\n'{
                break;
            }
            match self.ch {
                '\n' => {
                    self.cur_line += 1;
                    self.read_char();
                    self.skip_whitespace();
                }
                '/' => {
                    //Comment
                    self.read_char();
                    match self.ch {
                        '/' => {
                            while self.ch != '\n' && self.ch != '\0' {
                                self.read_char();
                            }
                        }
                        '*' => loop {
                            while self.ch != '*' && self.ch != '\0' {
                                self.read_char();
                            }
                            if self.ch == '\0' {
                                break;
                            }
                            self.read_char();
                            if self.ch == '/' {
                                self.read_char();
                                break;
                            }
                        },
                        _ => {}
                    }
                }
                /* Multi line comment */
                _ => {}
            }
        }
        self.skip_whitespace();
        let token = match self.ch {
            ';' => TokenType::Semicolon,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '{' => TokenType::OpenBrace,
            '}' => TokenType::CloseBrace,
            '(' => TokenType::OpenParen,
            ')' => TokenType::CloseParen,
            '[' => TokenType::OpenBracket,
            ']' => TokenType::CloseBracket,

            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Star,
            '/' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenType::SlashEquals
                }
                _ => TokenType::Slash,
            },
            '=' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenType::EqualEqual
                }
                _ => TokenType::Equal,
            },
            '!' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenType::NotEqual
                }
                _ => TokenType::Not,
            },
            '%' => TokenType::Percent,
            '<' => TokenType::Less,
            '>' => match self.peek_char() {
                '=' => {
                    self.read_char();
                    TokenType::GreaterEqual
                }
                '>' => {
                    self.read_char();
                    TokenType::ShiftRight
                }
                _ => TokenType::Greater,
            },
            //'<=' => TokenType::LessEqual,
            //'<<' => TokenType::ShiftLeft,
            '&' => TokenType::Ampersand,
            '|' => TokenType::Or,
            '^' => TokenType::Xor,
            //Keywords
            'a'..='z' | 'A'..='Z' | '_' => {
                let identifier: String = self.get_keyword();
                match identifier.as_str() {
                    "void" => TokenType::Void,
                    "int" => TokenType::Int,
                    "long" => TokenType::Long,
                    "double" => TokenType::Double,
                    "unsigned" => TokenType::Unsigned,
                    "const" => TokenType::Const,
                    "short" => TokenType::Short,
                    "extern" => TokenType::Extern,
                    "restrict" => TokenType::Restrict,
                    "typedef" => TokenType::Typedef,
                    "char" => TokenType::Char,
                    "if" => TokenType::If,
                    "return" => TokenType::Return,
                    "else" => TokenType::Else,
                    "while" => TokenType::While,
                    "for" => TokenType::For,
                    "switch" => TokenType::Switch,
                    "case" => TokenType::Case,
                    "struct" => TokenType::Struct,
                    "union" => TokenType::Union,

                    _ => TokenType::Identifier(identifier),
                }
            }
            //Numbers
            '0'..='9' => {
                let int: i64 = self.get_int();
                TokenType::IntConst(int)
            }

            '#' => {
                self.read_char();
                if self.ch.is_numeric() {
                    let line_number = self.get_int();
                    let file_name = match self.ch {
                        '\"' => Some(self.get_string()),
                        _ => None,
                    };
                    let mut extra_numbers = Vec::new();
                    while self.ch.is_numeric(){
                        extra_numbers.push(self.get_int())
                    }
                    TokenType::LineDirective(line_number, file_name.unwrap(), extra_numbers)
                } //.expect("Expected valid filepath")
                else {
                    TokenType::Hashtag
                }
            },
            '"' => TokenType::Quote,
            '\'' => TokenType::SingleQuote,
            ':' => TokenType::Colon,
            '\\' => TokenType::ForwardSlash,
            '~' => TokenType::Tilde,
            '?' => TokenType::QuestionMark,

            //End of file
            '\0' => TokenType::EOF,

            _ => todo!("need to implement this..{:?}", self.ch),
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
            if self.peek_char().is_ascii_alphabetic() {
                panic!("Invalid integer!")
            }
        }
        self.input[start..self.pos]
            .replace("_", "") // remove underscores
            .parse()
            .expect("Failed to parse integer")
    }
    fn get_string(&mut self) -> String {
        let mut result = String::new();
        if self.ch != '"' {
            return result;
        }
        loop {
            self.read_char(); // Advance to the next character
            if self.ch == '"' || self.ch == '\0' {
                break; // We've reached the end of the string
            }
            result.push(self.ch);
        }
        // Advance past the closing quote
        if self.ch == '"' {
            self.read_char();
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lexer_correctly_tokenizes_keywords() {
        let input = "int if else return";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Some(TokenType::Int));
        assert_eq!(lexer.next_token(), Some(TokenType::If));
        assert_eq!(lexer.next_token(), Some(TokenType::Else));
        assert_eq!(lexer.next_token(), Some(TokenType::Return));
    }

    #[test]
    fn lexer_correctly_tokenizes_identifiers() {
        let input = "foo bar baz";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next_token(),
            Some(TokenType::Identifier("foo".to_string()))
        );
        assert_eq!(
            lexer.next_token(),
            Some(TokenType::Identifier("bar".to_string()))
        );
        assert_eq!(
            lexer.next_token(),
            Some(TokenType::Identifier("baz".to_string()))
        );
    }

    #[test]
    fn lexer_correctly_tokenizes_operators() {
        let input = "+ - * / =";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Some(TokenType::Plus));
        assert_eq!(lexer.next_token(), Some(TokenType::Minus));
        assert_eq!(lexer.next_token(), Some(TokenType::Star));
        assert_eq!(lexer.next_token(), Some(TokenType::Slash));
        assert_eq!(lexer.next_token(), Some(TokenType::Equal));
    }

    #[test]
    fn lexer_correctly_tokenizes_integer_constants() {
        let input = "123 456 789";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Some(TokenType::IntConst(123)));
        assert_eq!(lexer.next_token(), Some(TokenType::IntConst(456)));
        assert_eq!(lexer.next_token(), Some(TokenType::IntConst(789)));
    }

    #[test]
    fn lexer_correctly_handles_whitespace() {
        let input = "  foo   bar\tbaz\nqux";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.next_token(),
            Some(TokenType::Identifier("foo".to_string()))
        );
        assert_eq!(
            lexer.next_token(),
            Some(TokenType::Identifier("bar".to_string()))
        );
        assert_eq!(
            lexer.next_token(),
            Some(TokenType::Identifier("baz".to_string()))
        );
        assert_eq!(
            lexer.next_token(),
            Some(TokenType::Identifier("qux".to_string()))
        );
    }

    #[test]
    fn lexer_asserts_equals() {
        let input = "== =";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Some(TokenType::EqualEqual));
        assert_eq!(lexer.next_token(), Some(TokenType::Equal));
    }
    #[test]
    fn lexer_asserts_not_equals() {
        let input = "!= !";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Some(TokenType::NotEqual));
        assert_eq!(lexer.next_token(), Some(TokenType::Not));
    }

    #[test]
    fn lexer_asserts_greater_and_co() {
        let input = "> >> >=";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Some(TokenType::Greater));
        assert_eq!(lexer.next_token(), Some(TokenType::ShiftRight));
        assert_eq!(lexer.next_token(), Some(TokenType::GreaterEqual));
    }
}
