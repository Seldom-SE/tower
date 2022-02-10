use std::{
    env::args,
    error::Error,
    fmt::{self, Display, Formatter},
    fs::read_to_string,
    process::exit,
};

#[derive(Debug)]
enum CommandError {
    WrongNumOfArgs,
}

impl Display for CommandError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CommandError::WrongNumOfArgs =>
                    "wrong number of arguments; expected: tower <script>",
            }
        )
    }
}

impl Error for CommandError {}

#[derive(Debug)]
enum LexError {
    InvalidChar { line: u32, c: char },
    InescapableChar { line: u32, c: char },
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LexError::InvalidChar { line, c } =>
                    format!("unexpected character: {c}, line {line}"),
                LexError::InescapableChar { line, c } => format!("cannot escape: {c}, line {line}"),
            }
        )
    }
}

impl Error for LexError {}

#[derive(Debug)]
enum Register {
    A,
    B,
    C,
}

#[derive(Debug)]
enum TokenType {
    NumLiteral(i32),
    CharLiteral(char),
    NumIo,
    CharIo,
    Register(Register),
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Not,
    Equal,
    Greater,
    Less,
    Condition,
    LeftBracket,
    RightBracket,
    Extract,
}

enum LexContext {
    Main,
    NumLiteral {
        num: String,
        lexeme: String,
        line: u32,
    },
    CharLiteral {
        escaped: bool,
        lexeme: String,
        line: u32,
    },
}

#[derive(Debug)]
struct Token {
    token_type: TokenType,
    lexeme: String,
    line: u32,
}

impl Token {
    fn lex(src: String) -> Result<Vec<Self>, Box<dyn Error>> {
        let mut tokens = Vec::new();
        let mut ctx = LexContext::Main;
        let mut line = 0;

        for c in src.chars() {
            if c == '\n' {
                line += 1;
            }

            let mut continue_lexing = true;

            if let LexContext::NumLiteral { num, lexeme, line } = ctx {
                if c.is_whitespace() || c == '-' || c.is_ascii_digit() {
                    ctx = LexContext::NumLiteral {
                        num: if c.is_ascii_digit() {
                            num + &c.to_string()
                        } else {
                            num
                        },
                        lexeme: lexeme + &c.to_string(),
                        line,
                    };
                    continue_lexing = false;
                } else {
                    tokens.push(Token {
                        token_type: TokenType::NumLiteral(
                            num.chars()
                                .into_iter()
                                .filter(|c| !c.is_whitespace())
                                .collect::<String>()
                                .parse::<i32>()?,
                        ),
                        lexeme: ":".to_string() + &num,
                        line,
                    });
                    ctx = LexContext::Main;
                }
            }

            if continue_lexing {
                match ctx {
                    LexContext::Main => {
                        if c.is_whitespace() {
                            continue;
                        }

                        if let Some(token_type) = match c {
                            ':' => {
                                ctx = LexContext::NumLiteral {
                                    num: String::default(),
                                    lexeme: ":".to_string(),
                                    line,
                                };
                                None
                            }
                            ';' => {
                                ctx = LexContext::CharLiteral {
                                    escaped: false,
                                    lexeme: ";".to_string(),
                                    line,
                                };
                                None
                            }
                            '.' => Some(TokenType::NumIo),
                            ',' => Some(TokenType::CharIo),
                            'a' => Some(TokenType::Register(Register::A)),
                            'b' => Some(TokenType::Register(Register::B)),
                            'c' => Some(TokenType::Register(Register::C)),
                            '+' => Some(TokenType::Add),
                            '-' => Some(TokenType::Sub),
                            '*' => Some(TokenType::Mul),
                            '/' => Some(TokenType::Div),
                            '%' => Some(TokenType::Mod),
                            '!' => Some(TokenType::Not),
                            '=' => Some(TokenType::Equal),
                            '>' => Some(TokenType::Greater),
                            '<' => Some(TokenType::Less),
                            '?' => Some(TokenType::Condition),
                            '[' => Some(TokenType::LeftBracket),
                            ']' => Some(TokenType::RightBracket),
                            '#' => Some(TokenType::Extract),
                            c => return Err(Box::new(LexError::InvalidChar { line, c })),
                        } {
                            tokens.push(Token {
                                token_type,
                                lexeme: c.to_string(),
                                line,
                            });
                        }
                    }
                    LexContext::CharLiteral {
                        escaped,
                        lexeme,
                        line,
                    } => {
                        let lexeme = lexeme + &c.to_string();

                        if c.is_whitespace() {
                            ctx = LexContext::CharLiteral {
                                escaped,
                                lexeme,
                                line,
                            };
                            continue;
                        }

                        if escaped {
                            tokens.push(Token {
                                token_type: TokenType::CharLiteral(match c {
                                    's' => ' ',
                                    'n' => '\n',
                                    't' => '\t',
                                    'r' => '\r',
                                    '\\' => '\\',
                                    c => {
                                        return Err(Box::new(LexError::InescapableChar { line, c }))
                                    }
                                }),
                                lexeme,
                                line,
                            });
                            ctx = LexContext::Main;
                        } else {
                            match c {
                                '\\' => {
                                    ctx = LexContext::CharLiteral {
                                        escaped: true,
                                        lexeme,
                                        line,
                                    };
                                }
                                c => {
                                    tokens.push(Token {
                                        token_type: TokenType::CharLiteral(c),
                                        lexeme,
                                        line,
                                    });
                                    ctx = LexContext::Main;
                                }
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }

        Ok(tokens)
    }
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {}", err);
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let cmd_args: Vec<String> = args().collect();

    if cmd_args.len() != 2 {
        Err(Box::new(CommandError::WrongNumOfArgs))
    } else if let Some(path) = cmd_args.last() {
        Token::lex(read_to_string(path)?)?
            .into_iter()
            .for_each(|token| {
                println!("{:?}", token);
            });
        Ok(())
    } else {
        unreachable!()
    }
}
