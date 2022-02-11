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
    CharEof,
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::InvalidChar { line, c } =>
                    format!("unexpected character on line {line}: '{c}'"),
                Self::InescapableChar { line, c } =>
                    format!("cannot escape character on line {line}: '{c}'"),
                Self::CharEof => {
                    "expected character at EOF".to_string()
                }
            }
        )
    }
}

impl Error for LexError {}

#[derive(Clone, Copy, Debug)]
enum Register {
    A,
    B,
    C,
}

impl Register {
    fn character(&self) -> char {
        match self {
            Self::A => 'a',
            Self::B => 'b',
            Self::C => 'c',
        }
    }
}

#[derive(Clone, Debug)]
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
    And,
    Or,
    Condition,
    LeftBracket,
    RightBracket,
    Extract,
}

enum LexContext {
    Main,
    NumLiteral { num: String, line: u32 },
    CharLiteral { escaped: bool, line: u32 },
}

#[derive(Clone, Debug)]
struct Token {
    token_type: TokenType,
    line: u32,
}

impl Token {
    fn lex(src: String) -> Result<Vec<Self>, Box<dyn Error>> {
        let mut tokens = Vec::default();
        let mut ctx = LexContext::Main;
        let mut line = 1;

        for c in src.chars() {
            if c == '\n' {
                line += 1;
            }

            let mut continue_lexing = true;

            if let LexContext::NumLiteral { num, line } = ctx {
                if c.is_whitespace() || c == '-' || c.is_ascii_digit() {
                    ctx = LexContext::NumLiteral {
                        num: if c.is_ascii_digit() {
                            num + &c.to_string()
                        } else {
                            num
                        },
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
                                    line,
                                };
                                None
                            }
                            ';' => {
                                ctx = LexContext::CharLiteral {
                                    escaped: false,
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
                            '&' => Some(TokenType::And),
                            '|' => Some(TokenType::Or),
                            '[' => Some(TokenType::LeftBracket),
                            ']' => Some(TokenType::RightBracket),
                            '#' => Some(TokenType::Extract),
                            c => return Err(Box::new(LexError::InvalidChar { line, c })),
                        } {
                            tokens.push(Token { token_type, line });
                        }
                    }
                    LexContext::CharLiteral { escaped, line } => {
                        if c.is_whitespace() {
                            ctx = LexContext::CharLiteral { escaped, line };
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
                                line,
                            });
                            ctx = LexContext::Main;
                        } else {
                            match c {
                                '\\' => {
                                    ctx = LexContext::CharLiteral {
                                        escaped: true,
                                        line,
                                    };
                                }
                                c => {
                                    tokens.push(Token {
                                        token_type: TokenType::CharLiteral(c),
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

        match ctx {
            LexContext::Main => {}
            LexContext::NumLiteral { num, line } => {
                tokens.push(Token {
                    token_type: TokenType::NumLiteral(
                        num.chars()
                            .into_iter()
                            .filter(|c| !c.is_whitespace())
                            .collect::<String>()
                            .parse::<i32>()?,
                    ),
                    line,
                });
            }
            LexContext::CharLiteral { escaped, line } => return Err(Box::new(LexError::CharEof)),
        }

        Ok(tokens)
    }

    fn character(&self) -> char {
        match &self.token_type {
            TokenType::NumLiteral(_) => ':',
            TokenType::CharLiteral(_) => ';',
            TokenType::NumIo => '.',
            TokenType::CharIo => ',',
            TokenType::Register(register) => register.character(),
            TokenType::Add => '+',
            TokenType::Sub => '-',
            TokenType::Mul => '*',
            TokenType::Div => '/',
            TokenType::Mod => '%',
            TokenType::Not => '!',
            TokenType::Equal => '=',
            TokenType::Greater => '>',
            TokenType::Less => '<',
            TokenType::And => '&',
            TokenType::Or => '|',
            TokenType::Condition => '?',
            TokenType::LeftBracket => '[',
            TokenType::RightBracket => ']',
            TokenType::Extract => '#',
        }
    }
}

#[derive(Debug)]
enum ParseError {
    InvalidStatement(Token),
    InvalidExpression(Token),
    MissingJumpForward { line: u32 },
    MissingJumpBack { line: u32 },
    UnclosedCompression { line: u32 },
    ExpressionEof { line: u32 },
    DuplicateRegister { register: Register, line: u32 },
    InvalidRegister(Token),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::InvalidStatement(token) => format!(
                    "invalid statement on line {}: '{}'",
                    token.line,
                    token.character()
                ),
                Self::InvalidExpression(token) => format!(
                    "invalid expression on line {}: '{}'",
                    token.line,
                    token.character()
                ),
                Self::MissingJumpForward { line } =>
                    format!("missing '[' jump to match ']' jump on line {}", line),
                Self::MissingJumpBack { line } =>
                    format!("missing ']' jump to match '[' jump on line {}", line),
                Self::UnclosedCompression { line } =>
                    format!("unclosed compression expression on line {}", line),
                Self::ExpressionEof { line } =>
                    format!("expected expression at EOF on line {}", line),
                Self::DuplicateRegister { register, line } => format!(
                    "duplicate register on line {}: '{}'",
                    line,
                    register.character(),
                ),
                Self::InvalidRegister(token) => format!(
                    "invalid register on line {}: '{}'",
                    token.line,
                    token.character()
                ),
            }
        )
    }
}

impl Error for ParseError {}

#[derive(Debug, Default)]
struct RegisterSet {
    a: bool,
    b: bool,
    c: bool,
}

impl RegisterSet {
    fn parse(tokens: &[Token], mut index: usize) -> Result<(Self, usize), ParseError> {
        let mut registers = Self::default();
        let line = tokens[index].line;
        index += 1;

        loop {
            let token = tokens
                .get(index)
                .ok_or(ParseError::UnclosedCompression { line })?;

            match &token.token_type {
                TokenType::Register(register) => {
                    if *registers.get(*register) {
                        return Err(ParseError::DuplicateRegister {
                            register: *register,
                            line,
                        });
                    }
                    *registers.get_mut(*register) = true;
                }
                TokenType::RightBracket => break,
                _ => return Err(ParseError::InvalidRegister(token.clone())),
            }

            index += 1;
        }

        Ok((registers, index + 1))
    }

    fn get(&self, register: Register) -> &bool {
        match register {
            Register::A => &self.a,
            Register::B => &self.b,
            Register::C => &self.c,
        }
    }

    fn get_mut(&mut self, register: Register) -> &mut bool {
        match register {
            Register::A => &mut self.a,
            Register::B => &mut self.b,
            Register::C => &mut self.c,
        }
    }
}

#[derive(Debug)]
enum ExpressionType {
    NumLiteral(i32),
    CharLiteral(char),
    ReadNum,
    ReadChar,
    Get(Register),
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mul(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Mod(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
    Equal(Box<Expression>, Box<Expression>),
    Greater(Box<Expression>, Box<Expression>),
    Less(Box<Expression>, Box<Expression>),
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Condition(Box<Expression>, Box<Expression>, Box<Expression>),
    Compress(RegisterSet),
}

#[derive(Debug)]
struct Expression {
    expression_type: ExpressionType,
    line: u32,
}

impl Expression {
    fn parse(
        tokens: &[Token],
        index: usize,
        parent_line: u32,
    ) -> Result<(Self, usize), ParseError> {
        let token = tokens
            .get(index)
            .ok_or(ParseError::ExpressionEof { line: parent_line })?;
        let index = index + 1;

        match token.token_type {
            TokenType::NumLiteral(_)
            | TokenType::CharLiteral(_)
            | TokenType::NumIo
            | TokenType::CharIo
            | TokenType::Register(_) => Ok((
                Self {
                    expression_type: match token.token_type {
                        TokenType::NumLiteral(num) => ExpressionType::NumLiteral(num),
                        TokenType::CharLiteral(c) => ExpressionType::CharLiteral(c),
                        TokenType::NumIo => ExpressionType::ReadNum,
                        TokenType::CharIo => ExpressionType::ReadChar,
                        TokenType::Register(register) => ExpressionType::Get(register),
                        _ => unreachable!(),
                    },
                    line: token.line,
                },
                index,
            )),
            TokenType::Add
            | TokenType::Sub
            | TokenType::Mul
            | TokenType::Div
            | TokenType::Mod
            | TokenType::Equal
            | TokenType::Greater
            | TokenType::Less
            | TokenType::And
            | TokenType::Or => {
                let (left, index) = Self::parse(tokens, index, token.line)?;
                let (right, index) = Self::parse(tokens, index, token.line)?;

                let left = Box::new(left);
                let right = Box::new(right);

                Ok((
                    Self {
                        expression_type: match token.token_type {
                            TokenType::Add => ExpressionType::Add(left, right),
                            TokenType::Sub => ExpressionType::Sub(left, right),
                            TokenType::Mul => ExpressionType::Mul(left, right),
                            TokenType::Div => ExpressionType::Div(left, right),
                            TokenType::Mod => ExpressionType::Mod(left, right),
                            TokenType::Equal => ExpressionType::Equal(left, right),
                            TokenType::Greater => ExpressionType::Greater(left, right),
                            TokenType::Less => ExpressionType::Less(left, right),
                            TokenType::And => ExpressionType::And(left, right),
                            TokenType::Or => ExpressionType::Or(left, right),
                            _ => unreachable!(),
                        },
                        line: token.line,
                    },
                    index,
                ))
            }
            TokenType::Not => {
                let (expression, index) = Self::parse(tokens, index, token.line)?;

                Ok((
                    Self {
                        expression_type: ExpressionType::Not(Box::new(expression)),
                        line: token.line,
                    },
                    index,
                ))
            }
            TokenType::Condition => {
                let (condition, index) = Self::parse(tokens, index, token.line)?;
                let (true_expression, index) = Self::parse(tokens, index, token.line)?;
                let (false_expression, index) = Self::parse(tokens, index, token.line)?;

                Ok((
                    Self {
                        expression_type: ExpressionType::Condition(
                            Box::new(condition),
                            Box::new(true_expression),
                            Box::new(false_expression),
                        ),
                        line: token.line,
                    },
                    index,
                ))
            }
            TokenType::LeftBracket => {
                let (registers, index) = RegisterSet::parse(tokens, index)?;

                Ok((
                    Self {
                        expression_type: ExpressionType::Compress(registers),
                        line: token.line,
                    },
                    index,
                ))
            }
            TokenType::RightBracket | TokenType::Extract => {
                Err(ParseError::InvalidExpression(token.clone()))
            }
        }
    }
}

#[derive(Debug)]
enum StatementType {
    PrintNum(Expression),
    PrintChar(Expression),
    Set(Register, Expression),
    If(Expression, Box<Statement>),
    JumpForward { statements: Vec<Statement> },
    JumpBack,
    Extract(Expression),
}

#[derive(Debug)]
struct Statement {
    statement_type: StatementType,
    line: u32,
}

impl Statement {
    fn parse(
        tokens: &[Token],
        index: usize,
        parent_line: u32,
    ) -> Result<(Self, usize), ParseError> {
        let token = tokens
            .get(index)
            .ok_or(ParseError::MissingJumpBack { line: parent_line })?;
        let index = index + 1;

        match token.token_type {
            TokenType::NumLiteral(_)
            | TokenType::CharLiteral(_)
            | TokenType::Add
            | TokenType::Sub
            | TokenType::Mul
            | TokenType::Div
            | TokenType::Mod
            | TokenType::Not
            | TokenType::Equal
            | TokenType::Greater
            | TokenType::Less
            | TokenType::And
            | TokenType::Or => Err(ParseError::InvalidStatement(token.clone())),
            TokenType::NumIo | TokenType::CharIo | TokenType::Register(_) | TokenType::Extract => {
                let (expression, index) = Expression::parse(tokens, index, token.line)?;
                Ok((
                    Self {
                        statement_type: match token.token_type {
                            TokenType::NumIo => StatementType::PrintNum(expression),
                            TokenType::CharIo => StatementType::PrintChar(expression),
                            TokenType::Register(register) => {
                                StatementType::Set(register, expression)
                            }
                            TokenType::Extract => StatementType::Extract(expression),
                            _ => unreachable!(),
                        },
                        line: token.line,
                    },
                    index,
                ))
            }
            TokenType::Condition => {
                let (expression, index) = Expression::parse(tokens, index, token.line)?;
                let (statement, index) = Self::parse(tokens, index, token.line)?;
                Ok((
                    Self {
                        statement_type: StatementType::If(expression, Box::new(statement)),
                        line: token.line,
                    },
                    index,
                ))
            }
            TokenType::LeftBracket => {
                let mut statements = Vec::default();
                let mut index = index;
                let mut expecting_jump_back = false;

                loop {
                    let (statement, new_index) = Self::parse(tokens, index, token.line)?;
                    if statement.jumps_forward() {
                        expecting_jump_back = true;
                    } else if statement.jumps_back() && !expecting_jump_back {
                        break;
                    } else {
                        expecting_jump_back = false;
                    }
                    index = new_index;
                    statements.push(statement);
                }
                Ok((
                    Self {
                        statement_type: StatementType::JumpForward { statements },
                        line: token.line,
                    },
                    index,
                ))
            }
            TokenType::RightBracket => Ok((
                Self {
                    statement_type: StatementType::JumpBack,
                    line: token.line,
                },
                index,
            )),
        }
    }

    fn parse_program(tokens: Vec<Token>) -> Result<Vec<Self>, ParseError> {
        println!("{tokens:?}");

        let mut statements = Vec::<Self>::default();
        let mut index = 0;
        let mut expecting_jump_back = false;

        while index < tokens.len() {
            let (statement, new_index) = Self::parse(
                &tokens,
                index,
                statements.last().map_or(1, |statement| statement.line),
            )?;
            println!("{statement:?}");
            if statement.jumps_forward() {
                expecting_jump_back = true;
            } else if statement.jumps_back() && !expecting_jump_back {
                return Err(ParseError::MissingJumpForward {
                    line: statement.line,
                });
            } else {
                expecting_jump_back = false;
            }
            index = new_index;
            statements.push(statement);
        }
        Ok(statements)
    }

    fn jumps_forward(&self) -> bool {
        match &self.statement_type {
            StatementType::PrintNum(_)
            | StatementType::PrintChar(_)
            | StatementType::Set(_, _)
            | StatementType::Extract(_)
            | StatementType::JumpBack => false,
            StatementType::If(_, statement) => statement.jumps_forward(),
            StatementType::JumpForward { .. } => true,
        }
    }

    fn jumps_back(&self) -> bool {
        match &self.statement_type {
            StatementType::PrintNum(_)
            | StatementType::PrintChar(_)
            | StatementType::Set(_, _)
            | StatementType::Extract(_)
            | StatementType::JumpForward { .. } => false,
            StatementType::If(_, statement) => statement.jumps_back(),
            StatementType::JumpBack => true,
        }
    }
}

fn main() {
    if let Err(err) = run() {
        eprintln!("Error: {err}");
        exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let cmd_args: Vec<String> = args().collect();

    if cmd_args.len() != 2 {
        Err(Box::new(CommandError::WrongNumOfArgs))
    } else if let Some(path) = cmd_args.last() {
        Statement::parse_program(Token::lex(read_to_string(path)?)?)?
            .into_iter()
            .for_each(|statement| {
                println!("{statement:?}");
            });
        Ok(())
    } else {
        unreachable!()
    }
}
