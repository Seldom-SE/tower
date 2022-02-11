// T
// o
// w by Seldom, 2022
// e
// r

use std::{
    env::args,
    error::Error,
    fmt::{self, Display, Formatter},
    fs::read_to_string,
    io::{stdin, stdout, Write},
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
    fn list() -> &'static [Self] {
        &[Self::A, Self::B, Self::C]
    }

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
                if c.is_whitespace() || c.is_ascii_digit() || (c == '-' && num.trim().is_empty()) {
                    ctx = LexContext::NumLiteral {
                        num: if c.is_whitespace() {
                            num
                        } else {
                            num + &c.to_string()
                        },
                        line,
                    };
                    continue_lexing = false;
                } else {
                    tokens.push(Self {
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
                            tokens.push(Self { token_type, line });
                        }
                    }
                    LexContext::CharLiteral { escaped, line } => {
                        if c.is_whitespace() {
                            ctx = LexContext::CharLiteral { escaped, line };
                            continue;
                        }

                        if escaped {
                            tokens.push(Self {
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
                                    tokens.push(Self {
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
                tokens.push(Self {
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
            LexContext::CharLiteral { .. } => return Err(Box::new(LexError::CharEof)),
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
    ExpressionEof,
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
                Self::ExpressionEof => "expected expression at EOF".to_string(),
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

#[derive(Clone, Debug, Default)]
struct RegisterSet<T> {
    a: T,
    b: T,
    c: T,
}

impl RegisterSet<bool> {
    fn parse(tokens: &[Token], mut index: usize) -> Result<(Self, usize), ParseError> {
        let mut registers = Self::default();
        let line = tokens[index].line;

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
}

impl RegisterSet<bool> {
    fn iter(&self) -> impl Iterator<Item = Register> + '_ {
        Register::list()
            .iter()
            .filter(|register| *self.get(**register))
            .copied()
    }
}

impl<T> RegisterSet<Option<T>> {
    fn iter(&self) -> impl Iterator<Item = (Register, &T)> {
        Register::list()
            .iter()
            .filter_map(|register| self.get(*register).as_ref().map(|value| (*register, value)))
    }
}

impl<T> RegisterSet<T> {
    fn get(&self, register: Register) -> &T {
        match register {
            Register::A => &self.a,
            Register::B => &self.b,
            Register::C => &self.c,
        }
    }

    fn get_mut(&mut self, register: Register) -> &mut T {
        match register {
            Register::A => &mut self.a,
            Register::B => &mut self.b,
            Register::C => &mut self.c,
        }
    }
}

#[derive(Debug)]
enum Expression {
    NumLiteral(i32),
    CharLiteral(char),
    ReadNum,
    ReadChar,
    Get(Register),
    Add(Box<Self>, Box<Self>),
    Sub(Box<Self>, Box<Self>),
    Mul(Box<Self>, Box<Self>),
    Div(Box<Self>, Box<Self>),
    Mod(Box<Self>, Box<Self>),
    Not(Box<Self>),
    Equal(Box<Self>, Box<Self>),
    Greater(Box<Self>, Box<Self>),
    Less(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Condition(Box<Self>, Box<Self>, Box<Self>),
    Compress(RegisterSet<bool>),
}

impl Expression {
    fn parse(tokens: &[Token], index: usize) -> Result<(Self, usize), ParseError> {
        let token = tokens.get(index).ok_or(ParseError::ExpressionEof)?;
        let index = index + 1;

        match token.token_type {
            TokenType::NumLiteral(_)
            | TokenType::CharLiteral(_)
            | TokenType::NumIo
            | TokenType::CharIo
            | TokenType::Register(_) => Ok((
                match token.token_type {
                    TokenType::NumLiteral(num) => Self::NumLiteral(num),
                    TokenType::CharLiteral(c) => Self::CharLiteral(c),
                    TokenType::NumIo => Self::ReadNum,
                    TokenType::CharIo => Self::ReadChar,
                    TokenType::Register(register) => Self::Get(register),
                    _ => unreachable!(),
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
                let (left, index) = Self::parse(tokens, index)?;
                let (right, index) = Self::parse(tokens, index)?;

                let left = Box::new(left);
                let right = Box::new(right);

                Ok((
                    match token.token_type {
                        TokenType::Add => Self::Add(left, right),
                        TokenType::Sub => Self::Sub(left, right),
                        TokenType::Mul => Self::Mul(left, right),
                        TokenType::Div => Self::Div(left, right),
                        TokenType::Mod => Self::Mod(left, right),
                        TokenType::Equal => Self::Equal(left, right),
                        TokenType::Greater => Self::Greater(left, right),
                        TokenType::Less => Self::Less(left, right),
                        TokenType::And => Self::And(left, right),
                        TokenType::Or => Self::Or(left, right),
                        _ => unreachable!(),
                    },
                    index,
                ))
            }
            TokenType::Not => {
                let (expression, index) = Self::parse(tokens, index)?;

                Ok((Self::Not(Box::new(expression)), index))
            }
            TokenType::Condition => {
                let (condition, index) = Self::parse(tokens, index)?;
                let (true_expression, index) = Self::parse(tokens, index)?;
                let (false_expression, index) = Self::parse(tokens, index)?;

                Ok((
                    Self::Condition(
                        Box::new(condition),
                        Box::new(true_expression),
                        Box::new(false_expression),
                    ),
                    index,
                ))
            }
            TokenType::LeftBracket => {
                let (registers, index) = RegisterSet::parse(tokens, index)?;

                Ok((Self::Compress(registers), index))
            }
            TokenType::RightBracket | TokenType::Extract => {
                Err(ParseError::InvalidExpression(token.clone()))
            }
        }
    }

    fn evaluate(&self, ctx: &mut Context) -> Value {
        match self {
            Self::NumLiteral(num) => Value::Num(*num),
            Self::CharLiteral(c) => Value::Num(*c as i32),
            Self::ReadNum => {
                let (num, buffer) = loop {
                    let mut num = None;
                    let mut buffer = None;

                    for c in ctx.buffer.chars() {
                        if let Some(num) = num.as_mut() {
                            if let Some(buffer) = buffer.as_mut() {
                                *buffer = format!("{buffer}{c}");
                            } else if c.is_ascii_digit() {
                                *num = format!("{num}{c}");
                            } else {
                                buffer = Some(c.to_string());
                            }
                        } else if c.is_ascii_digit() || c == '-' {
                            num = Some(c.to_string());
                        }
                    }

                    if let Some(num) = num {
                        break (
                            num.parse::<i32>().unwrap(),
                            buffer.unwrap_or_else(|| "".to_string()),
                        );
                    }

                    ctx.read();
                };

                ctx.buffer = buffer;
                Value::Num(num)
            }
            Self::ReadChar => {
                let buffer_str = ctx.buffer.clone();
                let mut chars = buffer_str.chars();
                let num = Value::Num(chars.next().unwrap_or_else(|| {
                    ctx.read();
                    chars = ctx.buffer.chars();
                    chars.next().unwrap()
                }) as i32);
                ctx.buffer = chars.as_str().to_string();
                num
            }
            Self::Get(register) => ctx.registers.get(*register).clone(),
            Self::Add(left, right)
            | Self::Sub(left, right)
            | Self::Mul(left, right)
            | Self::Div(left, right)
            | Self::Mod(left, right)
            | Self::Greater(left, right)
            | Self::Less(left, right) => Value::Num(if let Value::Num(left) = left.evaluate(ctx) {
                if let Value::Num(right) = right.evaluate(ctx) {
                    match self {
                        Self::Add(..) => left + right,
                        Self::Sub(..) => left - right,
                        Self::Mul(..) => left * right,
                        Self::Div(..) => left / right,
                        Self::Mod(..) => left % right,
                        Self::Greater(..) => (left > right) as i32,
                        Self::Less(..) => (left < right) as i32,
                        _ => unreachable!(),
                    }
                } else {
                    0
                }
            } else {
                0
            }),
            Self::Not(expression) => Value::Num((!expression.evaluate(ctx).truthy()) as i32),
            Self::Equal(left, right) => {
                Value::Num(match (left.evaluate(ctx), right.evaluate(ctx)) {
                    (Value::Num(left), Value::Num(right)) => (left == right) as i32,
                    (Value::Num(_), Value::Archive(_)) | (Value::Archive(_), Value::Num(_)) => 0,
                    (Value::Archive(_), Value::Archive(_)) => 1,
                })
            }
            Self::And(left, right) => Value::Num(if left.evaluate(ctx).truthy() {
                right.evaluate(ctx).truthy() as i32
            } else {
                0
            }),
            Self::Or(left, right) => Value::Num(if left.evaluate(ctx).truthy() {
                1
            } else {
                right.evaluate(ctx).truthy() as i32
            }),
            Self::Condition(condition, true_expression, false_expression) => {
                if condition.evaluate(ctx).truthy() {
                    true_expression.evaluate(ctx)
                } else {
                    false_expression.evaluate(ctx)
                }
            }
            Self::Compress(registers) => {
                let mut archive = RegisterSet::default();
                for register in registers.iter() {
                    *archive.get_mut(register) = Some(ctx.registers.get(register).clone());
                }
                Value::Archive(Box::new(archive))
            }
        }
    }
}

#[derive(Debug)]
enum Statement {
    PrintNum(Expression),
    PrintChar(Expression),
    Set(Register, Expression),
    If(Expression, Box<Self>),
    JumpForward { statements: Vec<Self> },
    JumpBack,
    Extract(Expression),
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
                let (expression, index) = Expression::parse(tokens, index)?;
                Ok((
                    match token.token_type {
                        TokenType::NumIo => Self::PrintNum(expression),
                        TokenType::CharIo => Self::PrintChar(expression),
                        TokenType::Register(register) => Self::Set(register, expression),
                        TokenType::Extract => Self::Extract(expression),
                        _ => unreachable!(),
                    },
                    index,
                ))
            }
            TokenType::Condition => {
                let (expression, index) = Expression::parse(tokens, index)?;
                let (statement, index) = Self::parse(tokens, index, token.line)?;
                Ok((Self::If(expression, Box::new(statement)), index))
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
                Ok((Self::JumpForward { statements }, index))
            }
            TokenType::RightBracket => Ok((Self::JumpBack, index)),
        }
    }

    fn parse_list(tokens: Vec<Token>) -> Result<Vec<Self>, ParseError> {
        let mut statements = Vec::<Self>::default();
        let mut index = 0;
        let mut expecting_jump_back = false;

        while index < tokens.len() {
            let (statement, new_index) = Self::parse(&tokens, index, tokens[index].line)?;

            if statement.jumps_forward() {
                expecting_jump_back = true;
            } else if statement.jumps_back() && !expecting_jump_back {
                return Err(ParseError::MissingJumpForward {
                    line: tokens[index].line,
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
        match self {
            Self::PrintNum(_)
            | Self::PrintChar(_)
            | Self::Set(_, _)
            | Self::Extract(_)
            | Self::JumpBack => false,
            Self::If(_, statement) => statement.jumps_forward(),
            Self::JumpForward { .. } => true,
        }
    }

    fn jumps_back(&self) -> bool {
        match self {
            Self::PrintNum(_)
            | Self::PrintChar(_)
            | Self::Set(_, _)
            | Self::Extract(_)
            | Self::JumpForward { .. } => false,
            Self::If(_, statement) => statement.jumps_back(),
            Self::JumpBack => true,
        }
    }

    fn run(&self, ctx: &mut Context, bypass_if: bool) -> isize {
        match self {
            Self::PrintNum(expression) => {
                print!("{}", expression.evaluate(ctx).as_num());
                stdout().flush().unwrap();
                1
            }
            Self::PrintChar(expression) => {
                print!("{}", expression.evaluate(ctx).as_char());
                stdout().flush().unwrap();
                1
            }
            Self::Set(register, expression) => {
                *ctx.registers.get_mut(*register) = expression.evaluate(ctx);
                1
            }
            Self::If(expression, statement) => {
                if !bypass_if && expression.evaluate(ctx).truthy() {
                    statement.run(ctx, bypass_if)
                } else {
                    if let Self::JumpForward { statements } = &**statement {
                        ctx.run(statements);
                    }
                    1
                }
            }
            Self::JumpForward { .. } => 2,
            Self::JumpBack => -1,
            Self::Extract(expression) => {
                if let Value::Archive(archive) = expression.evaluate(ctx) {
                    for (register, value) in archive.iter() {
                        *ctx.registers.get_mut(register) = value.clone();
                    }
                }
                1
            }
        }
    }
}

#[derive(Clone)]
enum Value {
    Num(i32),
    Archive(Box<RegisterSet<Option<Self>>>),
}

impl Default for Value {
    fn default() -> Self {
        Self::Num(0)
    }
}

impl Value {
    fn truthy(&self) -> bool {
        match self {
            Self::Num(num) => *num != 0,
            Self::Archive(_) => true,
        }
    }

    fn as_num(&self) -> String {
        match self {
            Self::Num(num) => num.to_string(),
            Self::Archive(_) => "".to_string(),
        }
    }

    fn as_char(&self) -> String {
        match self {
            Self::Num(num) => match char::from_u32(*num as u32) {
                Some(c) => c.to_string(),
                None => "�".to_string(),
            },
            Self::Archive(_) => "".to_string(),
        }
    }
}

#[derive(Default)]
struct Context {
    registers: RegisterSet<Value>,
    buffer: String,
}

impl Context {
    fn run(&mut self, statements: &[Statement]) {
        let mut index = 0;
        let mut bypass_if = false;

        while let Some(statement) = statements.get(index) {
            let offset = statement.run(self, bypass_if);
            bypass_if = offset < 0;
            index = (index as isize + offset) as usize;
        }
    }

    fn read(&mut self) {
        stdin().read_line(&mut self.buffer).unwrap();
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
        Context::default().run(&Statement::parse_list(Token::lex(read_to_string(path)?)?)?);
        Ok(())
    } else {
        unreachable!()
    }
}
