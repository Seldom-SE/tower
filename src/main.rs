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
enum LexError {}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "lexical error")
    }
}

impl Error for LexError {}

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
        // Token::lex(read_to_string(path)?).for_each(|token| {
        //     println!("{:?}", token);
        // });
        Ok(())
    } else {
        unreachable!()
    }
}
