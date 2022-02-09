use std::{
    env::args,
    fs::read_to_string,
    io::{stdin, stdout, Write},
    process::exit,
};

fn main() {
    let cmd_args: Vec<String> = args().collect();

    if cmd_args.len() > 2 {
        eprintln!("Usage: tower [script]");
        exit(1);
    } else if cmd_args.len() == 1 {
        let mut input = String::default();

        loop {
            print!("> ");
            if let Err(err) = stdout().flush() {
                eprintln!("Error: {err}");
                exit(1);
            }

            input.clear();
            if let Err(err) = stdin().read_line(&mut input) {
                eprintln!("Error: {err}");
                exit(1);
            }

            if input.trim().is_empty() {
                break;
            }

            run(&input);
        }
    } else if let Some(path) = cmd_args.last() {
        match read_to_string(path) {
            Ok(file_text) => run(&file_text),
            Err(err) => {
                eprintln!("Error: {err}");
                exit(1);
            }
        }
        if let Ok(file_text) = read_to_string(path) {
            run(&file_text);
        } else {
            eprintln!("Failed to read file: {}", path);
            exit(1);
        }
    }
}

fn run(code: &str) {
    // Token::lex(code).for_each(|token| {
    //     println!("{:?}", token);
    // });
}
