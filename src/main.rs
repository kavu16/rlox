use miette::{IntoDiagnostic, WrapErr};
use rlox::{Lexer, Parser};

use std::env;
use std::fs::{self};
use std::io::{self, Write};
fn main() -> miette::Result<()> {
    let mut args = env::args();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(args.nth(1).unwrap()),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: rlox [script]",
        ))
        .into_diagnostic()
        .wrap_err_with(|| "usage error"),
    }?;
    Ok(())
}

fn run_prompt() -> miette::Result<()> {
    loop {
        print!("> ");
        io::stdout()
            .flush()
            .into_diagnostic()
            .wrap_err_with(|| "flush err")?;
        let mut line = String::new();
        io::stdin()
            .read_line(&mut line)
            .into_diagnostic()
            .wrap_err_with(|| "read err")?;
        if &line == "\n" {
            break;
        }

        run(line)?;
    }
    Ok(())
}

fn run_file(path: String) -> miette::Result<()> {
    let data = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| "read err")?;

    run(data)?;
    Ok(())
}

fn run(source: String) -> miette::Result<()> {
    let parser = Parser::new(&source);
    let exp = parser.expression()?;
    println!("{:?}", exp);
    Ok(())
}
