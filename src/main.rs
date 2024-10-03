use rlox::*;
use std::io::{self, Read, Write};
use std::io::BufReader;
use std::fs::{self, File};
use std::env;
use miette::{IntoDiagnostic, WrapErr};
fn main() -> miette::Result<()> {
    let args = env::args();

    match args.len() {
        1 => run_prompt(),
        2 => run_file(args.skip(1).next().unwrap()),
        _ => Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: rlox [script]")).into_diagnostic().wrap_err_with(|| "usage error")
    }?;
    Ok(())
}

fn run_prompt() -> miette::Result<()> {
    loop {
        print!("> "); io::stdout().flush().into_diagnostic().wrap_err_with(|| "flush err")?;
        let mut line = String::new();
        io::stdin().read_line(&mut line).into_diagnostic().wrap_err_with(|| "read err")?;
        if &line == "\n" { break; }

        run(line)?;
    }
    Ok(())
}

fn run_file(path: String) -> miette::Result<()> {
    // let mut data = String::new();
    // BufReader::new(File::open(path)?).read_to_string(&mut data).into_diagnostic().wrap_err_with(|| "read err");
    // run(data);
    // Ok(())
    let data = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| "read err")?;

    run(data)?;
    Ok(())
}

fn run(source: String) -> miette::Result<()> {
    for token in Lexer::new(&source) {
        let token = token?;
        println!("{token}");
    }
    Ok(())
}