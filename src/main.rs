mod lexer;

use lexer::lex;
use std::fs;

use structopt::StructOpt;

/// Search for a pattern in a file and display the lines that contain it.
#[derive(StructOpt)]
struct Cli {
    /// The path to the file to read
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::from_args();
    let data = fs::read_to_string(args.path.to_str().unwrap()).expect("Unable to read file");
    let result = lex(data.as_str());

    for token in result {
        println!("{:?}", token);
    }
}
