mod lexer;

use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use lexer::Lexer;
use std::fs;
use structopt::StructOpt;
use std::collections::HashMap;

/// Search for a pattern in a file and display the lines that contain it.
#[derive(StructOpt)]
struct Cli {
    /// The path to the file to read
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::from_args();
    let path = args
        .path
        .to_str()
        .expect("should be a valid path to a file");
    let file_name = args
        .path
        .as_path()
        .file_name()
        .expect("should be a file not a directory")
        .to_str()
        .unwrap();
    let source = fs::read_to_string(path).expect("Unable to read file");
    let mut files = SimpleFiles::new();
    let file_id = files.add(file_name, &source);

    let result = Lexer::new(source.as_str(), file_id);

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let mut symbol_table: HashMap<String, i32> = HashMap::new();

    for token in result {
        if symbol_table.contains_key(&token.get_kind().to_string()) {
            symbol_table.insert(token.get_kind().to_string(), 2);
        } else {
            symbol_table.insert(token.get_kind().to_string(), 1);
        }

        println!("{:?}", token.get_kind());
        
        token
            .to_error()
            .map(|err| err.to_diagnostic())
            .map(|diagnostic| {
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap()
            });
    }

    println!("-----------------------------");
    for (key, value) in symbol_table.iter() {
        println!("{} - {}", key, value);
    }
}
