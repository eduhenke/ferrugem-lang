mod lexer;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use lexer::lex;
use std::fs;
use structopt::StructOpt;

use crate::lexer::Token;

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
    let result = lex(source.as_str());

    let mut files = SimpleFiles::new();
    let file_id = files.add(file_name, &source);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for (token, range) in result {
        println!("{:?}", token);

        match token {
            Token::Error => {
                let diagnostic = Diagnostic::error()
                    .with_message("lexical error")
                    .with_labels(vec![
                        Label::primary(file_id, range).with_message("invalid symbol")
                    ]);
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            }
            _ => {}
        };
    }
}
