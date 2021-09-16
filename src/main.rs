mod ast;
mod lexer;
mod parser;
mod semantic;

use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use lexer::Lexer;
use parser::parse;
use semantic::TypeCheckable;
// use std::collections::HashMap;
use std::fs;
use std::rc::Rc;
use structopt::StructOpt;

use crate::semantic::Scope;
#[macro_use]
extern crate lalrpop_util;
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

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    let tokens = Lexer::new(source.as_str(), file_id);

    match parse(tokens) {
        Ok(ast) => {
            println!("Successfully parsed!\n{:?}", ast);
            let result = ast.type_check(Scope::new_global());
            println!("Succesfully type-checked! {:?}", result);
        }
        Err(err) => term::emit(&mut writer.lock(), &config, &files, &err.to_diagnostic()).unwrap(),
    };

    // let mut symbol_table: HashMap<String, i32> = HashMap::new();
    // for token in result {
    //     match token.kind {
    //         lexer::TokenKind::Identifier(name) => {
    //             *symbol_table.entry(name.to_string()).or_insert(0) += 1;
    //         }
    //         _ => {}
    //     }
    // }
}
