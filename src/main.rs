mod ast;
mod codegen;
mod lexer;
mod parser;
mod semantic;

use codegen::CodeGeneratable;
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use lexer::Lexer;
use parser::parse;
use semantic::TypeCheckable;
// use std::collections::HashMap;
use std::fs;

use structopt::StructOpt;

use crate::codegen::Context;
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

    // lexical analysis
    let tokens = Lexer::new(source.as_str(), file_id);

    // syntactic analysis
    let ast = parse(tokens)
        .map_err(|err| {
            term::emit(&mut writer.lock(), &config, &files, &err.to_diagnostic()).unwrap()
        })
        .unwrap();
    println!("Successfully parsed!\n{:?}\n", ast);

    // semantic analysis
    let (global_scope, _) = ast
        .type_check(Scope::new_global())
        .map_err(|err| {
            // term::emit(&mut writer.lock(), &config, &files, &err.to_diagnostic()).unwrap()
            panic!("{:?}", err);
        })
        .unwrap();

    println!("Succesfully type-checked!");
    // these are always true, because if there is any type-checking error
    // they will not print whatsoever, because it will panic above
    println!("Arithmetic expressions: ok");
    println!("Variable declarations inside scope: ok");
    println!("Every break inside a for: ok");

    println!("Scope: {:#?}", global_scope);

    // IR generation(generating 3-address intermediate representation code)
    let instructions = ast.generate_code(&mut Context::new());
    println!(
        "Succesfully generated code: \n{}",
        instructions
            .into_iter()
            .map(|i| i.to_string())
            .filter(|s| s.len() > 0)
            .collect::<Vec<String>>()
            .join("\n")
    );
}
