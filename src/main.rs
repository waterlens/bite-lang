use bite_lang::{core::conversion::SexpWithString, parser::core::parse};
use clap::Parser;
use std::{fs::read_to_string, path::PathBuf};

#[derive(Parser, Debug)]
struct Args {
    path: PathBuf,
}

fn main() {
    let args = Args::parse();
    let input = read_to_string(args.path).unwrap();
    let module = parse(input.as_str()).unwrap();
    let sexp: SexpWithString = (&module).into();
    println!("{sexp}\n\n");
    let module = module.anf();
    let sexp: SexpWithString = (&module).into();
    println!("{sexp}\n\n");
    let module = module.type_propagation();
    let sexp: SexpWithString = (&module).into();
    println!("{sexp}\n\n");
    let module = module.closure_conversion();
    let sexp: SexpWithString = (&module).into();
    println!("{sexp}\n\n");
}
