use std::env;

extern crate static_site;
use static_site::parse_file;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let html = parse_file(filename);

    println!("{}", html);
}
