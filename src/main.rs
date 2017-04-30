extern crate toml;
extern crate clap;
extern crate rustache;
#[macro_use] extern crate serde_derive;

use rustache::{HashBuilder, Render};
use toml::Value;
use std::io::Cursor;

#[derive(Debug, Deserialize)]
struct Author {
    name: Option<String>,
    email: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Config {
    license: Option<String>,
    author: Option<Author>,
}

fn main() {
    // parse the toml
    let toml_str = r#"
        license = "BSD3"

        [author]
        name = "vanessa"
        email = "vamchale@gmail.com"
    "#;
    let decoded: Config = toml::from_str(toml_str).unwrap();
    println!("{:#?}", decoded);
    
    // Renders the given template string
    let data = HashBuilder::new().insert("name", "Vanessa McHale");
    let mut out = Cursor::new(Vec::new());
    data.render("Hello {{ name }}", &mut out).unwrap();
    println!("{}", String::from_utf8(out.into_inner()).unwrap());
}
