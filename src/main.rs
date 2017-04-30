#[macro_use] extern crate serde_derive;
#[macro_use] extern crate clap;

extern crate time;
extern crate toml;
extern crate rustache;

use rustache::{HashBuilder, Render};
use time::now;
use std::io::Cursor;
use std::io::prelude::*;
use std::fs::File;
use clap::App;

#[derive(Debug, Deserialize)]
struct Author {
    name: Option<String>,
    email: Option<String>,
}

#[derive(Debug, Deserialize)]
struct Config {
    license: Option<String>,
    version_control: Option<String>,
    author: Option<Author>,
}

#[derive(Debug, Deserialize)]
struct Directory {
    files: Option<Vec<String>>,
    directories: Option<Vec<String>>,
    templates: Option<Vec<String>>,
}

fn main() {
    // command-line parser
    let yaml = load_yaml!("options-en.yml");
    let matches = App::from_yaml(yaml).version(crate_version!()).get_matches();

    // set path to .pi.toml
    let mut path = std::env::home_dir().expect("Couldn't determine home directory.");
    path.push(".pi.toml");

    // read config file
    let mut file = File::open(path)
        .expect("File could not be read.");
    let mut toml_str = String::new();
    file.read_to_string(&mut toml_str)
        .expect("File read failed");
    let decoded: Config = toml::from_str(&toml_str).unwrap();

    // read template.toml
    let mut template_path = matches
        .value_of("directory")
        .expect("Failed to supply a required argument").to_string();
    template_path.push_str("/template.toml");
    println!("{}",template_path);
    let template_file = File::open(template_path);
    let mut template = String::new();
    template_file.expect("Failed to open file")
        .read_to_string(&mut template)
        .expect("File read failed");
    let parsed_template: Directory = toml::from_str(&template).unwrap();
   
    // get project name
    let project = matches
        .value_of("directory")
        .expect("Failed to supply a required argument");

    let project_hash = HashBuilder::new().insert("project",project);
    let substitutions: Vec<String> = parsed_template.files.expect("need some files").into_iter()
                               .map(|file| { let mut o = Cursor::new(Vec::new());
                                   project_hash.render(&file, &mut o).unwrap();
                                   String::from_utf8(o.into_inner()).unwrap()}).collect();
    println!("{:?}",substitutions);

    //get year
    let year = now().tm_year;

    // Renders the given template string
    let data = HashBuilder::new().insert("name", "Vanessa McHale").insert("year", year + 1900);
    let mut out = Cursor::new(Vec::new());
    data.render("Copyright {{ name }} (c) {{ year }}", &mut out).unwrap();
    println!("{}", String::from_utf8(out.into_inner()).unwrap());
}
