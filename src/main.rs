#[macro_use] extern crate serde_derive;
#[macro_use] extern crate clap;

extern crate time;
extern crate toml;
extern crate rustache;

use std::process::Command;
use rustache::{HashBuilder, Render};
use time::now;
use std::io::Cursor;
use std::io::prelude::*;
use std::fs::File;
use std::fs;
use clap::App;

#[derive(Debug, Deserialize)]
struct Author {
    name: String,
    email: String,
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
    let template_file = File::open(template_path);
    let mut template = String::new();
    template_file.expect("Failed to open file")
        .read_to_string(&mut template)
        .expect("File read failed");
    let parsed_template: Directory = toml::from_str(&template).unwrap();
   
    // get project director
    let project = matches
        .value_of("directory")
        .expect("Failed to supply a required argument");

    // get project name
    let name = matches
        .value_of("name")
        .expect("Failed to supply a required argument");

    let _ = fs::create_dir(name);
    let _ = parsed_template.directories.expect("need some directories").into_iter()
        .map(|dir| { let mut subdir = name.to_string() ; 
            subdir.push('/') ;
            subdir.push_str(&dir) ; 
            fs::create_dir(subdir) } ).count();

    //get year
    let year = now().tm_year;

    let project_hash = HashBuilder::new().insert("project",project).insert("year", year + 1900);
    let substitutions: Vec<String> = parsed_template.files.expect("need some files").into_iter()
                               .map(|file| { let mut o = Cursor::new(Vec::new());
                                   project_hash.render(&file, &mut o).unwrap();
                                   String::from_utf8(o.into_inner()).unwrap()}).collect();

    let _ = substitutions.into_iter()
        .map(|path| { let mut full_path = name.to_string() ; 
            full_path.push('/') ;
            full_path.push_str(&path) ; 
            File::create(full_path) } ).count();

    let templates: Vec<String> = parsed_template.templates.clone().expect("need some templates").into_iter()
        .map(|file| { let mut p = project.to_string() ;
            p.push('/') ;
            p.push_str(&file) ;
            p } ).collect();

    let templates_new: Vec<String> = parsed_template.templates.expect("need some templates").into_iter()
        .map(|file| { let mut p = name.to_string() ;
            p.push('/') ;
            p.push_str(&file) ;
            p } ).collect();

    let template_files: Vec<String> = templates.into_iter()
        .map(|p| { 
            let template_f = File::open(p) ;
            let mut t = String::new();
            template_f.expect("Failed to open file")
                .read_to_string(&mut t)
                .expect("File read failed.");
            t }).collect();

    let s: Vec<String> = template_files.clone().into_iter()
                               .map(|file| { let mut o = Cursor::new(Vec::new());
                                   project_hash.render(&file, &mut o).unwrap();
                                   String::from_utf8(o.into_inner()).unwrap()}).collect();

    let files_to_write = templates_new.iter().zip(s.iter());
    let _ = files_to_write.into_iter()
        .map(|(path, contents)| { 
            println!("{}",path) ;
            let mut c = File::create(path).expect("File create failed.") ;
            c.write(contents.as_bytes()) } ).count();

    //Command::new("git init ".push_str(project))
    //        .spawn()
    //        .expect("git failed to initialize.");

    // Renders the license string
    let data = HashBuilder::new().insert("name", decoded.author.expect("no author").name).insert("year", year + 1900);
    let mut out = Cursor::new(Vec::new());
    data.render("Copyright {{ name }} (c) {{ year }}", &mut out).unwrap();
    println!("{}", String::from_utf8(out.into_inner()).unwrap());
}
