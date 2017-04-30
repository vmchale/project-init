#[macro_use] extern crate clap;

extern crate time;
extern crate toml;
extern crate rustache;
extern crate project_init;

use std::process::Command;
use rustache::{HashBuilder, Render};
use time::now;
use std::io::Cursor;
use std::io::prelude::*;
use std::fs::File;
use std::fs;
use clap::App;
use project_init::types::*;
use project_init::*;

fn main() {
    // command-line parser
    let yaml = load_yaml!("options-en.yml");
    let matches = App::from_yaml(yaml).version(crate_version!()).get_matches();

    // set path to .pi.toml
    let mut path = std::env::home_dir().expect("Couldn't determine home directory.");
    path.push(".pi.toml");

    // read config file
    let decoded: Config = read_toml_config(path);

    // get project directory
    let project = matches
        .value_of("directory")
        .expect("Failed to supply a required argument");

    // get project name
    let name = matches
        .value_of("name")
        .expect("Failed to supply a required argument");

    //get year
    let year = now().tm_year;

    // Make a hash for inserting stuff into templates.
    let hash = HashBuilder::new().insert("project",project)
        .insert("year", year + 1900)
        .insert("name", decoded.author.expect("no author").name)
        .insert("version", decoded.version.expect("no author").version)
        .insert("email", decoded.author.expect("no author").email);
   
    // read template.toml
    let mut template_path = project.to_string();
    template_path.push_str("/template.toml");
    let parsed_template = read_toml_dir(&template_path); //: Directory = toml::from_str(&template).unwrap();
    
    // create directories
    let _ = fs::create_dir(name);
    let _ = 
        if let Some(dirs) = parsed_template.directories {
            dirs.create_dirs(name);
        };

    let substitutions: Vec<String> = parsed_template.files.expect("need some files").into_iter()
                               .map(|file| { let mut o = Cursor::new(Vec::new());
                                   hash.render(&file, &mut o).unwrap();
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
            p }).collect();

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
                                   hash.render(&file, &mut o).unwrap();
                                   String::from_utf8(o.into_inner()).unwrap()}).collect();

    let files_to_write = templates_new.iter().zip(s.iter());
    let _ = files_to_write.into_iter()
        .map(|(path, contents)| { 
            let mut c = File::create(path).expect("File create failed.") ;
            c.write(contents.as_bytes()) } ).count();

    Command::new("sh")
            .args(&["git init", name])
            .stderr(std::process::Stdio::null())
            .spawn()
            .expect("git failed to initialize.");

    // Renders the license string
    let mut out = Cursor::new(Vec::new());
    hash.render("Copyright {{ name }} (c) {{ year }}", &mut out).unwrap();
    println!("Project initialized successfully in {}/",name);
}
