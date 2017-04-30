#[macro_use] extern crate clap;

extern crate time;
extern crate toml;
extern crate rustache;
extern crate project_init;

use rustache::{HashBuilder, Render};
use std::io::Cursor;
use std::fs::File;
use std::fs;
use clap::App;
use project_init::types::*;
use project_init::render::*;
use project_init::*;

fn main() {

    // command-line parser
    let yaml = load_yaml!("options-en.yml");
    let matches = App::from_yaml(yaml).version(crate_version!()).get_matches();

    // set path to .pi.toml
    let mut path = std::env::home_dir()
        .expect("Couldn't determine home directory.");
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
    let now = get_date();
    let year = now.date.tm_year;

    // Make a hash for inserting stuff into templates.
    let author = decoded.author.expect("no author information in ~/.pi.toml"); // TODO unwrap this nicely
    let hash = HashBuilder::new().insert("project",project)
        .insert("year", year)
        .insert("name", author.name)
        .insert("email", author.email)
        .insert("date", get_date().to_string());
  
    // read template.toml
    let mut template_path = project.to_string();
    template_path.push_str("/template.toml");
    let parsed_toml = read_toml_dir(&template_path);
    let parsed_dirs = parsed_toml.files;
    let parsed_config = parsed_toml.config;
    
    // create directories
    let _ = fs::create_dir(name);
    let _ = 
        if let Some(dirs) = parsed_dirs.directories {
            dirs.create_dirs(name);
        };

    // substitute stuff into file names
    let substitutions: Vec<String> = parsed_dirs.files.expect("need some files").into_iter()
                               .map(|file| { let mut o = Cursor::new(Vec::new());
                                   hash.render(&file, &mut o).unwrap();
                                   String::from_utf8(o.into_inner()).unwrap()}).collect();

    // create files
    let _ = substitutions.into_iter()
        .map(|path| { let mut full_path = name.to_string(); 
            full_path.push('/');
            full_path.push_str(&path); 
            File::create(full_path) } ).count();

    let templates_pre = parsed_dirs.templates;
    render_templates(project, name, hash, templates_pre);

    // initialize version control
    if let Some(config) = parsed_config {
        if let Some(vc) = config.version_control {
            if vc == "git" {
                repo::git_init(name);
            }
            else if vc == "hc" || vc == "mercurial" {
                repo::hg_init(name);
            }
        }
    }

    // Print that we're done
    println!("Finished initializing project in {}/",name);
}
