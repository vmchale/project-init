#[macro_use] extern crate clap;
#[macro_use] extern crate text_io;

extern crate time;
extern crate toml;
extern crate rustache;
extern crate project_init;

//use rustache::{HashBuilder, Render, VecBuilder, Data};
use rustache::*;
use std::io::Cursor;
use std::fs::File;
use std::fs;
use clap::App;
use project_init::types::*;
use project_init::render::*;
use project_init::*;
use std::path::Path;

fn main() {

    // command-line parser
    let yaml = load_yaml!("options-en.yml");
    let matches = App::from_yaml(yaml).version(crate_version!()).get_matches();
    let force: bool = matches.occurrences_of("force") == 1 ;

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

    // read template.toml
    let mut template_path = project.to_string();
    template_path.push_str("/template.toml");
    let parsed_toml = read_toml_dir(&template_path);
    let parsed_dirs = parsed_toml.files;
    let parsed_config = parsed_toml.config;
    
    // create author struct
    let author = 
        if let Some(aut) = decoded.author {
            aut 
        }
        else {
            let nam: String = read!("Enter your name: {}!");
            let ema: String = read!("Enter your email: {}!");
            Author { name: nam, email: ema, github_username: None }
        };
        
    // set version
    let version = 
        if let Some(config) = parsed_config.clone() {
            if let Some(v) = config.version {
                v
            }
            else {
                "0.1.0".to_string()
            }
        }
        else {
            "0.1.0".to_string()
        };

    // set github username to null if it's not provided
    let github_username = 
        if let Some(uname) = author.github_username {
            uname
        }
        else {
            "".to_string()
        };

    // Make a hash for inserting stuff into templates.
    let hash = HashBuilder::new().insert("project",name)
        .insert("year", year)
        .insert("name", author.name.clone())
        .insert("version", version.clone())
        .insert("email", author.email.clone())
        .insert("github_username", github_username.clone())
        .insert("date", get_date().to_string());
 
    // check if the directory exists, and exit if we haven't forced an override.
    if Path::new(name).exists() && force == false {
<<<<<<< HEAD
        println!("Path '{}' already exists. Rerun with -f or --force to overwrite.", name);
=======
        println!("Path {} already exists. Rerun with -f or --force to overwrite.", name);
>>>>>>> 4bc8f7701348f29014d489f2fcd9c414b744b138
        std::process::exit(0x0f00);
    };

    // create directories
    let _ = fs::create_dir(name);
    if let Some(dirs_pre) = parsed_dirs.directories {
        // substitute into directory names using templates
        let dirs: Vec<String> = dirs_pre.into_iter()
                                   .map(|file| { let mut o = Cursor::new(Vec::new());
                                       hash.render(&file, &mut o).unwrap();
                                       String::from_utf8(o.into_inner()).unwrap() })
                                   .collect();

        // create directories
        dirs.create_dirs(name);
    };

    let files =
        if let Some(files_pre) = parsed_dirs.files {
            let substitutions: Vec<String> = files_pre.into_iter()
                                       .map(|file| { let mut o = Cursor::new(Vec::new());
                                           hash.render(&file, &mut o).unwrap();
                                           String::from_utf8(o.into_inner()).unwrap()}).collect();

            let _ = substitutions.clone().into_iter()
                .map(|path| { let mut full_path = name.to_string() ; 
                    full_path.push('/') ;
                    full_path.push_str(&path) ; 
                    File::create(full_path) } ).count();

            let s: Vec<Data> = substitutions.into_iter()
                .map(|string| Data::from(string))
                .collect();

            VecBuilder { data: s }
        }
        else {
            VecBuilder::new()
        };

    // Make a hash for inserting stuff into templates.
    let hash_with_files = HashBuilder::new().insert("project",name)
        .insert("year", year)
        .insert("name", author.name)
        .insert("version", version)
        .insert("email", author.email)
        .insert("files", files)
        .insert("github_username", github_username.clone())
        .insert("date", get_date().to_string());
    // TODO insert user keys as well!
 
    //fn convert(toml: Toml) -> Json {
    //match toml {
        //Toml::Table(table) => Json::Object(table.into_iter().map(|(k, v)| {
            //(k, convert(v))
        //}).collect()),
    //}

    // render templates
    render_templates(project, name, hash_with_files, parsed_dirs.templates, false);

    // render scripts, i.e. files that should be executable.
    render_templates(project, name, hash, parsed_dirs.scripts, true);

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
    else if let Some(vc) = decoded.version_control {
        if vc == "git" {
            repo::git_init(name);
        }
        else if vc == "hc" || vc == "mercurial" {
            repo::hg_init(name);
        }
    }

    // Print that we're done
    println!("Finished initializing project in {}/",name);
}
