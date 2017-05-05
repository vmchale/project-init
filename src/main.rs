//! Source file for the binary.
#[macro_use] extern crate clap;
#[macro_use] extern crate text_io;

extern crate time;
extern crate toml;
extern crate rustache;
extern crate project_init;

use rustache::*;
use std::fs;
use clap::App;
use project_init::types::*;
use project_init::render::*;
use project_init::*;
use std::path::Path;
use time::strftime;

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
        
    //get year
    let now = time::now();
    let year = now.tm_year + 1900;
    let current_date = strftime("%m-%d-%Y", &now).unwrap();

    if let Some(matches_init) = matches.subcommand_matches("new") {

        // get project name
        let name = matches_init
            .value_of("name")
            .expect("Failed to supply project name");

        // get project template type
        let template_str = matches_init
            .value_of("template")
            .expect("Failed to supply project directory");
    }
    else if let Some(matches_init) = matches.subcommand_matches("init") {

        // get project name
        let name = matches_init
            .value_of("name")
            .expect("Failed to supply project name");

        // get project directory
        let project = matches_init
            .value_of("directory")
            .expect("Failed to supply project directory");

        // read template.toml
        let mut template_path = project.to_string();
        template_path.push_str("/template.toml");
        let parsed_toml = read_toml_dir(&template_path);
        let parsed_dirs = parsed_toml.files;
        let parsed_config = parsed_toml.config;
        
        // set license if it's set
        let license_contents =
            if let Some(l) = decoded.license {
                match l.as_str() {
                    "BSD3" => Some(includes::BSD3),
                    "BSD" => Some(includes::BSD),
                    "MIT" => Some(includes::MIT),
                    "GPL3" => Some(includes::GPL3),
                    "AllRightsReserved" => Some(includes::BSD3),
                    _ => { println!("Warning: requested license not found. Defaulting to AllRightsReserved") 
                           ; Some(includes::ALL_RIGHTS_RESERVED) }
                }
            }
            else if let Some(l) = parsed_toml.license {
                match l.as_str() {
                    "BSD3" => Some(includes::BSD3),
                    "BSD" => Some(includes::BSD),
                    "MIT" => Some(includes::MIT),
                    "GPL3" => Some(includes::GPL3),
                    "AllRightsReserved" => Some(includes::BSD3),
                    _ => { println!("Warning: requested license not found. Defaulting to AllRightsReserved") 
                           ; Some(includes::ALL_RIGHTS_RESERVED) }
                }
            }
            else {
                None
            };

        // set version
        // TODO only insert into the hash if necessary
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
                println!("Warning: no version info found, defaulting to '0.1.0'");
                "0.1.0".to_string()
            };

        // set github username to null if it's not provided
        let github_username = 
            if let Some(uname) = author.github_username {
                uname
            }
            else {
                println!("Warning: no version info found, defaulting to '0.1.0'");
                "".to_string()
            };

        // Make a hash for inserting stuff into templates.
        let hash = HashBuilder::new().insert("project",name)
            .insert("year", year)
            .insert("name", author.name)
            .insert("version", version)
            .insert("email", author.email)
            .insert("github_username", github_username)
            .insert("date", current_date);
     
        // check if the directory exists and exit, if we haven't forced an overwrite.
        if Path::new(name).exists() && force == false {
            println!("Path '{}' already exists. Rerun with -f or --force to overwrite.", name);
            std::process::exit(0x0f00);
        };

        // create directories
        let _ = fs::create_dir(name);
        if let Some(dirs_pre) = parsed_dirs.directories {
            render_dirs(dirs_pre, &hash, name);
        }

        // create a list of files contained in the project, and create those files.
        // TODO should include templates/scripts/etc.
        let files =
            if let Some(files_pre) = parsed_dirs.files {
                render_files(files_pre, &hash, name)
            }
            else {
                VecBuilder::new()
            };

        // create license if it was asked for
        if let Some(lic) = license_contents {
            render_file(lic, name, "LICENSE", &hash);
        }

        // render readme if requested
        if let Some(readme) = parsed_toml.with_readme {
            if readme == true {
                render_file(includes::README, name, "README.md", &hash);
            }
        }

        // Make a hash for inserting stuff into templates.
        let hash_with_files = &hash
            .insert("files", files);

        // render templates
        render_templates(project, name, &hash_with_files, parsed_dirs.templates, false);

        // render scripts, i.e. files that should be executable.
        render_templates(project, name, &hash_with_files, parsed_dirs.scripts, true);

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
}
