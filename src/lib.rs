//! This library provides the functions/structs/methods used by the main
//! binary. They are included
//! here in the hopes that they can be illuminating to users.
// #![allow(clippy::cognitive_complexity)]

use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::path::PathBuf;

use case::*;
use colored::*;
use heck::ToUpperCamelCase;
use rustache::{HashBuilder, VecBuilder};
use toml::Value::Table;

pub mod includes;
pub mod render;
pub mod repo;
pub mod types;

/// Given a filepath, read the .toml file there as containing the
/// directories/templates.
/// If no such file is found, read from global template directory in
/// `$HOME/.pi_templates/`.
pub fn read_toml_dir(template_path: &str, home: PathBuf) -> (types::Project, bool) {
    let (mut template_file, is_global_template) = if let Ok(f) = File::open(&template_path) {
        (f, false)
    } else if let Ok(f) = {
        let mut p = home;
        p.push(".pi_templates/");
        p.push(template_path);
        File::open(p)
    } {
        (f, true)
    } else {
        println!(
            "{}: File {:?} could not be opened. Check that it exists.",
            "Error".red(),
            template_path
        );
        std::process::exit(0x0f00);
    };
    let mut template = String::new();
    template_file
        .read_to_string(&mut template)
        .expect("Failed to read file"); // we can panic because we already errored if the file didn't exist.
    (read_toml_str(&template, template_path), is_global_template)
}

/// Read a string containing a toml file
pub fn read_toml_str(template: &str, template_path: &str) -> types::Project {
    let extract = toml::from_str(template);
    if let Ok(t) = extract {
        t
    } else if let Err(e) = extract {
        println!("Error parsing {:?}: {}", template_path, e);
        std::process::exit(0x0f00);
    } else {
        std::process::exit(0x0f00);
    }
}

/// Given a `PathBuf`, read the .toml file there as a configuration file.
pub fn read_toml_config(config_path: &Path) -> types::Config {
    let file = if let Ok(f) = File::open(&config_path) {
        Some(f)
    } else {
        println!(
            "{}: File {:?} could not be opened. Check that it exists.",
            "Warning".yellow(),
            config_path
        );
        None
    };
    let mut toml_str = String::new();
    let maybe_file = file.map(|mut x| x.read_to_string(&mut toml_str));
    let extract = toml::from_str(&toml_str);
    if maybe_file.is_some() && maybe_file.unwrap().is_ok() {
        if let Ok(t) = extract {
            t
        } else if let Err(e) = extract {
            println!("Error parsing {:?}: {}", config_path, e);
            std::process::exit(0x0f00);
        } else {
            std::process::exit(0x0f00);
        }
    } else {
        eprintln!(
            "{}: No ~/.pi.toml found. Using defaults.",
            "Warning".yellow()
        );
        types::Config {
            version_control: None,
            author: None,
            license: None,
            user: None,
        }
    }
}

#[allow(clippy::too_many_arguments)]
pub fn init_helper(
    home: PathBuf,
    project_dir: &str,
    decoded: types::Config,
    author: types::Author,
    name: &str,
    year: i32,
    current_date: &str,
    force: bool,
    parsed_toml: types::Project,
    is_global_project: bool,
) {
    let project = if is_global_project {
        let mut p = home;
        p.push(".pi_templates/");
        p.push(project_dir);
        p.to_str().unwrap().to_string()
    } else {
        project_dir.to_string()
    };
    let parsed_dirs = parsed_toml.files;
    let parsed_config = parsed_toml.config;

    // set license if it's set
    let (license_contents, license_name) =
        // prefer project-specific license over global
        if let Some(l) = parsed_toml.license {
            match l.as_str() {
                "BSD3" => (Some(includes::BSD3), "BSD3"),
                "BSD" => (Some(includes::BSD), "BSD"),
                "MIT" => (Some(includes::MIT), "MIT"),
                "GPL3" => (Some(includes::GPL3), "GLP3"),
                "AllRightsReserved" => (Some(includes::BSD3), "AllRightsReserved"),
                _ => { println!("{}: requested license not found. Defaulting to AllRightsReserved","Warning".yellow()) 
                        ; (Some(includes::ALL_RIGHTS_RESERVED), "AllRightsReserved") }
            }
        }
        else if let Some(l) = decoded.license {
            match l.as_str() {
                "BSD3" => (Some(includes::BSD3), "BSD3"),
                "BSD" => (Some(includes::BSD), "BSD"),
                "MIT" => (Some(includes::MIT), "MIT"),
                "GPL3" => (Some(includes::GPL3), "GLP3"),
                "AllRightsReserved" => (Some(includes::BSD3), "AllRightsReserved"),
                _ => { println!("{}: requested license not found. Defaulting to AllRightsReserved","Warning".yellow()) 
                        ; (Some(includes::ALL_RIGHTS_RESERVED), "AllRightsReserved") }
            }
        }
        else {
            (None,"")
        };

    // set version
    let version = if let Some(config) = parsed_config.clone() {
        if let Some(v) = config.version {
            v
        } else {
            "0.1.0".to_string()
        }
    } else {
        eprintln!(
            "{}: no version info found, defaulting to '0.1.0'",
            "Warning".yellow()
        );
        "0.1.0".to_string()
    };

    // set github username to null if it's not provided
    let github_username = if let Some(uname) = author.github_username {
        uname
    } else {
        eprintln!(
            "{}: no github username found, defaulting to null",
            "Warning".yellow()
        );
        "".to_string()
    };

    // make user_keys into a vector; prepare to insert them into the `HashBuilder`
    let user_keys = if let Some(u) = parsed_toml.user {
        match u.toml {
            Table(t) => Some(t),
            _ => None,
        }
    } else {
        None
    };

    // make user_keys into a vector; prepare to insert them into the `HashBuilder`
    let user_keys_global = if let Some(u) = decoded.user {
        match u.toml {
            Table(t) => Some(t),
            _ => None,
        }
    } else {
        None
    };

    // Make a hash for inserting stuff into templates.
    let mut hash = HashBuilder::new();
    // project-specific
    if let Some(x) = user_keys {
        for (key, value) in &x {
            if let Some(a) = value.as_str() {
                hash = hash.insert(key, a);
            }
        }
    }
    // global
    if let Some(x) = user_keys_global {
        for (key, value) in &x {
            if let Some(a) = value.as_str() {
                hash = hash.insert(key, a);
            }
        }
    }
    // add the normal stuff
    hash = hash
        .insert("project", name)
        .insert("Project", name.to_capitalized())
        .insert("ProjectCamelCase", name.to_upper_camel_case())
        .insert("year", year)
        .insert("name", author.name)
        .insert("version", version)
        .insert("email", author.email)
        .insert("github_username", github_username)
        .insert("license", license_name)
        .insert("date", current_date);

    // check if the directory exists and exit, if we haven't forced an overwrite.
    if Path::new(name).exists() && !force {
        println!(
            "Path '{}' already exists. Rerun with -f or --force to overwrite.",
            name
        );
        std::process::exit(0x0f00);
    };

    // create directories
    let _ = fs::create_dir(name);
    if let Some(dirs_pre) = parsed_dirs.directories {
        render::render_dirs(dirs_pre, &hash, name);
    }

    // create a list of files contained in the project, and create those files.
    // TODO should include templates/scripts/etc.
    let files = if let Some(files_pre) = parsed_dirs.files {
        render::render_files(files_pre, &hash, name) // FIXME files need to have a newline insert in between them?
    } else {
        VecBuilder::new()
    };

    // create license if it was asked for
    if let Some(lic) = license_contents {
        render::render_file(lic, name, "LICENSE", &hash);
    }

    // render readme if requested
    if let Some(readme) = parsed_toml.with_readme {
        if readme {
            render::render_file(includes::README, name, "README.md", &hash);
        }
    }

    // Make a hash for inserting stuff into templates.
    hash = hash.insert("files", files);

    // render templates
    render::render_templates(&project, name, &hash, parsed_dirs.templates, false);

    // render scripts, i.e. files that should be executable.
    render::render_templates(&project, name, &hash, parsed_dirs.scripts, true);

    // initialize version control
    if let Some(config) = parsed_config {
        if let Some(vc) = config.version_control {
            match vc.as_str() {
                "git" => repo::git_init(name),
                "hg" | "mercurial" => repo::hg_init(name),
                "pijul" => repo::pijul_init(name),
                "darcs" => repo::darcs_init(name),
                _ => {
                    eprintln!(
                        "{}: version control {} is not yet supported. Supported version control tools are darcs, pijul, mercurial, and git.",
                        "Error".red(),
                        vc
                    );
                }
            }
        }
    } else if let Some(vc) = decoded.version_control {
        match vc.as_str() {
            "git" => repo::git_init(name),
            "hg" | "mercurial" => repo::hg_init(name),
            "pijul" => repo::pijul_init(name),
            "darcs" => repo::darcs_init(name),
            _ => {
                eprintln!(
                    "{}: version control {} is not yet supported. Supported version control tools are darcs, pijul, mercurial, and git.",
                    "Error".red(),
                    vc
                );
            }
        }
    }

    // Print that we're done
    println!("Finished initializing project in {}/", name);
}
