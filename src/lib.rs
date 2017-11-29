//! This library provides the functions/structs/methods used by the main
//! binary. They are included
//! here in the hopes that they can be illuminating to users.

#![feature(type_ascription)]

#[macro_use]
extern crate serde_derive;
extern crate toml;
extern crate colored;

use colored::*;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;
use toml::de;

pub mod types;
pub mod repo;
pub mod includes;
pub mod render;

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
           }
    {
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
    template_file.read_to_string(&mut template).expect(
        "Failed to read file",
    ); // we can panic because we already errored if the file didn't exist.
    (read_toml_str(&template, template_path), is_global_template)
}

/// Read a string containing a toml file
pub fn read_toml_str(template: &str, template_path: &str) -> types::Project {
    if let Ok(t) = toml::from_str(template) {
        t
    } else if let Err(e) = toml::from_str(template): Result<String, de::Error> {
        println!("Error parsing {:?}: {}", template_path, e);
        std::process::exit(0x0f00);
    } else {
        std::process::exit(0x0f00);
    }
}

/// Given a `PathBuf`, read the .toml file there as a configuration file.
pub fn read_toml_config(config_path: &std::path::PathBuf) -> types::Config {
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
    if maybe_file.is_some() && maybe_file.unwrap().is_ok() {
        if let Ok(t) = toml::from_str(&toml_str) {
            t
        } else if let Err(e) = toml::from_str(&toml_str): Result<String, de::Error> {
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
