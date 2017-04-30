//! This library provides the functions/structs/methods used by the main binary. They are included
//! here in the hopes that they can be illuminating to users.

#[macro_use] extern crate serde_derive;
extern crate toml;

use std::fs::File;
use std::io::prelude::*;

pub mod types;

// Given a filepath, read the .toml file there as containing the directories/templates.
pub fn read_toml_dir(template_path: &str) -> types::Directory {
    let mut template_file = File::open(template_path)
        .expect("File could not be opened");
    let mut template = String::new();
    template_file.read_to_string(&mut template)
        .expect("File read failed");
    let parsed_template: types::Directory = toml::from_str(&template).unwrap();
    parsed_template
}
    
// Given a PathBuf, read the .toml file there as a configuration file.
pub fn read_toml_config(config_path: std::path::PathBuf) -> types::Config {
    let mut file = File::open(config_path)
        .expect("File could not be opened.");
    let mut toml_str = String::new();
    file.read_to_string(&mut toml_str)
        .expect("File read failed");
    let decoded: types::Config = toml::from_str(&toml_str).unwrap();
    decoded
}
