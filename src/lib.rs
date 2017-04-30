//! This library provides the functions/structs/methods used by the main binary. They are included
//! here in the hopes that they can be illuminating to users.

#[macro_use] extern crate serde_derive;
extern crate toml;

use std::fs::File;
use std::io::prelude::*;
use std::fs;

pub mod types;

// Trait allowing us to create dirs/templates/files
pub trait Create {
    fn create_dirs(&self, name: &str) -> ();
}

// Create directories given a Vec<String> of directory names
impl <T:ToString>Create for Vec<T> {
    fn create_dirs(&self, name: &str) -> () {
    self.into_iter()
        .map(|dir| { let mut subdir = name.to_string() ;
            subdir.push('/') ;
            subdir.push_str(&dir.to_string()) ;
            fs::create_dir(subdir) } ).count();
    }
}

// Given a filepath, read the .toml file there as containing the directories/templates.
pub fn read_toml_dir(template_path: &str) -> types::Directory {
    let mut template_file = File::open(template_path)
        .expect("File could not be opened");
    let mut template = String::new();
    template_file.read_to_string(&mut template)
        .expect("File read failed");
    toml::from_str(&template).unwrap()
}
    
// Given a PathBuf, read the .toml file there as a configuration file.
pub fn read_toml_config(config_path: std::path::PathBuf) -> types::Config {
    let mut file = File::open(config_path)
        .expect("File could not be opened.");
    let mut toml_str = String::new();
    file.read_to_string(&mut toml_str)
        .expect("File read failed");
    toml::from_str(&toml_str).unwrap()
}
