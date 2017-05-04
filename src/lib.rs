//! This library provides the functions/structs/methods used by the main binary. They are included
//! here in the hopes that they can be illuminating to users.

#[macro_use] extern crate serde_derive;
extern crate toml;
extern crate time;
extern crate core;

use std::fs::File;
use std::io::prelude::*;
use core::fmt;
use time::now;

pub mod types;
pub mod repo;
pub mod render;

// Formatter for our date struct.
impl fmt::Display for types::Date {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}-{}", self.date.tm_mon, self.date.tm_mday, self.date.tm_year)
    }
}

// Get current date.
pub fn get_date() -> types::Date {
    let mut t = now();
    t.tm_year = t.tm_year + 1900;
    types::Date { date: t } 
}

// Given a filepath, read the .toml file there as containing the directories/templates.
pub fn read_toml_dir(template_path: &str) -> types::Project {
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
