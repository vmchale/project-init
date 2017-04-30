//! This module contains the structs for the configuration files.

extern crate time;

use time::*;

pub struct Date {
    pub date: Tm,
}

#[derive(Debug, Deserialize)]
pub struct Author {
    pub name: String,
    pub email: String,
}

#[derive(Debug, Deserialize)]
pub struct Config {
    pub license: Option<String>,
    pub version_control: Option<String>,
    pub author: Option<Author>,
}

#[derive(Debug, Deserialize)]
pub struct Directory {
    pub files: Option<Vec<String>>,
    pub directories: Option<Vec<String>>,
    pub templates: Option<Vec<String>>,
}

#[derive(Debug, Deserialize)]
pub struct ProjectConfig {
    pub name_github_repo: bool,
    pub version_control: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct Project {
    pub files: Directory,
    pub config: Option<ProjectConfig>,
}
