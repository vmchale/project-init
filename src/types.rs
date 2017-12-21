//! This module contains the structs for the configuration files.

use toml::value::Value;

/// Struct for the author. This is read from the global
/// configuration that resides at $HOME/.pi.toml
#[derive(Debug, Deserialize)]
pub struct Author {
    pub name:            String,
    pub email:           String,
    pub github_username: Option<String>,
    pub reco_developer:  Option<bool>,
}

/// Struct for the global configuration at $HOME/.pi.toml
#[derive(Debug, Deserialize)]
pub struct Config {
    pub version_control: Option<String>,
    pub author:          Option<Author>,
    pub license:         Option<String>,
    pub user:            Option<UserConfig>,
}

/// Struct for directories, files, templates, and scripts to be created.
#[derive(Debug, Deserialize, Clone)]
pub struct Directory {
    pub files:       Option<Vec<String>>,
    pub directories: Option<Vec<String>>,
    pub templates:   Option<Vec<String>>,
    pub scripts:     Option<Vec<String>>,
}

/// Struct for project-specific configuration options
#[derive(Debug, Deserialize, Clone)]
pub struct ProjectConfig {
    pub version_control: Option<String>,
    pub version:         Option<String>,
}

/// Struct for a project
#[derive(Debug, Deserialize, Clone)]
pub struct Project {
    pub license:     Option<String>,
    pub with_readme: Option<bool>,
    pub files:       Directory,
    pub config:      Option<ProjectConfig>,
    pub user:        Option<UserConfig>,
}

/// Struct for custom user keys
#[derive(Debug, Deserialize, Clone)]
pub struct UserConfig {
    pub toml: Value,
}
