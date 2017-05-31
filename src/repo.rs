use std::process::Command;
use std;
use colored::*;

pub fn git_init(name: &str) -> std::process::Child {
    let mut cmd = "git init".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn() {
            c
        }
    else {
        eprintln!("{}, git failed to initialize. Is git on your path?","Error".red());
        std::process::exit(0x0f01);
    }
}

pub fn hg_init(name: &str) -> std::process::Child {
    let mut cmd = "hg init".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn() {
            c
        }
    else {
        eprintln!("{}, Mercurial failed to initialize. Is hg on your path?","Error".red());
        std::process::exit(0x0f01);
    }
}
