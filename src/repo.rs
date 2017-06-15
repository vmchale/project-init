use std::process::Command;
use std;
use colored::*;

pub fn git_init(name: &str) -> () {
    let mut cmd = "git init && git add *".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn() {
            c.wait_with_output().expect("failed to wait on child");
        }
    else {
        eprintln!("{}, git failed to initialize. Is git on your path?","Error".red());
        std::process::exit(0x0f01);
    }
}

pub fn pijul_init(name: &str) -> () {
    let mut cmd = "pijul init && pijul add **".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn() {
            c.wait_with_output().expect("failed to wait on child");
        }
    else {
        eprintln!("{}, Pijul failed to initialize. Is hg on your path?","Error".red());
        std::process::exit(0x0f01);
    }
}

pub fn darcs_init(name: &str) -> () {
    let mut cmd = "darcs init && darcs add **".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn() {
            c.wait_with_output().expect("failed to wait on child");
        }
    else {
        eprintln!("{}, Darcs failed to initialize. Is hg on your path?","Error".red());
        std::process::exit(0x0f01);
    }
}

pub fn hg_init(name: &str) -> () {
    let mut cmd = "hg init".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn() {
            c.wait_with_output().expect("failed to wait on child");
        }
    else {
        eprintln!("{}, Mercurial failed to initialize. Is hg on your path?","Error".red());
        std::process::exit(0x0f01);
    }
}
