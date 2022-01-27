use std::process::Command;

use colored::*;

pub fn git_init(name: &str) {
    let mut cmd = "cd ".to_string();
    cmd.push_str(name);
    cmd.push_str("&&");
    cmd.push_str("git init && git add *");
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn()
    {
        c.wait_with_output().expect("failed to wait on child");
    } else {
        eprintln!(
            "{}, git failed to initialize. Is git on your path?",
            "Error".red()
        );
        std::process::exit(0x0f01);
    }
}

pub fn pijul_init(name: &str) {
    let mut cmd = "cd ".to_string();
    cmd.push_str(name);
    cmd.push_str("&&");
    cmd.push_str("pijul init && pijul add **");
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn()
    {
        c.wait_with_output().expect("failed to wait on child");
    } else {
        eprintln!(
            "{}, Pijul failed to initialize. Is it on your path?",
            "Error".red()
        );
        std::process::exit(0x0f01);
    }
}

pub fn darcs_init(name: &str) {
    let mut cmd = "cd ".to_string();
    cmd.push_str(name);
    cmd.push_str("&&");
    cmd.push_str("darcs init && darcs add **");
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn()
    {
        c.wait_with_output().expect("failed to wait on child");
    } else {
        eprintln!(
            "{}, Darcs failed to initialize. Is hg on your path?",
            "Error".red()
        );
        std::process::exit(0x0f01);
    }
}

pub fn hg_init(name: &str) {
    let mut cmd = "cd ".to_string();
    cmd.push_str(name);
    cmd.push_str("&&");
    cmd.push_str("hg init && hg add *");
    if let Ok(c) = Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .stdout(std::process::Stdio::null())
        .spawn()
    {
        c.wait_with_output().expect("failed to wait on child");
    } else {
        eprintln!(
            "{}, Mercurial failed to initialize. Is it on your path?",
            "Error".red()
        );
        std::process::exit(0x0f01);
    }
}
