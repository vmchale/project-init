use std::process::Command;
use std;

pub fn git_init(name: &str) -> std::process::Child {
    let mut cmd = "git init".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    Command::new("sh")
            .arg("-c")
            .arg(cmd)
            .stdout(std::process::Stdio::null())
            .spawn()
            .expect("git failed to initialize.")
}

pub fn hg_init(name: &str) -> std::process::Child {
    let mut cmd = "hg init".to_string();
    cmd.push(' ');
    cmd.push_str(name);
    Command::new("sh")
            .arg("-c")
            .arg(cmd)
            .stdout(std::process::Stdio::null())
            .spawn()
            .expect("mercurial failed to initialize.")
}
