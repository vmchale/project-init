use std::env::home_dir;
use std::path::PathBuf;
use std::fs::File;
use std::io::prelude::*;
use std::fs::create_dir_all;

pub const MAN_PI: &'static str = include_str!("man/pi.1");

// #manpath updated by cli-setup
// export MANPATH=~/.local/share:$MANPATH

fn main() {

    let mut man_dir = match home_dir() { Some(p) => p, None => PathBuf::from("."), };
    man_dir.push(".local");
    man_dir.push("share");
    man_dir.push("man");
    man_dir.push("man1");
    let mut man_path = man_dir.clone();
    
    let _ = create_dir_all(man_dir);

    man_path.push("pi");
    man_path.set_extension("1");

    let pre_f = File::create(man_path);
    match pre_f {
        Ok(mut f) => {
    let res = f.write(MAN_PI.as_bytes());
    match res {
        Ok(_) => (),
        Err(_) => (),
    }
        },
        Err(_) => (),
    }

}
