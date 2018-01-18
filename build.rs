extern crate cli_setup;

use cli_setup::*;

pub const MAN_PI: &'static str = include_str!("man/pi.1");

fn main() {
    setup_manpages(MAN_PI, "pi");
}
