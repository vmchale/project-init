//! Read in files at compile time, making them available no matter what
pub const BSD3:&'static str = include_str!("includes/licenses/BSD3");
pub const BSD:&'static str = include_str!("includes/licenses/BSD");
pub const GPL3:&'static str = include_str!("includes/licenses/GPL3");
pub const MIT:&'static str = include_str!("includes/licenses/MIT");
pub const ALL_RIGHTS_RESERVED:&'static str = include_str!("includes/licenses/AllRightsReserved");
pub const README:&'static str = include_str!("includes/README.md");
pub const CARGO_TOML:&'static str = include_str!("includes/rust/Cargo.toml");
pub const RUST_LIB:&'static str = include_str!("includes/rust/lib.rs");
pub const RUST_TRAVIS_CI:&'static str = include_str!("includes/rust/.travis.yml");
