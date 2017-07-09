//! Read in files at compile time, making them available no matter what

// Licenses
pub const BSD3:&'static str = include_str!("includes/licenses/BSD3");
pub const BSD:&'static str = include_str!("includes/licenses/BSD");
pub const GPL3:&'static str = include_str!("includes/licenses/GPL3");
pub const MIT:&'static str = include_str!("includes/licenses/MIT");
pub const ALL_RIGHTS_RESERVED:&'static str = include_str!("includes/licenses/AllRightsReserved");

// README.md
pub const README:&'static str = include_str!("includes/README.md");

// Plain template
pub const PLAIN_TEMPLATE:&'static str = include_str!("includes/plain.toml");

// Rust includes 
pub const RUST_GITIGNORE:&'static str = include_str!("includes/rust/.gitignore");
pub const CARGO_TOML:&'static str = include_str!("includes/rust/Cargo.toml.init");
pub const RUST_TEMPLATE:&'static str = include_str!("includes/rust/template.toml");
pub const RUST_LIB:&'static str = include_str!("includes/rust/lib.rs.init");
pub const RUST_MAIN:&'static str = include_str!("includes/rust/main.rs.init");
pub const RUST_TRAVIS_CI:&'static str = include_str!("includes/rust/.travis.yml");

// Vim includes
pub const VIM_GITIGNORE:&'static str = include_str!("includes/vim/.gitignore");
pub const VIMBALL:&'static str = include_str!("includes/vim/vimball.txt");
pub const VIM_TEMPLATE:&'static str = include_str!("includes/vim/template.toml");

// Python includes
pub const PY_GITIGNORE:&'static str = include_str!("includes/python/.gitignore");
pub const PY_BIN:&'static str = include_str!("includes/python/bin.py");
pub const PY_SETUP:&'static str = include_str!("includes/python/setup.py");
pub const PY_CFG:&'static str = include_str!("includes/python/setup.cfg");
pub const PY_TEMPLATE:&'static str = include_str!("includes/python/template.toml");

// Julia includes

pub const JULIA_TEMPLATE:&'static str = include_str!("includes/julia-lib/template.toml");
pub const JULIA_GITIGNORE:&'static str = include_str!("includes/julia-lib/.gitignore");
pub const JULIA_REQUIRE:&'static str = include_str!("includes/julia-lib/REQUIRE");
pub const JULIA_TEST:&'static str = include_str!("includes/julia-lib/test/test.jl");
pub const JULIA_SRC:&'static str = include_str!("includes/julia-lib/src/{{ Project }}.jl");

// Haskell includes
pub const SETUP_HS:&'static str = include_str!("includes/haskell/Setup.hs");
pub const MAIN:&'static str = include_str!("includes/haskell/app/Main.hs");
pub const LIB:&'static str = include_str!("includes/haskell/src/Lib.hs");
pub const BENCH:&'static str = include_str!("includes/haskell/bench/Bench.hs");
pub const TEST:&'static str = include_str!("includes/haskell/test/Spec.hs");
pub const DEFAULT_NIX:&'static str = include_str!("includes/haskell/default.nix");
pub const RELEASE_NIX:&'static str = include_str!("includes/haskell/release.nix");
pub const CABAL:&'static str = include_str!("includes/haskell/project.cabal");
pub const STACK_YAML:&'static str = include_str!("includes/haskell/stack.yaml");
pub const HASK_TEMPLATE:&'static str = include_str!("includes/haskell/template.toml");
pub const HASKELL_TRAVIS_CI:&'static str = include_str!("includes/haskell/.travis.yml");
pub const HASKELL_GITIGNORE:&'static str = include_str!("includes/haskell/.gitignore");

// Idiris includes
pub const IDRIS_TEMPLATE:&'static str = include_str!("includes/idris/template.toml");
pub const IDRIS_LIB:&'static str = include_str!("includes/idris/{{ Project }}/Lib.idr");
pub const IDRIS_EXE:&'static str = include_str!("includes/idris/{{ Project }}.idr");
pub const IPKG:&'static str = include_str!("includes/idris/{{ project }}.ipkg");
pub const IDRIS_GITIGNORE:&'static str = include_str!("includes/idris/.gitignore");
