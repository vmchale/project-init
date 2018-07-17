//! Read in files at compile time, making them available no matter what

// Licenses
pub const BSD3: &str = include_str!("includes/licenses/BSD3");
pub const BSD: &str = include_str!("includes/licenses/BSD");
pub const GPL3: &str = include_str!("includes/licenses/GPL3");
pub const MIT: &str = include_str!("includes/licenses/MIT");
pub const ALL_RIGHTS_RESERVED: &str = include_str!("includes/licenses/AllRightsReserved");

// README.md
pub const README: &str = include_str!("includes/README.md");

// Plain template
pub const PLAIN_TEMPLATE: &str = include_str!("includes/plain.toml");

// ATS
pub const ATS_SRC: &str = include_str!("includes/ats/project.dats");
pub const ATS_PKG: &str = include_str!("includes/ats/atspkg.dhall");
pub const ATS_LIB: &str = include_str!("includes/ats/pkg.dhall");
pub const ATS_CTAGS: &str = include_str!("includes/ats/.ctags");
pub const ATS_TEMPLATE: &str = include_str!("includes/ats/template.toml");
pub const ATS_FORMAT: &str = include_str!("includes/ats/.atsfmt.toml");
pub const ATS_TRAVIS: &str = include_str!("includes/ats/.travis.yml");
pub const ATS_GITIGNORE: &str = include_str!("includes/ats/.gitignore");

// Madlang
pub const MADLANG_TEMPLATE: &str = include_str!("includes/madlang/template.toml");
pub const MADLANG_SRC: &str = include_str!("includes/madlang/gambling.mad");

// Rust includes
pub const RUST_GITIGNORE: &str = include_str!("includes/rust/.gitignore");
pub const CARGO_TOML: &str = include_str!("includes/rust/Cargo.toml.init");
pub const RUST_TEMPLATE: &str = include_str!("includes/rust/template.toml");
pub const RUST_LIB: &str = include_str!("includes/rust/lib.rs.init");
pub const RUST_MAIN: &str = include_str!("includes/rust/main.rs.init");
pub const RUST_TRAVIS_CI: &str = include_str!("includes/rust/.travis.yml");
pub const RUST_BENCHMARKS: &str = include_str!("includes/rust/benches.rs.init");

// Vim includes
pub const VIM_GITIGNORE: &str = include_str!("includes/vim/.gitignore");
pub const VIMBALL: &str = include_str!("includes/vim/vimball.txt");
pub const VIM_TEMPLATE: &str = include_str!("includes/vim/template.toml");
pub const VIM_TRAVIS: &str = include_str!("includes/vim/.travis.yml");

// Python includes
pub const PY_GITIGNORE: &str = include_str!("includes/python/.gitignore");
pub const PY_BIN: &str = include_str!("includes/python/bin.py");
pub const PY_SETUP: &str = include_str!("includes/python/setup.py");
pub const PY_CFG: &str = include_str!("includes/python/setup.cfg");
pub const PY_TEMPLATE: &str = include_str!("includes/python/template.toml");

// Julia includes
pub const JULIA_TEMPLATE: &str = include_str!("includes/julia-lib/template.toml");
pub const JULIA_GITIGNORE: &str = include_str!("includes/julia-lib/.gitignore");
pub const JULIA_REQUIRE: &str = include_str!("includes/julia-lib/REQUIRE");
pub const JULIA_TEST: &str = include_str!("includes/julia-lib/test/test.jl");
pub const JULIA_SRC: &str = include_str!("includes/julia-lib/src/{{ Project }}.jl");

// Elm includes
pub const ELM_PACKAGE: &str = include_str!("includes/elm/elm-package.json");
pub const ELM_MAIN: &str = include_str!("includes/elm/src/main.elm");
pub const ELM_STATE: &str = include_str!("includes/elm/src/State.elm");
pub const ELM_UPDATE: &str = include_str!("includes/elm/src/Update.elm");
pub const ELM_VIEW: &str = include_str!("includes/elm/src/View.elm");
pub const ELM_GITIGNORE: &str = include_str!("includes/elm/.gitignore");
pub const ELM_TEMPLATE: &str = include_str!("includes/elm/template.toml");
pub const ELM_CTAGS: &str = include_str!("includes/elm/.ctags");

// Miso includes
pub const MISO_SETUP_HS: &str = include_str!("includes/miso/Setup.hs");
pub const MISO_MAIN: &str = include_str!("includes/miso/app/Main.hs");
pub const MISO_STACK: &str = include_str!("includes/miso/stack.yaml");
pub const SHAKE_STACK: &str = include_str!("includes/miso/stack-shake.yaml");
pub const MISO_CABAL: &str = include_str!("includes/miso/{{ project }}.cabal");
pub const MISO_LIB: &str = include_str!("includes/miso/src/Lib.hs");
pub const MISO_GITIGNORE: &str = include_str!("includes/miso/.gitignore");
pub const MISO_TRAVIS: &str = include_str!("includes/miso/.travis.yml");
pub const MISO_TEMPLATE: &str = include_str!("includes/miso/template.toml");
pub const MISO_SHAKE: &str = include_str!("includes/miso/shake.hs");
pub const MISO_HTML: &str = include_str!("includes/miso/web-src/index.html");

// Haskell includes
pub const SETUP_HS: &str = include_str!("includes/haskell/Setup.hs");
pub const MAIN: &str = include_str!("includes/haskell/app/Main.hs");
pub const LIB: &str = include_str!("includes/haskell/src/Lib.hs");
pub const BENCH: &str = include_str!("includes/haskell/bench/Bench.hs");
pub const TEST: &str = include_str!("includes/haskell/test/Spec.hs");
pub const DEFAULT_NIX: &str = include_str!("includes/haskell/default.nix");
pub const RELEASE_NIX: &str = include_str!("includes/haskell/release.nix");
pub const CABAL: &str = include_str!("includes/haskell/project.cabal");
pub const KMETT: &str = include_str!("includes/kmett/project.cabal");
pub const STACK_YAML: &str = include_str!("includes/haskell/stack.yaml");
pub const HASK_TEMPLATE: &str = include_str!("includes/haskell/template.toml");
pub const HASK_YAMLLINT: &str = include_str!("includes/haskell/.yamllint");
pub const HASKELL_TRAVIS_CI: &str = include_str!("includes/haskell/.travis.yml");
pub const HASKELL_GITIGNORE: &str = include_str!("includes/haskell/.gitignore");
pub const CABAL_PROJECT: &str = include_str!("includes/haskell/cabal.project.local");
pub const HLINT_TEMPLATE: &str = include_str!("includes/haskell/.hlint.yaml");
pub const HASKELL_APPVEYOR: &str = include_str!("includes/haskell/appveyor.yml");
pub const STYLISH_HASKELL: &str = include_str!("includes/haskell/.stylish-haskell.yaml");
pub const HSPEC: &str = include_str!("includes/haskell/.hspec");
pub const HS_GITATTRIBUTES: &str = include_str!("includes/haskell/.gitattributes");

// Idiris includes
pub const IDRIS_TEMPLATE: &str = include_str!("includes/idris/template.toml");
pub const IDRIS_LIB: &str = include_str!("includes/idris/{{ Project }}/Lib.idr");
pub const IDRIS_EXE: &str = include_str!("includes/idris/{{ Project }}.idr");
pub const IPKG: &str = include_str!("includes/idris/{{ project }}.ipkg");
pub const IPKG_TEST: &str = include_str!("includes/idris/{{ project }}_test.ipkg");
pub const IDRIS_TEST: &str = include_str!("includes/idris/Spec.idr");
pub const IDRIS_GITIGNORE: &str = include_str!("includes/idris/.gitignore");
pub const IDRIS_CTAGS: &str = include_str!("includes/idris/.ctags");
