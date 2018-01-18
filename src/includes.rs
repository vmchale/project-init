//! Read in files at compile time, making them available no matter what

// Licenses
pub const BSD3: &'static str = include_str!("includes/licenses/BSD3");
pub const BSD: &'static str = include_str!("includes/licenses/BSD");
pub const GPL3: &'static str = include_str!("includes/licenses/GPL3");
pub const MIT: &'static str = include_str!("includes/licenses/MIT");
pub const ALL_RIGHTS_RESERVED: &'static str = include_str!("includes/licenses/AllRightsReserved");

// README.md
pub const README: &'static str = include_str!("includes/README.md");

// Plain template
pub const PLAIN_TEMPLATE: &'static str = include_str!("includes/plain.toml");

// ATS
pub const ATS_SRC: &'static str = include_str!("includes/ats/project.dats");
pub const ATS_SHAKE: &'static str = include_str!("includes/ats/shake.hs");
pub const ATS_CTAGS: &'static str = include_str!("includes/ats/.ctags");
pub const ATS_TEMPLATE: &'static str = include_str!("includes/ats/template.toml");
pub const ATS_FORMAT: &'static str = include_str!("includes/ats/.atsfmt.toml");

// Madlang
pub const MADLANG_TEMPLATE: &'static str = include_str!("includes/madlang/template.toml");
pub const MADLANG_SRC: &'static str = include_str!("includes/madlang/gambling.mad");

// Reco template
pub const RECO_TEMPLATE: &'static str = include_str!("includes/reco/template.toml");
pub const RECO_MAIN: &'static str = include_str!("includes/reco/main.go");
pub const RECO_README: &'static str = include_str!("includes/reco/README.md");
pub const RECO_TEST_COMMAND: &'static str = include_str!("includes/reco/cmd/test-reco/main.go");

// Rust includes
pub const RUST_GITIGNORE: &'static str = include_str!("includes/rust/.gitignore");
pub const CARGO_TOML: &'static str = include_str!("includes/rust/Cargo.toml.init");
pub const RUST_TEMPLATE: &'static str = include_str!("includes/rust/template.toml");
pub const RUST_LIB: &'static str = include_str!("includes/rust/lib.rs.init");
pub const RUST_MAIN: &'static str = include_str!("includes/rust/main.rs.init");
pub const RUST_TRAVIS_CI: &'static str = include_str!("includes/rust/.travis.yml");
pub const RUST_BENCHMARKS: &'static str = include_str!("includes/rust/benches.rs.init");

// Vim includes
pub const VIM_GITIGNORE: &'static str = include_str!("includes/vim/.gitignore");
pub const VIMBALL: &'static str = include_str!("includes/vim/vimball.txt");
pub const VIM_TEMPLATE: &'static str = include_str!("includes/vim/template.toml");
pub const VIM_TRAVIS: &'static str = include_str!("includes/vim/.travis.yml");

// Python includes
pub const PY_GITIGNORE: &'static str = include_str!("includes/python/.gitignore");
pub const PY_BIN: &'static str = include_str!("includes/python/bin.py");
pub const PY_SETUP: &'static str = include_str!("includes/python/setup.py");
pub const PY_CFG: &'static str = include_str!("includes/python/setup.cfg");
pub const PY_TEMPLATE: &'static str = include_str!("includes/python/template.toml");

// Julia includes
pub const JULIA_TEMPLATE: &'static str = include_str!("includes/julia-lib/template.toml");
pub const JULIA_GITIGNORE: &'static str = include_str!("includes/julia-lib/.gitignore");
pub const JULIA_REQUIRE: &'static str = include_str!("includes/julia-lib/REQUIRE");
pub const JULIA_TEST: &'static str = include_str!("includes/julia-lib/test/test.jl");
pub const JULIA_SRC: &'static str = include_str!("includes/julia-lib/src/{{ Project }}.jl");

// Elm includes
pub const ELM_PACKAGE: &'static str = include_str!("includes/elm/elm-package.json");
pub const ELM_MAIN: &'static str = include_str!("includes/elm/src/main.elm");
pub const ELM_STATE: &'static str = include_str!("includes/elm/src/State.elm");
pub const ELM_UPDATE: &'static str = include_str!("includes/elm/src/Update.elm");
pub const ELM_VIEW: &'static str = include_str!("includes/elm/src/View.elm");
pub const ELM_GITIGNORE: &'static str = include_str!("includes/elm/.gitignore");
pub const ELM_TEMPLATE: &'static str = include_str!("includes/elm/template.toml");
pub const ELM_CTAGS: &'static str = include_str!("includes/elm/.ctags");

// Miso includes
pub const MISO_SETUP_HS: &'static str = include_str!("includes/miso/Setup.hs");
pub const MISO_MAIN: &'static str = include_str!("includes/miso/app/Main.hs");
pub const MISO_STACK: &'static str = include_str!("includes/miso/stack.yaml");
pub const SHAKE_STACK: &'static str = include_str!("includes/miso/stack-shake.yaml");
pub const MISO_CABAL: &'static str = include_str!("includes/miso/{{ project }}.cabal");
pub const MISO_LIB: &'static str = include_str!("includes/miso/src/Lib.hs");
pub const MISO_GITIGNORE: &'static str = include_str!("includes/miso/.gitignore");
pub const MISO_TRAVIS: &'static str = include_str!("includes/miso/.travis.yml");
pub const MISO_TEMPLATE: &'static str = include_str!("includes/miso/template.toml");
pub const MISO_SHAKE: &'static str = include_str!("includes/miso/shake.hs");
pub const MISO_HTML: &'static str = include_str!("includes/miso/web-src/index.html");

// Haskell includes
pub const SETUP_HS: &'static str = include_str!("includes/haskell/Setup.hs");
pub const MAIN: &'static str = include_str!("includes/haskell/app/Main.hs");
pub const LIB: &'static str = include_str!("includes/haskell/src/Lib.hs");
pub const BENCH: &'static str = include_str!("includes/haskell/bench/Bench.hs");
pub const TEST: &'static str = include_str!("includes/haskell/test/Spec.hs");
pub const DEFAULT_NIX: &'static str = include_str!("includes/haskell/default.nix");
pub const RELEASE_NIX: &'static str = include_str!("includes/haskell/release.nix");
pub const CABAL: &'static str = include_str!("includes/haskell/project.cabal");
pub const KMETT: &'static str = include_str!("includes/kmett/project.cabal");
pub const STACK_YAML: &'static str = include_str!("includes/haskell/stack.yaml");
pub const HASK_TEMPLATE: &'static str = include_str!("includes/haskell/template.toml");
pub const HASK_YAMLLINT: &'static str = include_str!("includes/haskell/.yamllint");
pub const HASKELL_TRAVIS_CI: &'static str = include_str!("includes/haskell/.travis.yml");
pub const HASKELL_GITIGNORE: &'static str = include_str!("includes/haskell/.gitignore");
pub const CABAL_PROJECT: &'static str = include_str!("includes/haskell/cabal.project.local");
pub const HLINT_TEMPLATE: &'static str = include_str!("includes/haskell/.hlint.yaml");
pub const HASKELL_APPVEYOR: &'static str = include_str!("includes/haskell/appveyor.yml");
pub const STYLISH_HASKELL: &'static str = include_str!("includes/haskell/.stylish-haskell.yaml");
pub const HSPEC: &'static str = include_str!("includes/haskell/.hspec");

// Idiris includes
pub const IDRIS_TEMPLATE: &'static str = include_str!("includes/idris/template.toml");
pub const IDRIS_LIB: &'static str = include_str!("includes/idris/{{ Project }}/Lib.idr");
pub const IDRIS_EXE: &'static str = include_str!("includes/idris/{{ Project }}.idr");
pub const IPKG: &'static str = include_str!("includes/idris/{{ project }}.ipkg");
pub const IPKG_TEST: &'static str = include_str!("includes/idris/{{ project }}_test.ipkg");
pub const IDRIS_TEST: &'static str = include_str!("includes/idris/Spec.idr");
pub const IDRIS_GITIGNORE: &'static str = include_str!("includes/idris/.gitignore");
pub const IDRIS_CTAGS: &'static str = include_str!("includes/idris/.ctags");
