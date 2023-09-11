//! Source file for the binary.
#[macro_use]
extern crate text_io;

extern crate case;
extern crate colored;
extern crate dirs;
extern crate git2;
extern crate project_init;
extern crate rustache;
extern crate tempdir;
extern crate time;
extern crate toml;

use case::CaseExt;
use colored::*;
use project_init::render::*;
use project_init::types::*;
use project_init::*;
use rustache::*;
use std::fs;
use std::fs::set_permissions;
use std::fs::File;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use tempdir::TempDir;

#[derive(clap::Parser)]
#[clap(name = "Project Init (pi)")]
#[clap(author = "Vanessa McHale <vamchale@gmail.com>")]
#[clap(about = "Initialize projects from a template.")]
#[clap(after_help = "See 'man pi' for more information")]
struct App {
    #[clap(subcommand)]
    command: Subcommand,
}

#[derive(clap::Subcommand)]
enum Subcommand {
    /// Fetch a template from github.
    #[clap(visible_alias = "g")]
    Git(GitCommand),

    /// List available templates.
    ///
    /// User templates can be added by placing them in `~/.pi_templates`.
    #[clap(visible_alias = "l")]
    List,

    /// Update pi (only works on UNIX).
    #[clap(visible_alias = "u")]
    Update(UpdateCommand),

    /// Use a template from a folder.
    #[clap(visible_alias = "i")]
    Init(InitCommand),

    /// Use a built-in template.
    #[clap(visible_alias = "n")]
    New(NewCommand),
}

#[derive(clap::Parser)]
struct GitCommand {
    /// User and repository name where the template is located.
    #[clap(value_name = "USER/REPO")]
    repo: String,

    /// Project name to be used for project directory.
    #[clap(value_name = "NAME")]
    name: String,

    /// Initialize project even if directory already exists.
    #[clap(long, short)]
    force: bool,
}

#[derive(clap::Parser)]
struct UpdateCommand {
    /// Force installation even when binary already exists.
    #[clap(long, short)]
    force: bool,
}

#[derive(clap::Parser)]
struct InitCommand {
    /// Directory containing your template, either in the current directory or in `$HOME/.pi_templates/`.
    #[clap(value_name = "TEMPLATE_DIR")]
    directory: PathBuf,

    /// Project name to be used for project directory.
    #[clap(value_name = "NAME")]
    name: String,

    /// Initialize project even if directory already exists.
    #[clap(long, short)]
    force: bool,
}

#[derive(clap::Parser)]
struct NewCommand {
    /// Template to be used
    ///
    /// Currently supported are Rust, Haskell, Idris, Elm, Python, Vimscript, Miso, and Julia.
    #[clap(value_name = "TEMPLATE_DIR")]
    template: String,

    /// Project name to be used for project directory.
    #[clap(value_name = "NAME")]
    name: String,

    /// Initialize project even if directory already exists.
    #[clap(long, short)]
    force: bool,
}

#[cfg(not(target_os = "windows"))]
use std::os::unix::fs::PermissionsExt;

#[cfg(not(target_os = "windows"))]
fn mk_executable<P: AsRef<Path>>(p: P) {
    let f = File::open(&p).unwrap();
    let metadata = f.metadata().unwrap();
    let mut permissions = metadata.permissions();
    permissions.set_mode(0o755);
    set_permissions(p, permissions).unwrap();
}

#[cfg(target_os = "windows")]
fn mk_executable<P: AsRef<Path>>(_: P) -> () {
    ()
}

#[allow(clippy::cognitive_complexity)]
#[allow(clippy::print_literal)]
fn main() {
    // command-line parser
    let args: App = clap::Parser::parse();

    // set path to .pi.toml
    let home = dirs::home_dir().expect("Couldn't determine home directory.");
    let mut path = home.clone();
    path.push(".pi.toml");

    // read global config file
    let decoded: Config = read_toml_config(&path);

    // create author struct
    let author = if let Some(aut) = decoded.clone().author {
        aut
    } else {
        println!("Enter your name");
        let nam: String = read!("{}");
        println!("Enter your email");
        let ema: String = read!("{}");
        Author {
            name: nam,
            email: ema,
            github_username: None,
        }
    };

    // get year
    let now = time::OffsetDateTime::now_utc();
    let current_date = format!("{:02}-{:02}-{:04}", now.month() as u8, now.day(), now.year());

    match args.command {
        Subcommand::Update(args) => {
            println!("current version: {}", env!("CARGO_PKG_VERSION"));

            let s = if args.force {
                "curl -LSfs https://japaric.github.io/trust/install.sh | sh -s -- --git vmchale/project-init --force"
            } else {
                "curl -LSfs https://japaric.github.io/trust/install.sh | sh -s -- --git vmchale/project-init"
            };

            let script = Command::new("bash")
                .arg("-c")
                .arg(s)
                .output()
                .expect("failed to execute update script.");

            let script_string = String::from_utf8(script.stderr).unwrap();

            println!("{}", script_string);
        },

        Subcommand::List => {
            let remote = vec!["vmchale/haskell-ats", "vmchale/madlang-miso"];
            let builtin = vec![
                "rust", "vim", "python", "haskell", "idris", "julia", "elm", "miso", "plain", "kmett",
                "madlang",
            ];
            println!("{}", "Remote Templates:".cyan());
            for b in remote {
                println!("  - {}", b);
            }
            println!();
            println!("{}", "Builtin Templates:".cyan());
            for b in builtin {
                println!("  - {}", b);
            }
            let mut p = home;
            p.push(".pi_templates");
            println!("{}", "\nUser Templates:".cyan());
            let iter = std::fs::read_dir(&p);
            match iter {
                Ok(x) => {
                    for dir in x {
                        if let Ok(x) = dir {
                            if x.path().is_dir()
                                && x.file_name().to_str().map(|c| c.chars().next().unwrap())
                                    != Some('.')
                                && x.file_name().to_str().map(|c| c.chars().next().unwrap())
                                    != Some('_')
                            {
                                println!("  - {}", x.file_name().to_string_lossy());
                            }
                        } else {
                        }
                    }
                }
                _ => eprintln!("{}: Could not access {}", "Warning".yellow(), p.display()),
            }
        },

        Subcommand::Git(args) => {
            // form the URL
            let url = format!("https://github.com/{}", args.repo);

            // create a temporary directory to hold the template
            let dir_name = args.repo.replace("/", "-");
            let tmp_dir = TempDir::new(&dir_name);
            let file = match tmp_dir {
                Ok(t) => t,
                Err(_) => {
                    eprintln!("{}: failed to create temporary directory", "Error".red());
                    std::process::exit(1)
                }
            };

            // clone into the temporary directory
            let file_path = file.path();

            let git_auth = auth_git2::GitAuthenticator::default();
            match git_auth.clone_repo(&url, file_path) {
                Ok(_) => (),
                Err(_) => {
                    eprintln!("{}: failed to clone repo at {}", "Error".red(), url);
                    std::process::exit(1)
                }
            };

            let string_dir = file_path.to_string_lossy().to_string();
            let mut toml_string = string_dir.clone();
            toml_string.push_str("/template.toml");

            // get the parsed TOML file from the repo.
            let (parsed_toml, _) = read_toml_dir(&toml_string, PathBuf::from("."));

            // initialize the project
            init_helper(
                home,
                &string_dir,
                decoded,
                author,
                &args.name,
                now.year(),
                &current_date,
                args.force,
                parsed_toml,
                false,
            )
        },
        Subcommand::New(args) => {

        let template_str_lower = args.template.to_lowercase();

        // read template.toml
        let toml_file = match template_str_lower.as_str() {
            "rust" => includes::RUST_TEMPLATE,
            "vim" | "vimscript" => includes::VIM_TEMPLATE,
            "python" => includes::PY_TEMPLATE,
            "haskell" | "kmett" => includes::HASK_TEMPLATE,
            "mad" | "madlang" => includes::MADLANG_TEMPLATE,
            "idris" => includes::IDRIS_TEMPLATE,
            "julia" => includes::JULIA_TEMPLATE,
            "miso" => includes::MISO_TEMPLATE,
            "plain" => includes::PLAIN_TEMPLATE,
            "ats" => includes::ATS_TEMPLATE,
            _ => {
                println!("The requested template is not a built-in :(");
                std::process::exit(0x0f00)
            }
        };
        let parsed_toml = read_toml_str(toml_file, "BUILTIN");
        let parsed_dirs = parsed_toml.files;
        let parsed_config = parsed_toml.config;

        // set license if it's set
        let (license_contents, license_name) =
            // prefer global license over builtin
            if let Some(l) = decoded.license {
                match l.as_str() {
                    "BSD3" => (Some(includes::BSD3), "BSD3"),
                    "BSD" => (Some(includes::BSD), "BSD"),
                    "MIT" => (Some(includes::MIT), "MIT"),
                    "GPL3" => (Some(includes::GPL3), "GLP3"),
                    "AllRightsReserved" => (Some(includes::BSD3), "AllRightsReserved"),
                    _ => { println!("{}: requested license not found. Defaulting to AllRightsReserved","Warning".yellow()) 
                           ; (Some(includes::ALL_RIGHTS_RESERVED), "AllRightsReserved") }
                }
            }
            else if let Some(l) = parsed_toml.license {
                match l.as_str() {
                    "BSD3" => (Some(includes::BSD3), "BSD3"),
                    "BSD" => (Some(includes::BSD), "BSD"),
                    "MIT" => (Some(includes::MIT), "MIT"),
                    "GPL3" => (Some(includes::GPL3), "GLP3"),
                    "AllRightsReserved" => (Some(includes::BSD3), "AllRightsReserved"),
                    _ => { println!("{}: requested license not found. Defaulting to AllRightsReserved","Warning".yellow()) 
                           ; (Some(includes::ALL_RIGHTS_RESERVED), "AllRightsReserved") }
                }
            }
            else {
                (None,"")
            };

        // set version
        let version = if let Some(config) = parsed_config {
            if let Some(v) = config.version {
                v
            } else {
                "0.1.0".to_string()
            }
        } else {
            println!(
                "{}: no version info found, defaulting to '0.1.0'",
                "Warning".yellow()
            );
            "0.1.0".to_string()
        };

        // set github username to null if it's not provided
        let github_username = if let Some(uname) = author.github_username {
            uname
        } else {
            println!(
                "{}: No github username found, defaulting to null.",
                "Warning".yellow()
            );
            "".to_string()
        };

        // Make a hash for inserting stuff into templates.
        let hash = HashBuilder::new()
            .insert("project", args.name.as_str())
            .insert("Project", args.name.to_capitalized())
            .insert("year", now.year())
            .insert("name", author.name)
            .insert("version", version)
            .insert("email", author.email)
            .insert("github_username", github_username)
            .insert("license", license_name)
            .insert("date", current_date);

        // check if the directory exists and exit, if we haven't forced an overwrite.
        if Path::new(&args.name).exists() && !args.force {
            println!(
                "Path '{}' already exists. Rerun with -f or --force to overwrite.",
                args.name
            );
            std::process::exit(0x0f00);
        };

        // create directories
        let _ = fs::create_dir(&args.name);
        if let Some(dirs_pre) = parsed_dirs.directories {
            render_dirs(dirs_pre, &hash, &args.name);
        }

        // Create files.
        let files = if let Some(files_pre) = parsed_dirs.files {
            render_files(files_pre, &hash, &args.name)
        } else {
            VecBuilder::new()
        };

        // create license if it was asked for
        if let Some(lic) = license_contents {
            render_file(lic, &args.name, "LICENSE", &hash);
        }

        // render readme if requested
        if let Some(readme) = parsed_toml.with_readme {
            if readme {
                render_file(includes::README, &args.name, "README.md", &hash);
            }
        }

        let hash_with_files = HashBuilder::new().insert("files", files);

        // render appropriate stuff by name.
        match template_str_lower.as_str() {
            "plain" => (),

            "rust" => {
                let mut bench_path = "benches/".to_string();
                bench_path.push_str(&args.name);
                bench_path.push_str(".rs");
                write_file_plain(includes::RUST_LIB, &args.name, "src/lib.rs");
                write_file_plain(includes::RUST_MAIN, &args.name, "src/main.rs");
                write_file_plain(includes::RUST_TRAVIS_CI, &args.name, ".travis.yml");
                write_file_plain(includes::RUST_GITIGNORE, &args.name, ".gitignore");
                write_file_plain(includes::RUST_BENCHMARKS, &args.name, &bench_path);
                render_file(includes::CARGO_TOML, &args.name, "Cargo.toml", &hash)
            }

            "vim" | "vimscript" => {
                write_file_plain(includes::VIM_GITIGNORE, &args.name, ".gitignore");
                render_file(includes::VIM_TRAVIS, &args.name, ".travis.yml", &hash_with_files);
                render_file(includes::VIMBALL, &args.name, "vimball.txt", &hash_with_files)
            }

            "python" => {
                render_file(includes::PY_SETUP, &args.name, "setup.py", &hash);
                write_file_plain(includes::PY_CFG, &args.name, "setup.cfg");
                write_file_plain(includes::PY_GITIGNORE, &args.name, ".gitignore");
                let mut bin_path = "bin/".to_string();
                bin_path.push_str(&args.name);
                render_file(includes::PY_BIN, &args.name, &bin_path, &hash);
            }

            "miso" => {
                write_file_plain(includes::MISO_SETUP_HS, &args.name, "Setup.hs");
                write_file_plain(includes::MISO_MAIN, &args.name, "app/Main.hs");
                write_file_plain(includes::MISO_LIB, &args.name, "src/Lib.hs");
                let cabal_path = format!("{}.cabal", args.name);
                render_file(includes::MISO_CABAL, &args.name, &cabal_path, &hash);
                write_file_plain(includes::MISO_GITIGNORE, &args.name, ".gitignore");
                render_file(includes::MISO_STACK, &args.name, "stack.yaml", &hash);
                write_file_plain(includes::HLINT_TEMPLATE, &args.name, ".hlint.yaml");
                write_file_plain(includes::SHAKE_STACK, &args.name, "stack-shake.yaml");
                write_file_plain(includes::MISO_TRAVIS, &args.name, ".travis.yml");
                render_file(includes::MISO_SHAKE, &args.name, "shake.hs", &hash);
                render_file(includes::MISO_HTML, &args.name, "web-src/index.html", &hash);
                write_file_plain(includes::HASKELL_TRAVIS_CI, &args.name, ".travis.yml");
                write_file_plain(includes::STYLISH_HASKELL, &args.name, ".stylish-haskell.yaml");
                let shake_path = format!("{}/shake.hs", args.name);
                mk_executable(shake_path);
            }

            "madlang" | "mad" => {
                let src_path = format!("src/{}.mad", args.name);
                render_file(includes::MADLANG_SRC, &args.name, &src_path, &hash);
            }

            "idris" => {
                let pkg_path = format!("{}.ipkg", args.name);
                write_file_plain(includes::IDRIS_GITIGNORE, &args.name, ".gitignore");
                write_file_plain(includes::IDRIS_CTAGS, &args.name, ".ctags");
                render_file(includes::IPKG, &args.name, &pkg_path, &hash);
                render_file(includes::IPKG_TEST, &args.name, "test.ipkg", &hash);
                // let main_path = format!("{}.idr", args.name.to_capitalized());
                // render_file(includes::IDRIS_EXE, args.name, &main_path, &hash);
                render_file(includes::IDRIS_TEST, &args.name, "src/Test/Spec.idr", &hash);
                let lib_path = format!("src/{}/Lib.idr", args.name.to_capitalized());
                render_file(includes::IDRIS_LIB, &args.name, &lib_path, &hash);
            }

            "julia" => {
                write_file_plain(includes::JULIA_REQUIRE, &args.name, "REQUIRE");
                let project_path = format!("src/{}.jl", args.name.to_capitalized());
                write_file_plain(includes::JULIA_GITIGNORE, &args.name, ".gitignore");
                write_file_plain(includes::JULIA_SRC, &args.name, &project_path);
                write_file_plain(includes::JULIA_TEST, &args.name, "test/test.jl");
            }

            "ats" => {
                write_file_plain(includes::ATS_CTAGS, &args.name, ".ctags");
                let src_path = format!("src/{}.dats", args.name);
                render_file(includes::ATS_SRC, &args.name, &src_path, &hash);
                write_file_plain(includes::ATS_FORMAT, &args.name, ".atsfmt.toml");
                write_file_plain(includes::ATS_TRAVIS, &args.name, ".clang-format");
                render_file(includes::ATS_PKG, &args.name, "atspkg.dhall", &hash);
                render_file(includes::ATS_LIB, &args.name, "pkg.dhall", &hash);
                render_file(includes::ATS_TRAVIS, &args.name, ".travis.yml", &hash);
                render_file(includes::ATS_GITIGNORE, &args.name, ".gitignore", &hash);
            }

            "haskell" | "kmett" => {
                write_file_plain(includes::SETUP_HS, &args.name, "Setup.hs");
                write_file_plain(includes::MAIN, &args.name, "app/Main.hs");
                render_file(includes::LIB, &args.name, "src/Lib.hs", &hash);
                write_file_plain(includes::BENCH, &args.name, "bench/Bench.hs");
                write_file_plain(includes::TEST, &args.name, "test/Spec.hs");
                write_file_plain(includes::HLINT_TEMPLATE, &args.name, ".hlint.yaml");
                write_file_plain(includes::STYLISH_HASKELL, &args.name, ".stylish-haskell.yaml");
                render_file(includes::DEFAULT_NIX, &args.name, "default.nix", &hash);
                render_file(includes::RELEASE_NIX, &args.name, "release.nix", &hash);
                let cabal_path = format!("{}.cabal", args.name);
                if template_str_lower == "haskell" {
                    render_file(includes::CABAL, &args.name, &cabal_path, &hash);
                } else {
                    render_file(includes::KMETT, &args.name, &cabal_path, &hash);
                }
                write_file_plain(includes::HASKELL_GITIGNORE, &args.name, ".gitignore");
                write_file_plain(includes::RELEASE_NIX, &args.name, "release.nix");
                write_file_plain(includes::HSPEC, &args.name, ".hspec");
                write_file_plain(includes::HS_GITATTRIBUTES, &args.name, ".gitattributes");
                render_file(includes::STACK_YAML, &args.name, "stack.yaml", &hash);
                render_file(includes::CABAL_PROJECT, &args.name, "cabal.project.local", &hash);
                render_file(includes::HASKELL_TRAVIS_CI, &args.name, ".travis.yml", &hash);
                render_file(includes::HASKELL_APPVEYOR, &args.name, "appveyor.yml", &hash);
                render_file(includes::HS_CHANGELOG, &args.name, "CHANGELOG.md", &hash);
            }

            _ => std::process::exit(0x0f01),
        };

        // initialize version control
        if let Some(vc) = decoded.version_control {
            match vc.as_str() {
                "git" => repo::git_init(&args.name),
                "hg" | "mercurial" => repo::hg_init(&args.name),
                "pijul" => repo::pijul_init(&args.name),
                "darcs" => repo::darcs_init(&args.name),
                _ => {
                    eprintln!(
                        "{}: version control {} is not yet supported. Supported version control tools are darcs, pijul, mercurial, and git.",
                        "Error".red(),
                        vc
                    );
                }
            }
        }

        // Print that we're done
        println!("Finished initializing project in {}/", args.name);
    },
    Subcommand::Init(args) => {
        let project_dir = args.directory.to_string_lossy();
        // read template.toml for template
        let template_path = format!("{}/template.toml", project_dir);
        let (parsed_toml, is_global_project) = read_toml_dir(&template_path, home.clone());

        init_helper(
            home,
            &project_dir,
            decoded,
            author,
            &args.name,
            now.year(),
            &current_date,
            args.force,
            parsed_toml,
            is_global_project,
        )
    },
    }
}
