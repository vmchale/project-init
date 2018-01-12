//! Source file for the binary.
#[macro_use]
extern crate clap;
#[macro_use]
extern crate text_io;

extern crate time;
extern crate toml;
extern crate rustache;
extern crate project_init;
extern crate case;
extern crate colored;
extern crate git2;
extern crate tempdir;

use git2::Repository;
use case::*;
use clap::{App, AppSettings};
use colored::*;
use project_init::*;
use project_init::render::*;
use project_init::types::*;
use rustache::*;
use std::fs;
use std::fs::File;
use std::fs::set_permissions;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use time::strftime;
use tempdir::TempDir;

#[cfg(not(target_os = "windows"))]
use std::os::unix::fs::PermissionsExt;

#[cfg(not(target_os = "windows"))]
fn mk_executable<P: AsRef<Path>>(p: P) -> () {
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

fn main() {

    // command-line parser
    let yaml = load_yaml!("options-en.yml");
    let matches = App::from_yaml(yaml)
        .version(crate_version!())
        .set_term_width(80)
        .setting(AppSettings::SubcommandRequired)
        .get_matches();

    // set path to .pi.toml
    let home = std::env::home_dir().expect("Couldn't determine home directory.");
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
            reco_developer: None,
        }
    };

    // get year
    let now = time::now();
    let year = now.tm_year + 1900;
    let current_date = strftime("%m-%d-%Y", &now).unwrap();

    if let Some(x) = matches.subcommand_matches("update") {

        let force = x.is_present("force");

        println!("current version: {}", crate_version!());

        let s = if force {
            "curl -LSfs https://japaric.github.io/trust/install.sh | sh -s -- --git vmchale/project-init --force"
        } else {
            "curl -LSfs https://japaric.github.io/trust/install.sh | sh -s -- --git vmchale/project-init"
        };

        let script = Command::new("bash").arg("-c").arg(s).output().expect(
            "failed to execute update script.",
        );

        let script_string = String::from_utf8(script.stderr).unwrap();

        println!("{}", script_string);

    } else if let Some(_) = matches.subcommand_matches("list") {
        let builtin = vec![
            "rust",
            "vim",
            "python",
            "haskell",
            "idris",
            "reco",
            "julia",
            "elm",
            "miso",
            "plain",
            "kmett",
            "madlang",
        ];
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
                    match dir {
                        Ok(x) => {
                            if x.path().is_dir() &&
                                x.file_name().to_str().map(|c| c.chars().nth(0).unwrap()) !=
                                    Some('.') &&
                                x.file_name().to_str().map(|c| c.chars().nth(0).unwrap()) !=
                                    Some('_')
                            {
                                println!("  - {}", x.file_name().to_string_lossy());
                            }
                        }
                        _ => (),
                    }
                }
            }
            _ => eprintln!("{}: Could not access {}", "Warning".yellow(), p.display()),
        }

    } else if let Some(matches_init) = matches.subcommand_matches("git") {

        // whether to overwrite
        let force = matches_init.is_present("force");

        // get repository name
        let repo = matches_init.value_of("repo").expect("Clap failed to supply repository name");

        // get project name
        let name = matches_init.value_of("name").expect(
            "Clap failed to supply project name",
        );

        // form the URL
        let mut url = "https://github.com/".to_string();
        url.push_str(repo);

        // create a temporary directory to hold the template
        let dir_name = repo.replace("/", "-");
        let tmp_dir = TempDir::new(&dir_name);
        let file = match tmp_dir {
            Ok(t) => t,
            Err(_) => {eprintln!("{}: failed to create temporary directory", "Error".red()) ; std::process::exit(1)},
        };

        // clone into the temporary directory
        let file_path = file.path();
        let _ = match Repository::clone(&url, file_path) {
            Ok(_) => (),
            Err(_) => {eprintln!("{}: failed to clone repo at {}", "Error".red(), url) ; std::process::exit(1)},
        };

        let string_dir = file_path.to_string_lossy().to_string();
        let mut toml_string = string_dir.clone();
        toml_string.push_str("/template.toml");

        // get the parsed TOML file from the repo.
        let (parsed_toml, _) = read_toml_dir(&toml_string, PathBuf::from("."));

        // initialize the project
        init_helper(home, &string_dir, decoded, author, name, year, &current_date, force, parsed_toml, false)

    } else if let Some(matches_init) = matches.subcommand_matches("new") {

        let force: bool = matches_init.occurrences_of("force") == 1;

        // get project name
        let name = matches_init.value_of("name").expect(
            "Clap failed to supply project name",
        );

        // get project template type
        let template_str_lower: String = matches_init
            .value_of("template")
            .expect("Clap failed to supply project directory")
            .to_string()
            .chars()
            .map(|c| c.to_lowercase().to_string())
            .collect::<Vec<String>>()
            .join("");

        let template_str = template_str_lower.as_str();

        // read template.toml
        let toml_file = match template_str {
            "rust" => includes::RUST_TEMPLATE,
            "vim" | "vimscript" => includes::VIM_TEMPLATE,
            "python" => includes::PY_TEMPLATE,
            "haskell" | "kmett" => includes::HASK_TEMPLATE,
            "mad" | "madlang" => includes::MADLANG_TEMPLATE,
            "idris" => includes::IDRIS_TEMPLATE,
            "reco" => includes::RECO_TEMPLATE,
            "julia" => includes::JULIA_TEMPLATE,
            "elm" => includes::ELM_TEMPLATE,
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
        let version = if let Some(config) = parsed_config.clone() {
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
            .insert("project", name)
            .insert("Project", name.to_capitalized())
            .insert("year", year)
            .insert("name", author.name)
            .insert("version", version)
            .insert("email", author.email)
            .insert("github_username", github_username)
            .insert("license", license_name)
            .insert("date", current_date);

        // check if the directory exists and exit, if we haven't forced an overwrite.
        if Path::new(name).exists() && !force {
            println!(
                "Path '{}' already exists. Rerun with -f or --force to overwrite.",
                name
            );
            std::process::exit(0x0f00);
        };

        // create directories
        let _ = fs::create_dir(name);
        if let Some(dirs_pre) = parsed_dirs.directories {
            render_dirs(dirs_pre, &hash, name);
        }

        // Create files.
        let files = if let Some(files_pre) = parsed_dirs.files {
            render_files(files_pre, &hash, name)
        } else {
            VecBuilder::new()
        };

        // create license if it was asked for
        if let Some(lic) = license_contents {
            render_file(lic, name, "LICENSE", &hash);
        }

        // render readme if requested
        if let Some(readme) = parsed_toml.with_readme {
            if readme {
                render_file(includes::README, name, "README.md", &hash);
            }
        }

        let hash_with_files = HashBuilder::new().insert("files", files);

        // render appropriate stuff by name.
        match template_str {
            "plain" => (),

            "rust" => {
                let mut bench_path = "benches/".to_string();
                bench_path.push_str(name);
                bench_path.push_str(".rs");
                write_file_plain(includes::RUST_LIB, name, "src/lib.rs");
                write_file_plain(includes::RUST_MAIN, name, "src/main.rs");
                write_file_plain(includes::RUST_TRAVIS_CI, name, ".travis.yml");
                write_file_plain(includes::RUST_GITIGNORE, name, ".gitignore");
                write_file_plain(includes::RUST_BENCHMARKS, name, &bench_path);
                render_file(includes::CARGO_TOML, name, "Cargo.toml", &hash)
            }

            "vim" | "vimscript" => {
                write_file_plain(includes::VIM_GITIGNORE, name, ".gitignore");
                render_file(includes::VIM_TRAVIS, name, ".travis.yml", &hash_with_files);
                render_file(includes::VIMBALL, name, "vimball.txt", &hash_with_files)
            }

            "python" => {
                render_file(includes::PY_SETUP, name, "setup.py", &hash);
                write_file_plain(includes::PY_CFG, name, "setup.cfg");
                write_file_plain(includes::PY_GITIGNORE, name, ".gitignore");
                let mut bin_path = "bin/".to_string();
                bin_path.push_str(name);
                render_file(includes::PY_BIN, name, &bin_path, &hash);
            }

            "elm" => {
                write_file_plain(includes::ELM_GITIGNORE, name, ".gitignore");
                write_file_plain(includes::ELM_MAIN, name, "src/main.elm");
                write_file_plain(includes::ELM_STATE, name, "src/State.elm");
                write_file_plain(includes::ELM_UPDATE, name, "src/Update.elm");
                write_file_plain(includes::ELM_VIEW, name, "src/View.elm");
                write_file_plain(includes::ELM_CTAGS, name, ".ctags");
                render_file(includes::ELM_PACKAGE, name, "elm-package.json", &hash);
            }

            "miso" => {
                write_file_plain(includes::MISO_SETUP_HS, name, "Setup.hs");
                write_file_plain(includes::MISO_MAIN, name, "app/Main.hs");
                write_file_plain(includes::MISO_LIB, name, "src/Lib.hs");
                let mut cabal_path = name.to_string();
                cabal_path.push_str(".cabal");
                render_file(includes::MISO_CABAL, name, &cabal_path, &hash);
                write_file_plain(includes::MISO_GITIGNORE, name, ".gitignore");
                render_file(includes::MISO_STACK, name, "stack.yaml", &hash);
                write_file_plain(includes::HLINT_TEMPLATE, name, ".hlint.yaml");
                write_file_plain(includes::SHAKE_STACK, name, "stack-shake.yaml");
                write_file_plain(includes::MISO_TRAVIS, name, ".travis.yml");
                render_file(includes::MISO_SHAKE, name, "shake.hs", &hash);
                render_file(includes::MISO_HTML, name, "web-src/index.html", &hash);
                write_file_plain(includes::HASKELL_TRAVIS_CI, name, ".travis.yml");
                write_file_plain(includes::STYLISH_HASKELL, name, ".stylish-haskell.yaml");
                let mut shake_path = name.to_string();
                shake_path.push_str("/shake.hs");
                mk_executable(shake_path);
            }

            "reco" => {
                // if author.reco_developer == Some(true) {
                // write_file_plain(includes::RECO_RULES, name, "optim/default.rules");
                // write_file_plain(includes::RECO_JUSTFILE, name, "Justfile");
                // }
                write_file_plain(includes::RECO_MAIN, name, "main.go");
                let mut command_path = "cmd/test-".to_string();
                command_path.push_str(name);
                command_path.push_str("/main.go");
                write_file_plain(includes::RECO_TEST_COMMAND, name, &command_path);
                render_file(includes::RECO_README, name, "README.md", &hash);
                if author.reco_developer != Some(true) {
                    let mut optim_path: String = name.to_string();
                    optim_path.push_str("/optim");
                    std::fs::remove_dir(optim_path).unwrap();
                }
            }

            "madlang" | "mad" => {
                let mut src_path = "src/".to_string();
                src_path.push_str(name);
                src_path.push_str(".mad");
                render_file(includes::MADLANG_SRC, name, &src_path, &hash);
            }

            "idris" => {
                let mut pkg_path = name.to_string();
                pkg_path.push_str(".ipkg");
                write_file_plain(includes::IDRIS_GITIGNORE, name, ".gitignore");
                write_file_plain(includes::IDRIS_CTAGS, name, ".ctags");
                let mut main_path = name.to_capitalized();
                main_path.push_str(".idr");
                render_file(includes::IPKG, name, &pkg_path, &hash);
                render_file(includes::IPKG_TEST, name, "test.ipkg", &hash);
                // render_file(includes::IDRIS_EXE, name, &main_path, &hash);
                render_file(includes::IDRIS_TEST, name, "src/Test/Spec.idr", &hash);
                let mut lib_path = "src/".to_string();
                lib_path.push_str(&name.to_capitalized());
                lib_path.push('/');
                lib_path.push_str("Lib.idr");
                render_file(includes::IDRIS_LIB, name, &lib_path, &hash);
            }

            "julia" => {
                write_file_plain(includes::JULIA_REQUIRE, name, "REQUIRE");
                let mut project_path = "src/".to_string();
                project_path.push_str(name.to_capitalized().as_str());
                project_path.push_str(".jl");
                write_file_plain(includes::JULIA_GITIGNORE, name, ".gitignore");
                write_file_plain(includes::JULIA_SRC, name, &project_path);
                write_file_plain(includes::JULIA_TEST, name, "test/test.jl");
            }

            "ats" => {
                write_file_plain(includes::ATS_CTAGS, name, ".ctags");
                let mut src_path = "src/".to_string();
                src_path.push_str(name);
                src_path.push_str(".dats");
                render_file(includes::ATS_SRC, name, &src_path, &hash);
                render_file(includes::ATS_SHAKE, name, "shake.hs", &hash);
                write_file_plain(includes::ATS_FORMAT, name, ".atsfmt.toml");
                let mut shake_path = name.to_string();
                shake_path.push_str("/shake.hs");
                mk_executable(shake_path);
            }

            "haskell" | "kmett" => {
                write_file_plain(includes::SETUP_HS, name, "Setup.hs");
                write_file_plain(includes::MAIN, name, "app/Main.hs");
                render_file(includes::LIB, name, "src/Lib.hs", &hash);
                write_file_plain(includes::BENCH, name, "bench/Bench.hs");
                write_file_plain(includes::TEST, name, "test/Spec.hs");
                write_file_plain(includes::HLINT_TEMPLATE, name, ".hlint.yaml");
                write_file_plain(includes::STYLISH_HASKELL, name, ".stylish-haskell.yaml");
                render_file(includes::DEFAULT_NIX, name, "default.nix", &hash);
                render_file(includes::RELEASE_NIX, name, "release.nix", &hash);
                let mut cabal_path = name.to_string();
                cabal_path.push_str(".cabal");
                if template_str == "haskell" {
                    render_file(includes::CABAL, name, &cabal_path, &hash);
                } else {
                    render_file(includes::KMETT, name, &cabal_path, &hash);
                }
                write_file_plain(includes::HASKELL_GITIGNORE, name, ".gitignore");
                write_file_plain(includes::RELEASE_NIX, name, "release.nix");
                render_file(includes::STACK_YAML, name, "stack.yaml", &hash);
                render_file(includes::CABAL_PROJECT, name, "cabal.project.local", &hash);
                render_file(includes::HASKELL_TRAVIS_CI, name, ".travis.yml", &hash);
                render_file(includes::HASKELL_APPVEYOR, name, "appveyor.yml", &hash);
            }

            _ => std::process::exit(0x0f01),
        };

        // initialize version control
        if let Some(vc) = decoded.version_control {
            match vc.as_str() {
                "git" => repo::git_init(name),
                "hg" | "mercurial" => repo::hg_init(name),
                "pijul" => repo::pijul_init(name),
                "darcs" => repo::darcs_init(name),
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
        println!("Finished initializing project in {}/", name);

    } else if let Some(matches_init) = matches.subcommand_matches("init") {

        let force: bool = matches_init.occurrences_of("force") == 1;

        // get project name
        let name = matches_init.value_of("name").expect(
            "Failed to supply project name",
        );

        // get project directory
        let project_dir = matches_init.value_of("directory").expect(
            "Failed to supply project directory",
        );

        // read template.toml for template
        let mut template_path = project_dir.to_string();
        template_path.push_str("/template.toml");
        let (parsed_toml, is_global_project) = read_toml_dir(&template_path, home.clone());

        init_helper(home, project_dir, decoded, author, name, year, &current_date, force, parsed_toml, is_global_project)
    }
}
