# project init (pi)

[![Build Status](https://travis-ci.org/vmchale/project-init.svg?branch=master)](https://travis-ci.org/vmchale/project-init)
[![](https://img.shields.io/crates/d/project-init.svg)](https://crates.io/crates/project-init)

`pi` is a command-line utility to initialize projects. It is written in rust.

It is intended to provide something like 
[cookiecutter](https://github.com/audreyr/cookiecutter), but faster.

Reasons to use pi:
  - You want to automate the process of starting a new project, for *all* your
    projects.
  - You want project initialization that's *quick*

Reasons to use pi over cookiecutter:
  - Templates are smaller. Define files you need in a `.toml`.
  - *Fast*. pi is **30x faster** than cookiecutter when rendering the sample vim
    plugin template.
  - pi uses mustache, a logic-less language, for templates.
  - pi can initialize a darcs, pijul, mercurial, or git repository inside your projects
  - pi provides opinionated templates for many languages
  - pi is extensible in Rust

Reasons to not use pi over cookiecutter:
  - cookiecutter uses [jinja](http://jinja.pocoo.org/) templates, which are far more sophisticated.
  - pi is newer and presumably more buggy
  - cookiecutter is extensible in Python

Benchmarks (with Haskell's [bench](https://github.com/Gabriel439/bench)):

| Tool | Language | Time (vim example plugin) | Time (rust library) |
| ---- | -------- | ------------------------- | ------------------- |
| pi init | rust | 10.10 ms | 8.809 ms |
| pi new | rust | 6.672 ms | 8.653 ms |
| cookiecutter | python | 317.1 ms | 316.9 ms |

## Installation

### Binary releases

The easiest way for most users is simply to download the prebuilt binaries.
You can find binaries for various platforms on the
[release](https://github.com/vmchale/project-init/releases) page.

### Cargo

First, install [cargo](https://rustup.rs/). Then:

```bash
 $ cargo install project_init
```

You will need to use the nightly release for this to work; if in doubt run

```bash
rustup run nightly cargo install project_init
```

## Use

`pi` reads from `$HOME/.pi_templates/` *and* your current directory. So, if you
place a template in the `$HOME/.pi_templates/idris/`, you can initialize a
project *anywhere* with

```bash
 $ pi init idris treesod
```

There is a repo containing pi templates
[here](https://github.com/vmchale/pi-templates). 

You can also use pi with built-in templates, viz. 

```bash
 $ pi new haskell really-good-project
Finished initializing project in really-good-project/
```

Or to fetch a template from github:

```bash
 $ pi git vmchale/haskell-ats ambitious-insane-project
```

### Examples

  * [haskell-ats](https://github.com/vmchale/haskell-ats) - a template for
    Haskell/ATS polyglot projects.
  * [madlang-miso](https://github.com/vmchale/madlang-miso) - a template for
    frontend using [Miso](https://haskell-miso.org/) and
    [Madlang](https://hub.darcs.net/vmchale/madlang).
  * [ats-makefile](https://github.com/vmchale/ats-makefile) - a template for ATS
    projects that uses a plain `Makefile`.

### Configuration

Global configuration is via the `$HOME/.pi.toml` file. The following is an example:

```toml
license = "BSD3"         # set default license to BSD3 
version_control = "git"  # initialize new repositories with git
version = "0.1.0"        # start new projects at version 0.1.0

[author]
name = "Vanessa McHale"
email = "vanessa.mchale@reconfigure.io"
github_username = "vmchale"

# put any custom keys you want under a [[user]] table
[[user]]
website = "https://vmchale.com"
```

Project-specific config lives in `$PROJECT_NAME/template.toml`. The following is
an example for a vim plugin:

```toml
license = "BSD3"        # overrides global value if set
with_readme = true      # add README.md

[files]
files = ["syntax/{{ project }}.vim","plugin/{{ project }}.vim","doc/{{ project }}.txt"] # blank files
directories = ["doc","syntax","plugin"]
templates = ["vimball.txt"] # files to be processed

[config]
version = "0.1.0"
version_control = "darcs"

# put any custom keys you want below [[user]]
[[user]]
vim_org_username = "vmchale"
```

This will generate the following directory structure:

```
vim-plugin
├── LICENSE
├── README.md
├── doc
│  └── vim-plugin.txt
├── plugin
│  └── vim-plugin.vim
├── syntax
│  └── vim-plugin.vim
└── vimball.txt
```

For a more in-depth example, see
[here](https://github.com/vmchale/madlang-miso). This is a template based off
the [recursion schemes
generator](http://vmchale.com/recursion-scheme-generator/index.html).

### Templates

`pi` uses [mustache](https://mustache.github.io/) for templating, via the
[rustache](https://github.com/rustache/rustache) crate.

You can find examples and help on the [mustache page](https://mustache.github.io/), or you can my look at [the example repo](https://github.com/vmchale/pi-templates).
