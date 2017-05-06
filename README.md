# project init (pi)

[![Build Status](https://travis-ci.org/vmchale/project-init.svg?branch=master)](https://travis-ci.org/vmchale/project-init)

This is a command-line utility written in rust that initializes projects based
on templates.
It is intended to provide something similar to
[cookiecutter](https://github.com/audreyr/cookiecutter), but faster. 

Reasons to use pi:
  - You want to automate the process of starting a new project, in a
    language-agnostic way.
  - You want project initialization that's *quick*

Reasons to use pi over cookiecutter:
  - Templates are smaller. Define files you need in a `.toml` rather than an
    entire directory tree.
  - *Fast*. pi **30x faster** than cookiecutter when rendering the sample vim
    plugin template.
  - pi uses mustache, a logic-less language that has libraries for *many* other
    languages. That means that you can manipulate your pi templates in other
    languages.
  - pi can initialize a git or mercurial repository inside your new project

Reasons to not use pi over cookiecutter:
  - pi does not fetch templates remotely, while cookiecutter does.
  - pi uses logic-less templates, which are not as sophisticated as the
    [jinja](http://jinja.pocoo.org/) templates that cookiecutter uses.
  - pi is a work in progress. This might mean you end up missing a feature.

Cool benchmarks (with Haskell's [bench](https://github.com/Gabriel439/bench)):

| Tool | Language | Time (vim example plugin) | Time (rust library) |
| ---- | -------- | ------------------------- | ------------------- |
| pi init | Rust | 10.10 ms | 8.809 ms |
| pi new | Rust | 6.672 ms | 8.653 ms |
| cookiecutter | Python | 317.1 ms | 316.9 ms |

## Installation

### Binary releases

You can find binaries on the 
[release](https://github.com/vmchale/project-init/releases) page. Unfortunately, 
I can only create binaries for x64 linux, ARM linux, and 64-bit Windows at this time.

### Cargo

First, install [cargo](https://rustup.rs/). Then type:

```bash
 $ cargo install project_init
```

and cargo will install pi for you. 

## Use

For use examples, check out `examples/vim-plugin`. 

Bash commands:

```bash
pi init path/to/template/dir/ new-project
```

For builtin templates (available or vim, rust, haskell, and python):

```bash
pi new rust new-project
```

### Configuration

Configuration is via the `~/.pi.toml` file. The following is an example:

```toml
[files]
files = ["syntax/{{ project }}.vim","plugin/{{ project }}.vim","doc/{{ project }}.txt"]
directories = ["doc","syntax","plugin"]
templates = ["LICENSE","README.md","vimball.txt"]

[config]
version = "0.1.0"
version_control = "git"
```

You can also set your defaults (e.g. name, email) in `~/.pi.toml`. The following is an example:

```bash
version_control = "git"

[author]
name = "Vanessa McHale"
email = "vamchale@gmail.com"
github_username = "vmchale"
```

This says your preferred version control is `git`, and sets your name & email.

### Templates

`pi` uses [mustache](https://mustache.github.io/) for templating, via the
[rustache](https://github.com/rustache/rustache) crate.

You can find examples and help on the [mustache page](https://mustache.github.io/).
