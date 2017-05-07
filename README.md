# project init (pi)

[![Build Status](https://travis-ci.org/vmchale/project-init.svg?branch=master)](https://travis-ci.org/vmchale/project-init)

`pi` is a command-line utility to initialize projects. It is written in rust.

It is intended to provide something like 
[cookiecutter](https://github.com/audreyr/cookiecutter), but faster.

Reasons to use pi:
  - You want to automate the process of starting a new project, in a
    language-agnostic way.
  - You want project initialization that's *quick*

Reasons to use pi over cookiecutter:
  - Templates are smaller. Define files you need in a `.toml`.
  - *Fast*. pi **30x faster** than cookiecutter when rendering the sample vim
    plugin template.
  - pi uses mustache, a logic-less language that has libraries for *many* other
    languages.
  - pi can initialize a git or mercurial repository inside your new project

Reasons to not use pi over cookiecutter:
  - pi does not fetch templates remotely.
  - pi uses logic-less templates, which are not as sophisticated as the
    [jinja](http://jinja.pocoo.org/) templates that cookiecutter uses.
  - pi is a work in progress. It does not yet have custom keys.

Cool benchmarks (with Haskell's [bench](https://github.com/Gabriel439/bench)):

| Tool | Language | Time (vim example plugin) | Time (rust library) |
| ---- | -------- | ------------------------- | ------------------- |
| pi init | rust | 10.10 ms | 8.809 ms |
| pi new | rust | 6.672 ms | 8.653 ms |
| cookiecutter | python | 317.1 ms | 316.9 ms |

## Installation

### Binary releases

You can find binaries for x64 linux, ARM linux, and x64-windows on the
[release](https://github.com/vmchale/project-init/releases) page.

### Cargo

First, install [cargo](https://rustup.rs/). Then:

```bash
 $ cargo install project_init
```

## Use

The easiest way to use pi is with the builtin templates:

```bash
$ pi new haskell new-project
Finished initializing project in new-project/
```

For a custom template:

```bash
$ pi init path/to/template/dir/ new-project
Finished initializing project in new-project/
```

Note that you can put templates in `$HOME/.pi_templates` and they can then be
called from anywhere.

For template examples, check out
[pi-templates](https://github.com/vmchale/pi-templates). 

### Configuration

Global configuration is via the `~/.pi.toml` file. The following is an example:

```toml
license = "BSD3"
version_control = "git"
version = "0.1.0"

[author]
name = "Vanessa McHale"
email = "vamchale@gmail.com"
github_username = "vmchale"
```

Project-specific config lives in `$PROJECT_NAME/template.toml`. The following is
an example for a vim plugin:

```toml
license = "BSD3"
with_readme = true

[files]
files = ["syntax/{{ project }}.vim","plugin/{{ project }}.vim","doc/{{ project }}.txt"] # blank files
directories = ["doc","syntax","plugin"]
templates = ["vimball.txt"] # files to be processed

[config]
version = "0.1.0"
version_control = "git"
```

### Templates

`pi` uses [mustache](https://mustache.github.io/) for templating, via the
[rustache](https://github.com/rustache/rustache) crate.

You can find examples and help on the [mustache page](https://mustache.github.io/).
