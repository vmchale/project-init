# project init (pi)

[![Build Status](https://travis-ci.org/vmchale/project-init.svg?branch=master)](https://travis-ci.org/vmchale/project-init)

This is intended to provide something similar to
[cookiecutter](https://github.com/audreyr/cookiecutter), albeit faster. It is
somewhat more limited that cookiecutter, but for most users the difference will
be imperceptible (though it *will* be noticeably faster!).

Cool benchmarks (with Haskell's [bench](https://github.com/Gabriel439/bench)):

| Tool | Language | Time (vim example plugin) |
| ---- | -------- | ------------------------- |
| pi | Rust | 317.1 ms |
| cookiecutter | Python | 10.10 ms |

You can find a bash script to benchmark them in `bash/bench`

Reasons to use pi:
  - Templates are smaller. Define files you need in a `.toml` rather than an
    entire directory tree.
  - *Fast*. pi **30x faster** than cookiecutter when rendering the sample vim
    plugin template.
  - pi uses mustache, a logic-less language that has libraries for *many* other
    languages. That means that you can manipulate your pi templates in other
    languages.
  - pi can initialize a git or mercurial repository inside your new project

Reasons to not use pi:
  - pi does not (currently) fetch templates remotely.
  - pi uses logic-less templates, which are not as sophisticated as the
    [jinja](http://jinja.pocoo.org/) templates that cookiecutter uses.
  - pi is a work in progress. This might mean you run into some bugs.

## Use

For use examples, check out `examples/vim-plugin`

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

### Templates

`pi` uses [mustache](https://mustache.github.io/) for templating, via the
[rustache](https://github.com/rustache/rustache) crate.

You can find examples and help on the [mustache page](https://mustache.github.io/).
