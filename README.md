# project init (pi)

[![Build Status](https://travis-ci.org/vmchale/project-init.svg?branch=master)](https://travis-ci.org/vmchale/project-init)

This is intended to provide something similar to
[cookiecutter](https://github.com/audreyr/cookiecutter), albeit faster. It is
somewhat more limited that cookiecutter, but for most users the difference will
be imperceptible (though it *will* be noticeably faster!).

Reasons to use pi:
  - Templates are smaller. Define files you need in a `.toml` rather than an
    entire directory tree.
  - *Fast*. pi is much faster than cookiecutter, even when used locally.
  - pi uses mustache, a logic-less language that has libraries for *many* other
    languages. That means that you can manipulate your pi templates in other
    languages, if you'd like.
Reasons not to use pi:
  - pi does not (currently) fetch templates remotely.
  - pi uses logic-less templates
  - pi does not allow custom keys to be set in config files. This can be
    inconvenient.

## Configuration

Configuration is via the `~/.pi.toml` file. The following is an example:

```toml
[author]
name = "Vanessa McHale"
email = "tmchale@wisc.edu"
github_username = "vmchale"

[version]
version = "0.1.0"
version-control = "git"
```

This will make the variable `name` available in the mustache templates (more
on that later), and it will initialize a git repository 

## Templates

`pi` uses [mustache](https://mustache.github.io/) for templating, via the
[rustache](https://github.com/rustache/rustache) crate.

You can find examples and help on the [mustache page](https://mustache.github.io/).
