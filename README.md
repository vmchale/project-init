# project init (pi)

This is intended to provide something similar to
[cookiecutter](https://github.com/audreyr/cookiecutter), albeit faster. It is
somewhat more limited that cookiecutter, but for most users the difference will
be imperceptible.

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
