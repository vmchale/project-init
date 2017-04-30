# project init (pi)

This is intended to provide something similar to
[cookiecutter](https://github.com/audreyr/cookiecutter), albeit faster. It is of
course somewhat limited in scope in its initial release

## Configuration

Configuration is via the `~/.pi.toml` file. The following is an example:

```toml
[author]
name = "Vanessa McHale"
email = "tmchale@wisc.edu"

[license]
license = "BDS3"

[version]
version = "0.1.0"
version-control = "git"
```

## Templates

`pi` uses [mustache](https://mustache.github.io/) for templating, via the
[rustache](https://github.com/rustache/rustache) crate. 
