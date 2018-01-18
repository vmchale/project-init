% pi (1)
% Vanessa McHale

# NAME

pi - initialize projects from mustache templates

# SYNOPSIS

  pi new \<language\> \<directory\> [--force]

  pi init \<template\> \<directory\> [--force]

  pi git \<username\>/\<repo\> [--force]

# DESCRIPTION

**pi** is a command-line tool that helps you start new projects. It can generate
new projects either from mustache templates or from the builtin templates.

# OPTIONS

**-h**, **--help**
:   Display help

**-f**, **--force**
:   Initialize project even if the directory already exists

# CONFIGURATION

Configuration files are located in $HOME/.pi.toml and are configured using TOML.

**Keys available:**

  **license** - The preferred license for new projects. Currently supported
  licenses are BSD, BSD3, MIT, GPL, and AllRightsReserved.

  **version_control** - The preferred version control for new projects. The
  relevant executable must be on your PATH. Currently supported are git,
  mercurial, darcs, and pijul.

  **version** - String such as "0.1.0.0" or "0.1.0" representing your preferred
  versioning scheme.

  **author.name**

  **author.email**

  **author.github_username**

# USER TEMPLATES

Templates are configured using mustache. Templates placed in a
user's $HOME/.pi_templates will be available to her anywhere.

For sample templates, see:

  https://github.com/vmchale/pi-templates

# BUILTIN TEMPLATES

Project templates for Julia, Rust, Idris, Elm, Vimscript, Python, Haskell, Miso
are included by default. There is also a 'plain' template which just contains a
license and a readme.

# EXAMPLES

```
pi new idris permutations
```

```
pi init ruby blog
```

```
pi git vmchale/haskell-ats fast-package
```
