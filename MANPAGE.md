% pi (1)
% Vanessa McHale

# NAME

pi - initialize projects from mustache templates

# SYNOPSIS

  pi new language directory [--force]

  pi init template directory [--force]

# DESCRIPTION

**pi** is a command-line tool that helps you start new projects. It can generate
new projects either from mustache templates or from the builtin templates for
Julia, Rust, Haskell, Idris, Elm, Vimscript, and Python.

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

Templates are configured using the mustache language. Templates placed in a
user's $HOME/.pi_templates will be available to her anywhere.

For sample templates, see:

  https://github.com/vmchale/pi-templates

# EXAMPLES

pi new idris permutations

  This would initialize a new Idris project in a directory called 'permutations'

pi init ruby blog

  This would initialize a new project in the directory 'blog', based on the
  template 'ruby'. pi will search for the template first in the current working
  directory and then in $HOME/.pi_templates
