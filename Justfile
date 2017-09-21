test:
    rm -rf project/
    cargo run -- new miso project
    cd project/ && ./shake.hs
    rm -rf project/
    cargo run -- new haskell project
    cd project/ && stack build --test --bench && hlint .
    rm -rf project/
    cargo run -- new idris project
    cd project/ && idris --testpkg test.ipkg
    rm -rf project/
    cargo run -- new elm project
    cd project/ && elm-make --yes src/main.elm
    rm -rf project/
    cargo run -- new rust project
    cd project/ && cargo build
    rm -rf project/
    cargo run -- new reco project
    cd project && reco check
    rm -rf project

manpages:
    pandoc MANPAGE.md -s -t man -o pi.1
    mkdir -p ~/.local/share/man/man1/
    cp pi.1 ~/.local/share/man/man1/
    mandb >> /dev/null
    rm pi.1

check:
    git diff master origin/master

patch:
    cargo release -l patch --no-dev-version
