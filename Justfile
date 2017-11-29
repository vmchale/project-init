clean:
    rm -f tags
    sn c .

name:
    github-release edit -s $(cat .git-token) -u vmchale -r project-init -n "$(madlang run ~/programming/madlang/releases/releases.mad)" -t "$(grep -P -o '\d+\.\d+\.\d+' Cargo.toml | head -n1)"

test:
    tomlcheck --file Cargo.toml
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
    cargo run -- new mad story
    cd story && madlang run src/story.mad
    rm -rf story
    cargo run -- new ats sample
    cd sample && ./shake.hs run
    rm -rf sample

manpages:
    pandoc MANPAGE.md -s -t man -o pi.1
    mkdir -p ~/.local/share/man/man1/
    cp pi.1 ~/.local/share/man/man1/
    mandb >> /dev/null
    rm pi.1

check:
    git diff master origin/master

patch:
    @rm -f tags
    cargo release -l patch --no-dev-version
