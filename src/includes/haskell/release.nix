let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskell.lib.justStaticExecutables (pkgs.haskellPackages.callPackage ./default.nix { })
