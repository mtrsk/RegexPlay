{ nixpkgs ? import <nixpkgs> {}
, ghc ? nixpkgs.ghc
}:

with nixpkgs;

haskell.lib.buildStackProject {
  inherit ghc;

  name = "haskell-env";

  buildInputs = [
    zlib
    hlint
    haskellPackages.hindent
    haskellPackages.ghcid
    haskellPackages.stylish-haskell
  ];

  shellHooks = ''
    alias stack="\stack --nix"
  '';
}
