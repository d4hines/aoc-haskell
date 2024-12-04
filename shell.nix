{ pkgs ? import <nixpkgs> {} }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
  packageSet = pkgs.haskell.packages.ghc98;
  myPkg = packageSet.callCabal2nix "aoc23" src {};
in
pkgs.mkShell {
  name = "aoc-shell";
  inputsFrom  = [
    myPkg
  ];

  packages = [
    packageSet.haskell-language-server
    packageSet.cabal-install
    packageSet.hlint
  ];
}
