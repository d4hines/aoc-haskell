{
  description = "advent-of-code";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        overlays = [
          haskellNix.overlay
          (
            final: prev: {
              advent-of-code = {
                project =
                  prev.haskell-nix.project'
                  {
                    name = "advent-of-code";
                    src = ./.;
                    compiler-nix-name = "ghc982";
                    shell = {
                      withHoogle = false;
                      tools = {
                        cabal = {};
                        hlint = {};
                        haskell-language-server = {};
                        fourmolu = {};
                      };
                    };
                  };
              };
            }
          )
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        advent-of-code-project = pkgs.advent-of-code.project;
        inherit (advent-of-code-project.hsPkgs) advent-of-code;
      in {
        packages =
          advent-of-code.components.exes
          // {
            inherit (pkgs.advent-of-code.site) site;
            default = advent-of-code.components.exes.aoc2024;
          };
        devShells.default = advent-of-code-project.shell;
      }
    );
}
