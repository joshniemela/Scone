{
  description = "Haskell flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        name = "JexLexer";
        pkgs = import nixpkgs {inherit system;};
        haskellPackages = pkgs.haskell.packages.ghc90;
        cabalStuff = haskellPackages.callCabal2nix name ./. {};
      in rec {
        devShell = haskellPackages.shellFor {
          withHoogle = true;
          packages = p: [
            cabalStuff
          ];
          buildInputs = with haskellPackages; [
            cabal-install
          ];
        };

        defaultPackage = cabalStuff;
      }
    );
}
