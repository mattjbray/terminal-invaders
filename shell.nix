{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          lens random vty
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "haskell-terminal-invaders-0";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
