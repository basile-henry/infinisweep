{ nixpkgs ? import ./nixpkgs.nix {}, compiler ? "ghc864" }:

with nixpkgs.pkgs;

haskell.lib.failOnAllWarnings (haskell.packages.${compiler}.callCabal2nix "infinisweep" ./. {})
