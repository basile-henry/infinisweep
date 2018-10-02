{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:

with nixpkgs.pkgs;

haskell.lib.failOnAllWarnings (haskell.packages.${compiler}.callCabal2nix "infinisweep" ./. {})
