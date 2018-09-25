{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:
(import ./default.nix { inherit nixpkgs compiler; }).env
