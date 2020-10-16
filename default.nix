{ nixpkgs ? import ./nixpkgs.nix {} }:
nixpkgs.pkgs.haskellPackages.callPackage ./infinisweep.nix {}
