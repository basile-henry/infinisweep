{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs;
{
  infinisweep = import ./. {};
  infinisweep-static =
    haskell.lib.mkStaticExe
      (pkgsMusl.haskellPackages.callPackage ./infinisweep.nix {});
}
