{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

with nixpkgs;
{
  infinisweep = import ./. {};
  infinisweep-static =
    haskell.lib.mkStaticExe
      (pkgsMusl.haskellPackages.callPackage ./infinisweep.nix {});
  infinisweep-static-aarch64 =
    haskell.lib.mkStaticExe
      (pkgsCross.aarch64-multiplatform-musl.haskellPackages.callPackage ./infinisweep.nix {});
}
