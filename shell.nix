{ nixpkgs ? import ./nix/nixpkgs.nix {} }:

nixpkgs.mkShell {
  name = "infinisweep-shell";
  packages = [
    nixpkgs.niv
  ];
  inputsFrom = [
    (import ./default.nix { inherit nixpkgs; })
  ];
}
