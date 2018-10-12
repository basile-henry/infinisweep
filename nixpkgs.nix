let
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src     = builtins.fetchTarball { inherit (nixpkgs) url sha256; };
in import src
