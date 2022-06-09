{ mkDerivation, lib, base, hashable, optparse-applicative, random, strict, vty
}:
mkDerivation {
  pname = "infinisweep";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base random ];
  executableHaskellDepends = [
    base hashable optparse-applicative random strict vty
  ];
  license = lib.licenses.mit;
}
