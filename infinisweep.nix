{ mkDerivation, lib, base, hashable, optparse-applicative, random, strict, vty, gitignoreSource
}:
mkDerivation {
  pname = "infinisweep";
  version = "1.0.0";
  src = gitignoreSource ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base random ];
  executableHaskellDepends = [
    base hashable optparse-applicative random strict vty
  ];
  license = lib.licenses.mit;
}
