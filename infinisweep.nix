{ mkDerivation, lib, base, hashable, ncurses, optparse-applicative, random, strict
}:
mkDerivation {
  pname = "infinisweep";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base random ];
  executableHaskellDepends = [
    base hashable ncurses optparse-applicative random strict
  ];
  license = lib.licenses.mit;
}
