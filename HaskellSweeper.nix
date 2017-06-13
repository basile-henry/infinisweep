{ mkDerivation, base, containers, ncurses, random, stdenv, strict
}:
mkDerivation {
  pname = "HaskellSweeper";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers ncurses random strict
  ];
  license = stdenv.lib.licenses.free;
}
