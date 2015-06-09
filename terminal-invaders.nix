{ mkDerivation, base, data-default, lens, mtl, random, stdenv, transformers, vty
}:
mkDerivation {
  pname = "terminal-invaders";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base data-default lens mtl random transformers vty ];
  homepage = "github.com/mattjbray/terminal-invaders";
  license = stdenv.lib.licenses.unfree;
}
