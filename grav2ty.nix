{ mkDerivation, base, containers, gloss, lens, linear, stdenv }:
mkDerivation {
  pname = "grav2ty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base containers lens linear ];
  executableHaskellDepends = [ base containers gloss linear ];
  doHaddock = false;
  license = stdenv.lib.licenses.gpl3;
}
