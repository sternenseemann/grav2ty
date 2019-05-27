{ mkDerivation, base, containers, gloss, lens, linear, stdenv }:
mkDerivation {
  pname = "grav2ty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  libraryHaskellDepends = [ base containers lens linear ];
  executableHaskellDepends = [ base containers gloss linear ];
  doHaddock = true;
  license = stdenv.lib.licenses.gpl3;
}
