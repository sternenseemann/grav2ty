{ mkDerivation, base, gloss, lens, linear, stdenv }:
mkDerivation {
  pname = "grav2ty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base gloss lens linear ];
  license = stdenv.lib.licenses.gpl3;
}
