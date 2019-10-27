{ mkDerivation, aeson, base, bytestring, containers, flat, gloss
, lens, linear, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "grav2ty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers flat lens linear
  ];
  executableHaskellDepends = [ base containers gloss lens linear ];
  testHaskellDepends = [ base tasty tasty-quickcheck ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  doHaddock = true;
  description = "a 2d space (ship) game with realistic physics-based gameplay";
  license = stdenv.lib.licenses.gpl3;
}
