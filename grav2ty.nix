{ mkDerivation, base, containers, gloss, lens, linear, stdenv
, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "grav2ty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers lens linear ];
  executableHaskellDepends = [ base containers gloss lens linear ];
  testHaskellDepends = [ base tasty tasty-quickcheck ];
  doHaddock = true;
  doCheck = true;
  description = "a 2d space (ship) game with realistic physics-based gameplay";
  license = stdenv.lib.licenses.gpl3;
}
