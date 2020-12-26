{ mkDerivation, async, attoparsec, base, bytestring, clock
, containers, flat, lens, linear, network, sdl2, sdl2-gfx, stdenv
, stm, tasty, tasty-quickcheck, time, transformers
}:
mkDerivation {
  pname = "grav2ty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base bytestring clock containers flat lens linear
    network stm transformers
  ];
  executableHaskellDepends = [
    async base clock containers lens linear network sdl2 sdl2-gfx stm
    time transformers
  ];
  testHaskellDepends = [ base tasty tasty-quickcheck ];
  doHaddock = false;
  description = "a 2d space (ship) game with realistic physics-based gameplay";
  license = stdenv.lib.licenses.gpl3;
}
