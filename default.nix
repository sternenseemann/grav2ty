let pkgs = import <nixpkgs> {};
    hp = pkgs.haskellPackages.override {
      overrides = self: super: {
        #mkDerivation = args: super.mkDerivation (args // {
        #  enableLibraryProfiling = true;
        #});

        flat = pkgs.haskell.lib.markUnbroken super.flat;
      };
    };
    drv = hp.callPackage ./grav2ty.nix { };
in if pkgs.lib.inNixShell then drv.env else drv
