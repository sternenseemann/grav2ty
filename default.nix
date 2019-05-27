let pkgs = import <nixpkgs> {};
    profiled = pkgs.haskellPackages.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          enableLibraryProfiling = true;
        });
      };
    };
in profiled.callPackage ./grav2ty.nix { }
