let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./grav2ty.nix { }
