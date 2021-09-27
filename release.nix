let
  pkgs = import <nixpkgs> { };

in
  { acinetobase-static = pkgs.haskellPackages.callPackage ./acinetobase-static.nix { };
  }
