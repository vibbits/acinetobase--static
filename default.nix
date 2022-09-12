let
  pkgs = import <nixpkgs> { };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      acinetobase-static =
        hself.callCabal2nix "acinetobase-static" ./. { };

      slick =
        hself.callPackage ./slick.nix { };
    };
  };

  shell = haskellPackages.shellFor {
    packages = p: [
      p."acinetobase-static"
    ];
    buildInputs = [
      # Haskell development
      pkgs.haskell-language-server
      pkgs.cabal-install
      pkgs.hlint

      # Purescript development
      pkgs.purescript
      pkgs.spago
      pkgs.nodePackages.pscid
      pkgs.nodePackages.purty

      # Node
      pkgs.nodePackages.npm
    ];
    withHoogle = true;
  };
in
{
  inherit shell;
  inherit haskellPackages;
  "acinetobase-static" = haskellPackages."acinetobase-static";
}
