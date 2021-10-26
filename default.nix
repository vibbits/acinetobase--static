let
  pkgs = import <nixpkgs> { };

  haskellPackages = pkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      "acinetobase-static" =
        hself.callCabal2nix "acinetobase-static" ./. { };
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
      pkgs.stylish-haskell
      pkgs.hlint

      # Purescript development
      pkgs.purescript
      pkgs.spago
      pkgs.nodePackages.pscid
      pkgs.nodePackages.parcel-bundler
      pkgs.nodePackages.purty
      pkgs.nodePackages.purescript-language-server

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
