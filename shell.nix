let
  pkgs = import <nixpkgs> { };
  build = import ./release.nix;
in
build.acinetobase-static.env.overrideAttrs (old: rec {
  buildInputs = old.buildInputs ++ [
    pkgs.haskell-language-server
    pkgs.stylish-haskell
    pkgs.hlint
  ];
})
