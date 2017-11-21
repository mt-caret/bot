let
  pkgs = import (import ./fetch-nixpkgs.nix) {};
in
  pkgs.haskell.packages.ghc802.callPackage ./bot.nix {}
