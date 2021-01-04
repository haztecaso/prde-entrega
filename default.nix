let
  pkgs = import <nixpkgs> { };

in
  {
    mttt = pkgs.haskellPackages.callPackage ./mttt.nix { };
  }
