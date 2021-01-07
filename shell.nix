{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [ base ];
  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
	pkgs.stack
    pkgs.gdb
    pkgs.ghcid
	#haskellPackages.haskell-language-server.override { supportedGhcVersions = [ "8102" ]; }
	haskellPackages.haskell-language-server #TODO: pin ghc version.
  ];
  mttt = pkgs.haskellPackages.callPackage ./mttt.nix { };
in
pkgs.stdenv.mkDerivation {
  name = "mttt-env";
  nativeBuildInputs = nixPackages;
  # buildInputs = [ mttt ];
}
