{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [ base ];
  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
	haskellPackages.hpack
	pkgs.stack
    pkgs.ghcid
    pkgs.tmux
	#haskellPackages.haskell-language-server.override { supportedGhcVersions = [ "8102" ]; }
	haskellPackages.haskell-language-server #TODO: pin ghc version.
  ];
  mttt = pkgs.haskellPackages.callPackage ./mttt.nix { };
in
pkgs.stdenv.mkDerivation {
  name = "mttt-dev";
  nativeBuildInputs = nixPackages;
  # shellHook = ''
  #   tmux new-session -d -s 'mttt-dev' nvim . \; \
  #   split-window -v \; \
  #   send-keys 'stack build; stack exec -- mttt' \; \
  #   split-window -h stack ghci\; \
  #   new-window ghcid\; \
  #   split-window -h stack haddock --file-watch\; \
  #   next-window \; \
  #   attach-session -t 'mttt-dev'
  #   exit
  # '';
}
