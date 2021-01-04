{ mkDerivation, array, base, gloss, hpack, parseargs, stdenv }:
mkDerivation {
  pname = "mttt";
  version = "0.3.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base gloss ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ array base gloss parseargs ];
  testHaskellDepends = [ array base gloss ];
  jailbreak = true;
  prePatch = "hpack";
  license = stdenv.lib.licenses.gpl3;
}
