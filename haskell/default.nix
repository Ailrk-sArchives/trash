{ mkDerivation, accelerate, ansi-terminal, array, async, base
, boxes, bytestring, comonad, conduit, conduit-extra, containers
, criterion, data-default, deepseq, gloss, hspec, http-conduit
, lens, lib, linear, mmap, monad-control, mtl, parallel, parsec
, pretty, process, QuickCheck, random, reflection, repa, singletons
, stm, template-haskell, text, time, transformers
, unordered-containers, utf8-string, vector, void, zlib
}:
mkDerivation {
  pname = "haskellwikibook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    accelerate ansi-terminal array async base boxes bytestring comonad
    conduit conduit-extra containers criterion data-default deepseq
    gloss hspec http-conduit lens linear mmap monad-control mtl
    parallel parsec pretty process QuickCheck random reflection repa
    singletons stm template-haskell text time transformers
    unordered-containers utf8-string vector void zlib
  ];
  executableHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base mtl ];
  homepage = "https://github.com/githubuser/haskellwikibook#readme";
  license = lib.licenses.bsd3;
}
