fetchFromGitHub:
{ mkDerivation, ansi-wl-pprint, base, containers, data-fix, deepseq
, parsers, stdenv, tasty, tasty-hunit, tasty-th, text, transformers
, trifecta, unordered-containers
}:
mkDerivation {
  pname = "hnix";
  version = "0.3.0";
  src = fetchFromGitHub {
    owner = "jwiegley";
    repo = "hnix";
    rev = "b4eff58b8c346ea401ccc3e917af2fe4a1ed1f17";
    sha256 = "151mhq0ssn9lsxw2ldw9mpdmxiskjjqirdv91ljd7vam7fmkczb8";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-wl-pprint base containers data-fix deepseq parsers text
    transformers trifecta unordered-containers
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base containers data-fix deepseq
  ];
  testHaskellDepends = [
    base containers data-fix tasty tasty-hunit tasty-th text
  ];
  homepage = "http://github.com/jwiegley/hnix";
  description = "Haskell implementation of the Nix language";
  license = stdenv.lib.licenses.bsd3;
}
