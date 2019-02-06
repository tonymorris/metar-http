{ mkDerivation, base, checkers, http-types, lens, metar
, network-uri, QuickCheck, semigroupoids, semigroups, stdenv, tasty
, tasty-hunit, tasty-quickcheck, text, transformers, utf8-string
, wai, warp
}:
mkDerivation {
  pname = "metar-http";
  version = "0.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base http-types lens metar network-uri semigroupoids semigroups
    text transformers utf8-string wai warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base checkers lens QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  homepage = "https://gitlab.com/tonymorris/metar-http";
  description = "HTTP for METAR";
  license = stdenv.lib.licenses.bsd3;
}
