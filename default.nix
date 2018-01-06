{ mkDerivation, base, containers, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text, transformers
}:
mkDerivation {
  pname = "ctfp";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers text transformers ];
  testHaskellDepends = [
    base containers tasty tasty-hunit tasty-quickcheck text
    transformers
  ];
  description = "Solutions to problems in /Category Theory for Programmers/";
  license = stdenv.lib.licenses.bsd3;
}
