{ mkDerivation, base, stdenv, text }:
mkDerivation {
  pname = "ctfp";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base text ];
  description = "Solutions to problems in /Category Theory for Programmers/";
  license = stdenv.lib.licenses.bsd3;
}
