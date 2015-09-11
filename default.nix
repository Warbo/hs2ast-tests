{ mkDerivation, base, ghc, HS2AST, QuickCheck, stdenv }:
mkDerivation {
  pname = "hs2ast-test";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ghc HS2AST QuickCheck ];
  homepage = "http://chriswarbo.net/git/hs2ast-test";
  description = "Test helpers for HS2AST types";
  license = stdenv.lib.licenses.publicDomain;
}
