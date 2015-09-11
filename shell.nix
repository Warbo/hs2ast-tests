{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghc, HS2AST, QuickCheck, stdenv }:
      mkDerivation {
        pname = "hs2ast-test";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base ghc HS2AST QuickCheck ];
        homepage = "http://chriswarbo.net/git/hs2ast-test";
        description = "Test helpers for HS2AST types";
        license = stdenv.lib.licenses.publicDomain;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
