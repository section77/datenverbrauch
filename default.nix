{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, HStringTemplate, lens
      , optparse-applicative, stdenv, text, time, transformers, wreq, xml
      }:
      mkDerivation {
        pname = "datenverbrauch";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring HStringTemplate lens optparse-applicative text time
          transformers wreq xml
        ];
        homepage = "http://github.com/section77/datenverbrauch#readme";
        description = "Simple project template from stack";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
