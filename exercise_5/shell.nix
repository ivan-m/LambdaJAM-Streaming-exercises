{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cassava, stdenv, streaming_0_2_1_0
      , streaming-bytestring, streaming-cassava, streaming-with
      , transformers
      }:
      mkDerivation {
        pname = "exercise5";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring cassava streaming_0_2_1_0 streaming-bytestring
          streaming-cassava streaming-with transformers
        ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
