{ stdenv, lib, callPackage }:

let
  installLib = callPackage ../../../lib/install.nix {};
  scripts = {
    get-aws-field = callPackage ./get-aws-field.nix {};
    get-aws-access-key = callPackage ./get-aws-access-key.nix {};
    get-aws-access-key-nixops = callPackage ./get-aws-access-key-nixops.nix {};
  };
in

stdenv.mkDerivation {
  name = "get-aws-access-key";

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = lib.concatStringsSep "\n" [
    (installLib.installAll scripts "bin")
  ];
}
