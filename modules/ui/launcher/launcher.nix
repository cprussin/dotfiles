{ stdenv, lib, callPackage, config }:

let
  stripOverrides = callPackage ../../../lib/stripOverrides.nix {};
  installLib = callPackage ../../../lib/install.nix {};

  scripts = stripOverrides (callPackage ./scripts { inherit config; });
  apps = stripOverrides (callPackage ./apps { inherit config; });
in

stdenv.mkDerivation {
  name = "launcher";

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = lib.concatStringsSep "\n" [
    (installLib.installAll scripts "bin")
    (installLib.installAll apps "share/apps")
  ];
}
