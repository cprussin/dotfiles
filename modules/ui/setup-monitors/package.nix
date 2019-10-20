{ stdenv, lib, callPackage }:

let
  installLib = callPackage ../../../lib/install.nix {};
  scripts = {
    setup-monitors = callPackage ./setup-monitors.nix {};
    watch-monitor-change = callPackage ./watch-monitor-change.nix {};
  };
in

stdenv.mkDerivation {
  name = "launcher";

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = installLib.installAll scripts "bin";
}
