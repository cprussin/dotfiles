{ stdenv, lib, callPackage, emacs, config }:

let
  installLib = callPackage ../../../lib/install.nix {};
  scripts = callPackage ./scripts { inherit emacs config; };
  apps = callPackage ./apps { inherit emacs config; };
in

stdenv.mkDerivation {
  name = "launcher";

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = lib.concatStringsSep "\n" [
    (installLib.installAll scripts "bin")
    (installLib.installAll apps "share/apps")
  ];
}
