{ stdenv, lib, callPackage, emacs, secrets, terminal }:

let
  installLib = callPackage ../../../lib/install.nix {};
  scripts = callPackage ./scripts { inherit emacs; };
  apps = callPackage ./apps { inherit emacs secrets terminal; };
in

stdenv.mkDerivation {
  name = "launcher";

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = lib.concatStringsSep "\n" [
    (installLib.installAll scripts "bin")
    (installLib.installAll apps "share/apps")
  ];
}
