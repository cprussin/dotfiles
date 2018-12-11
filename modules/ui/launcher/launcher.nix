{ stdenv, lib, callPackage, emacs, secrets }:

let
  scripts = callPackage ./scripts { inherit emacs; };

  apps = callPackage ./apps { inherit emacs secrets; };

  install = location: name: script:
    if name == "override" || name == "overrideDerivation"
      then ""
      else ''
        install -m755 -D ${script.outPath} $out/${location}/${name}
        substituteInPlace $out/${location}/${name} --subst-var out
      '';

  installAll = set: location:
    lib.concatStringsSep "\n" (lib.mapAttrsToList (install location) set);
in

stdenv.mkDerivation {
  name = "launcher";

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = lib.concatStringsSep "\n" [
    (installAll scripts "bin")
    (installAll apps "share/apps")
  ];
}
