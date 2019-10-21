{ lib }:

let
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

{
  inherit install installAll;
}
