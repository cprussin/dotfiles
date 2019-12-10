{ stdenv }:

let
  sources = import ../../sources.nix;
in

stdenv.mkDerivation {
  name = "dircolors-solarized";
  src = sources.dircolors-solarized;
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    for file in $src/dircolors.*
    do
      install -m644 -D $file $out/etc/dircolors/solarized/''${file##*/}
        done
  '';
}
