{ stdenv, src }:

stdenv.mkDerivation {
  inherit src;

  name = "dircolors-solarized";
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    for file in $src/dircolors.*
    do
      install -m644 -D $file $out/etc/dircolors/solarized/''${file##*/}
    done
  '';
}
