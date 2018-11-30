{ stdenv }:

stdenv.mkDerivation rec {
  name = "shakti";
  src = ./shakti;
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/shakti
  '';
}
