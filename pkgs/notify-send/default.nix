{ stdenv, lib, makeWrapper, glib }:

let
  sources = import ../../niv-sources.nix;
in

stdenv.mkDerivation {
  name = "notify-send";
  src = sources.notify-send;
  buildInputs = [ makeWrapper ];
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    for file in $src/*.sh
    do
      install -m755 -D $file $out/lib/notify-send/''${file##*/}
    done
    mkdir $out/bin
    ln -s $out/lib/notify-send/notify-send.sh $out/bin/notify-send
    wrapProgram $out/bin/notify-send \
      --prefix PATH : ${lib.makeBinPath [ glib ]}
  '';
}
