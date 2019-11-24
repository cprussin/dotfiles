{ stdenv, lib, fetchFromGitHub, makeWrapper, glib }:

stdenv.mkDerivation {
  name = "notify-send";
  src = fetchFromGitHub {
    owner = "vlevit";
    repo = "notify-send.sh";
    rev = "4b90bb968dbe7364ec9526e76d349950627e967f";
    sha256 = "04vj1spymn62ncpxgry1w5arlxvnhr2v77c8zq7f4k8814lpvzas";
  };
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
