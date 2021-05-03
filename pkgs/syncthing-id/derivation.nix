{ fetchurl, stdenv, python38, makeWrapper, lib, openssl }:
let
  version = "b3a6f0093ddb1635b39279e9a539ca21";
in
stdenv.mkDerivation {
  inherit version;
  pname = "syncthing-id";
  src = fetchurl {
    url = "https://gist.githubusercontent.com/spectras/${version}/raw/81cadd3b9a12fb9ffc401416d672b8aebbfe8f6b/syncthing_id.py";
    sha256 = "0mkgi25m2xh18956409d7g2x0n89rn2xy6vfxqysgcdh3c6idiha";
  };
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    install -m755 -D $src $out/bin/syncthing-id
    wrapProgram $out/bin/syncthing-id \
      --prefix PATH : ${lib.makeBinPath [ openssl ]}
  '';
  buildInputs = [
    python38
    makeWrapper
  ];
}
