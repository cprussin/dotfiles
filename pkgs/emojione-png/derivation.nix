{ stdenv }:
let
  pname = "emojione";
  version = "2.2.6-22";
in
stdenv.mkDerivation {
  inherit pname version;

  src = fetchTarball {
    url = "https://raw.githubusercontent.com/iqbalansari/emacs-emojify/9e36d0e8c2a9c373a39728f837a507adfbb7b931/emojione-fixed-v${version}.tar";
    sha256 = "1jfpc9q40gd5gr6mxj4m6z7vh3j7x7agb1bx8hhknffxc8ps5lr1";
  };

  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
  installPhase = ''
    install -m644 -D -t $out $src/*
  '';
}
