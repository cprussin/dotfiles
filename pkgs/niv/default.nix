{ symlinkJoin, makeWrapper }:

let
  sources = import ../../sources.nix;
in

symlinkJoin {
  name = "niv";
  paths = [ sources.niv ];
  buildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/niv \
      --add-flags "--sources-file ${toString ../../sources.json}"
  '';
}
