{ lib, symlinkJoin, slack, xdg_utils, makeWrapper }:

symlinkJoin {
  name = "slack";
  paths = [ slack ];
  buildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram $out/bin/slack --prefix PATH : ${lib.makeBinPath [ xdg_utils ]}
  '';
}
