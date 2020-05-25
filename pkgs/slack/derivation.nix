{ lib, slack, xdg_utils, makeWrapper }:

slack.overrideAttrs (
  oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [ makeWrapper ];
    postInstall = ''
      ${oldAttrs.postInstall or ""}
      wrapProgram $out/bin/slack --prefix PATH : ${lib.makeBinPath [ xdg_utils ]}
    '';
  }
)
