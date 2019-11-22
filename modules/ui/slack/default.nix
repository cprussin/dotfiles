{ ... }:

{
  nixpkgs.overlays = [
    (
      self: super: {
        slack = self.symlinkJoin {
          name = "slack";
          paths = [ super.slack ];
          buildInputs = [ self.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/slack \
              --prefix PATH : ${self.lib.makeBinPath [ self.xdg_utils ]}
          '';
        };
      }
    )
  ];
}
