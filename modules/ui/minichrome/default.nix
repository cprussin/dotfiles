{ pkgs, config, ... }:

{
  nixpkgs.overlays = [
    (
      _: super: {
        minichrome = super.callPackage ./minichrome.nix {};
      }
    )
  ];

  home-manager.users.${config.primaryUserName} = { ... }: {
    systemd.user.services.minichrome = {
      Unit = {
        Description = "minichrome daemon";
        After = [ "graphical-session-pre.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${pkgs.minichrome}/bin/minichrome";
      };
    };
  };
}
