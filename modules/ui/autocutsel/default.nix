{ pkgs, config, ... }:

{
  home-manager.users.${config.primaryUserName} = { ... }:
    let
      mkUnit = selection: {
        Unit = {
          Description = "autocutsel daemon for tracking ${selection}";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Install = {
          WantedBy = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${pkgs.autocutsel}/bin/autocutsel -s ${selection}";
        };
      };
    in
      {
        systemd.user.services = {
          autocutsel-clipboard = mkUnit "CLIPBOARD";
          autocutsel-primary = mkUnit "PRIMARY";
        };
      };
}
