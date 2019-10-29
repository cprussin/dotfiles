{ pkgs, config, ... }:

let
  background =
    builtins.replaceStrings [ "#" ] [ "x" ] config.colorTheme.background;
in

{
  home-manager.users.${config.primaryUserName}.systemd.user.services.tray = {
    Unit = {
      Description = "Trayer tray applet";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = ''
        ${pkgs.trayer}/bin/trayer \
          --widthtype pixel \
          --align right \
          --width 55 \
          --height 20 \
          --margin 5 \
          --distance 6 \
          --transparent true \
          --alpha 0 \
          --tint "0${background}" \
          --monitor primary
      '';
    };
  };
}
