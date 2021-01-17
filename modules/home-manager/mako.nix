# TODO remove this when https://github.com/nix-community/home-manager/pull/1622
# lands
{ pkgs, config, lib, ... }:
let
  cfg = config.programs.mako;
in
{
  config = lib.mkIf cfg.enable {
    systemd.user.services.mako = {
      Unit = {
        Description = "Lightweight Wayland notification daemon";
        Documentation = [ "man:mako(1)" ];
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };

      Install.WantedBy = [ "graphical-session.target" ];

      Service = {
        Type = "dbus";
        BusName = "org.freedesktop.Notifications";
        ExecStart = "${pkgs.mako}/bin/mako";
        ExecReload = "${pkgs.mako}/bin/makoctl reload";
      };
    };
  };
}
