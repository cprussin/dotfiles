{ pkgs, config, ... }:

let
  fontLib = pkgs.callPackage ../../../../lib/fonts.nix {};
  font = {
    face = config.fontTheme.primaryFont.face;
    size = 1.1 * config.fontTheme.primaryFont.size;
  };
in

{
  primary-user.home-manager.programs.mako = {
    enable = true;
    font = fontLib.pangoFont font;
    background-color = "${config.colorTheme.foreground}E0";
    text-color = config.colorTheme.background;
    border-size = 3;
    border-color = config.colorTheme.highlightForeground;
    padding = 20;
    margin = 30;
    width = 500;
    height = 600;
    progress-color = "source ${config.colorTheme.bright}";
    default-timeout = 10000;
    icon-path = [
      "${config.iconTheme.package}/share/icons/${config.iconTheme.name}"
    ];
  };
}
