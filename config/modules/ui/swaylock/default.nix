{ pkgs, config, ... }:

let
  fontLib = pkgs.callPackage ../../../../lib/fonts.nix {};
  swaylockColor = builtins.replaceStrings [ "#" ] [ "" ];
  font = fontLib.pangoFont config.fontTheme.primaryFont;
  background = swaylockColor config.colorTheme.background;
  highlightBackground = swaylockColor config.colorTheme.highlightBackground;
  urgent = swaylockColor config.colorTheme.urgent;
  warn = swaylockColor config.colorTheme.warn;
  bright = swaylockColor config.colorTheme.bright;
  selection = swaylockColor config.colorTheme.selection;
in

{
  primary-user.home-manager.programs.swaylock = {
    inherit font;

    enable = true;
    daemonize = true;
    image = ../sway/background.png;
    color = background;

    text-caps-lock-color = background;
    text-ver-color = background;
    text-wrong-color = background;

    bs-hl-color = warn;
    caps-lock-bs-hl-color = urgent;
    caps-lock-key-hl-color = selection;

    inside-color = "00000000";
    inside-clear-color = warn + "D0";
    inside-caps-lock-color = urgent + "D0";
    inside-ver-color = selection + "D0";
    inside-wrong-color = urgent + "D0";

    ring-color = bright;
    ring-clear-color = warn;
    ring-caps-lock-color = urgent;
    ring-ver-color = selection;
    ring-wrong-color = urgent;

    line-color = highlightBackground;
    line-clear-color = highlightBackground;
    line-caps-lock-color = highlightBackground;
    line-ver-color = highlightBackground;
    line-wrong-color = highlightBackground;

    separator-color = highlightBackground;

    indicator-radius = 200;
    indicator-thickness = 50;
  };
}
