{ lib, config, ... }:

let
  mkColorOption = color: lib.mkOption {
    description = "The color code for ${color}";
    type = lib.types.str;
  };
in

{
  options.colorTheme = lib.mkOption {
    description = "The color theme for the UI.";
    type = lib.types.submodule {
      options = {
        background = mkColorOption "background";
        foreground = mkColorOption "foreground";
        highlightBackground = mkColorOption "highlighted background";
        highlightForeground = mkColorOption "highlighted foreground";
        secondaryContent = mkColorOption "secondary content foreground";
        selection = mkColorOption "selection";
        bright = mkColorOption "bright";
        urgent = mkColorOption "urgent";
        warn = mkColorOption "warn";
        black = mkColorOption "black";
        white = mkColorOption "white";
        grey = mkColorOption "grey";
        lightGrey = mkColorOption "light grey";
        red = mkColorOption "red";
        lightRed = mkColorOption "light red";
        green = mkColorOption "green";
        lightGreen = mkColorOption "light green";
        yellow = mkColorOption "yellow";
        lightYellow = mkColorOption "light yellow";
        blue = mkColorOption "blue";
        lightBlue = mkColorOption "light blue";
        purple = mkColorOption "purple";
        lightPurple = mkColorOption "light purple";
        cyan = mkColorOption "cyan";
        lightCyan = mkColorOption "light cyan";

        dircolors = lib.mkOption {
          description = "The dircolors file to use for the color theme.";
          type = lib.types.path;
        };
      };
    };
  };

  config.home-manager.users.${config.primaryUserName}.xresources.properties = {
    "*background" = config.colorTheme.background;
    "*foreground" = config.colorTheme.foreground;
    "*fadeColor" = config.colorTheme.background;
    "*cursorColor" = config.colorTheme.highlightForeground;
    "*pointerColorBackground" = config.colorTheme.highlightBackground;
    "*pointerColorForeground" = config.colorTheme.highlightForeground;
    "*color0" = config.colorTheme.black;
    "*color1" = config.colorTheme.red;
    "*color2" = config.colorTheme.green;
    "*color3" = config.colorTheme.yellow;
    "*color4" = config.colorTheme.blue;
    "*color5" = config.colorTheme.purple;
    "*color6" = config.colorTheme.cyan;
    "*color7" = config.colorTheme.lightGrey;
    "*color8" = config.colorTheme.grey;
    "*color9" = config.colorTheme.lightRed;
    "*color10" = config.colorTheme.lightGreen;
    "*color11" = config.colorTheme.lightYellow;
    "*color12" = config.colorTheme.lightBlue;
    "*color13" = config.colorTheme.lightPurple;
    "*color14" = config.colorTheme.lightCyan;
    "*color15" = config.colorTheme.white;
  };
}
