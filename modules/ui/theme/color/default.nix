{ lib, ... }:

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
}
