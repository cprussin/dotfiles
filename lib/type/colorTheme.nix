{
  lib,
  pkgs,
}: let
  mkColorOption = color:
    lib.mkOption {
      description = "The color code for ${color}";
      type = lib.types.str;
    };
in
  lib.types.submodule (
    {config, ...}: {
      options = {
        name = lib.mkOption {
          description = "The color theme for the UI.";
          type = lib.types.nullOr (
            lib.types.enum [
              "solarized/dark"
              "solarized/light"
            ]
          );
          default = null;
        };

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

      config = lib.mkIf (config.name != null) (
        let
          theme = pkgs.callPackage (../color-themes + "/${config.name}.nix") {};
        in {
          background = lib.mkDefault theme.background;
          foreground = lib.mkDefault theme.foreground;
          highlightBackground = lib.mkDefault theme.highlightBackground;
          highlightForeground = lib.mkDefault theme.highlightForeground;
          secondaryContent = lib.mkDefault theme.secondaryContent;
          selection = lib.mkDefault theme.selection;
          bright = lib.mkDefault theme.bright;
          urgent = lib.mkDefault theme.urgent;
          warn = lib.mkDefault theme.warn;
          black = lib.mkDefault theme.black;
          white = lib.mkDefault theme.white;
          grey = lib.mkDefault theme.grey;
          lightGrey = lib.mkDefault theme.lightGrey;
          red = lib.mkDefault theme.red;
          lightRed = lib.mkDefault theme.lightRed;
          green = lib.mkDefault theme.green;
          lightGreen = lib.mkDefault theme.lightGreen;
          yellow = lib.mkDefault theme.yellow;
          lightYellow = lib.mkDefault theme.lightYellow;
          blue = lib.mkDefault theme.blue;
          lightBlue = lib.mkDefault theme.lightBlue;
          purple = lib.mkDefault theme.purple;
          lightPurple = lib.mkDefault theme.lightPurple;
          cyan = lib.mkDefault theme.cyan;
          lightCyan = lib.mkDefault theme.lightCyan;
          dircolors = lib.mkDefault theme.dircolors;
        }
      );
    }
  )
