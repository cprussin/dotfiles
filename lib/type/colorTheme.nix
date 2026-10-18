{
  lib,
  pkgs,
  # More options for the theme, for a module system with more to theme.
  modules ? [],
}: let
  mkColorOption = color:
    lib.mkOption {
      description = "The color code for ${color}";
      type = lib.types.str;
    };

  # Every theme family comes in these variants, which are domicile's two.
  variantNames = ["dark" "light"];

  paletteOptions = {
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

  # Just the palette: `callPackage` adds `override` and the like.
  load = name:
    lib.getAttrs (builtins.attrNames paletteOptions) (
      pkgs.callPackage (../color-themes + "/${name}.nix") {}
    );

  palette = lib.types.submodule {
    options =
      paletteOptions
      // {
        name = lib.mkOption {
          description = "The variant's theme name, as `<family>/<variant>`.";
          type = lib.types.str;
        };
      };
  };
in
  lib.types.submoduleWith {
    shorthandOnlyDefinesConfig = true;
    modules =
      [
        ({config, ...}: {
          options =
            paletteOptions
            // {
              name = lib.mkOption {
                description = ''
                  The color theme for the UI, and the variant it starts in where
                  the desktop can switch between them.
                '';
                type = lib.types.nullOr (
                  lib.types.enum [
                    "solarized/dark"
                    "solarized/light"
                  ]
                );
                default = null;
              };

              variant = lib.mkOption {
                description = "The variant `name` picks, `dark` or `light`.";
                type = lib.types.enum variantNames;
                readOnly = true;
              };

              variants = lib.mkOption {
                description = ''
                  Every variant of `name`'s family, by variant: what follows the
                  desktop's light/dark preference picks between.
                '';
                type = lib.types.attrsOf palette;
                readOnly = true;
              };
            };

          # The startup variant is the colors as set, overrides and all; without
          # a `name`, they're both variants.
          config = lib.mkMerge [
            (lib.mkIf (config.name != null) (lib.mapAttrs (_: lib.mkDefault) (load config.name)))
            {
              variant =
                if config.name == null
                then "dark"
                else lib.last (lib.splitString "/" config.name);
              variants = lib.genAttrs variantNames (
                variant:
                  if config.name == null
                  then lib.getAttrs (builtins.attrNames paletteOptions) config // {name = "custom/${variant}";}
                  else if variant == config.variant
                  then lib.getAttrs (builtins.attrNames paletteOptions) config // {inherit (config) name;}
                  else let
                    name = "${builtins.head (lib.splitString "/" config.name)}/${variant}";
                  in
                    load name // {inherit name;}
              );
            }
          ];
        })
      ]
      ++ modules;
  }
