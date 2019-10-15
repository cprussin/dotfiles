{ pkgs, lib, config, ... }:

let
  fontLib = pkgs.callPackage ../../../../lib/fonts.nix {};
  fontOptions = {
    options = {
      face = lib.mkOption {
        type = lib.types.str;
        description = "The font name.";
      };

      size = lib.mkOption {
        type = lib.types.int;
        description = "The font size.";
      };
    };
  };
in

{
  options.primaryFont = lib.mkOption {
    description = "The primary font for the majority of the UI.";
    type = lib.types.submodule fontOptions;
  };

  config.xresources.properties."*.font" = fontLib.xftFont config.primaryFont;
}
