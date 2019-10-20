{ pkgs, lib, config, ... }:

let
  fontLib = pkgs.callPackage ../../../../lib/fonts.nix {};
in

{
  options.primaryFont = lib.mkOption {
    description = "The primary font for the majority of the UI.";
    type = lib.types.submodule {
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
  };

  config.home-manager.users.${config.primaryUserName} = { ... }: {
    config.xresources.properties."*.font" = fontLib.xftFont config.primaryFont;
  };
}
