{ lib, ... }:

let
  fontType = {
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
  options.fontTheme = lib.mkOption {
    description = "The font theme for the UI.";
    type = lib.types.submodule {
      options = {
        primaryFont = lib.mkOption {
          description = "The primary font for the majority of the UI.";
          type = lib.types.submodule fontType;
        };
      };
    };
  };
}
