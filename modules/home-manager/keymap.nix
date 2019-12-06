{ lib, ... }:

{
  options.keymap = {
    layout = lib.mkOption {
      description = "Keymap";
      type = lib.types.str;
    };

    variant = lib.mkOption {
      description = "Keymap variant";
      type = lib.types.str;
    };

    options = lib.mkOption {
      description = "Options to apply to the keymap";
      type = lib.types.str;
    };
  };
}
