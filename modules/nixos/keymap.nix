{ pkgs, config, lib, ... }:

let
  cfg = config.keymap;
in

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

  config = lib.mkIf (cfg.layout != null) {
    i18n.consoleKeyMap = pkgs.runCommand "console-keymap" {} ''
      '${pkgs.ckbcomp}/bin/ckbcomp' \
        -layout '${cfg.layout}' \
        -option '${cfg.options}' \
        -variant '${cfg.variant}' > "$out"
    '';
  };
}
