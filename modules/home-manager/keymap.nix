{ pkgs, lib, config, ... }:
let
  keymapType = pkgs.callPackage ../../lib/type/keymap.nix { };
  cfg = config.keymap;
in
{
  options.keymap = lib.mkOption {
    type = lib.types.nullOr keymapType;
    default = null;
  };

  config = lib.mkIf (cfg != null) {
    wayland.windowManager.sway.config.input."*" = {
      xkb_layout = cfg.layout;
      xkb_variant = cfg.variant;
      xkb_options = cfg.options;
    };
  };
}
