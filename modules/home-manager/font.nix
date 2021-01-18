{ lib, config, ... }:
let
  cfg = config.font;
  pangoFont = "${cfg.face} ${toString cfg.size}";
in
{
  options.font = {
    package = lib.mkOption {
      description = "The package that provides the font";
      default = null;
      type = lib.types.nullOr lib.types.package;
    };

    face = lib.mkOption {
      type = lib.types.str;
      description = "The font face name.";
    };

    size = lib.mkOption {
      type = lib.types.int;
      description = "The font size.";
    };
  };

  config = lib.mkIf (cfg.package != null) {
    home.packages = [ cfg.package ];

    programs = {
      termite = {
        font = pangoFont;
        hintsFont = pangoFont;
      };
      swaylock.font = pangoFont;
      mako.font = pangoFont;
      zathura.options.font = pangoFont;
      waybar-custom.styles.common = {
        font-family = cfg.face;
        font-size = "${toString cfg.size}pt";
      };
      imv.overlayFont = { inherit (cfg) face size; };
    };

    wayland.windowManager.sway.config.fonts = [ "${cfg.face} ${toString cfg.size}" ];
  };
}
