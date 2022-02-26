{
  lib,
  config,
  ...
}: let
  cfg = config.font;
  pangoFont = "${cfg.face} ${toString cfg.size}";
in {
  options.font = lib.mkOption {
    default = {};
    type = lib.types.submodule {
      options = {
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
          type = lib.types.float;
          description = "The font size.";
        };
      };
    };
  };

  config = lib.mkIf (cfg.package != null) {
    home.packages = [cfg.package];

    programs = {
      termite = {
        font = pangoFont;
        hintsFont = pangoFont;
      };
      kitty.font = {
        inherit (cfg) package;
        name = pangoFont;
      };
      swaylock.font = pangoFont;
      mako.font = pangoFont;
      zathura.options.font = pangoFont;
      waybar-custom.styles.common = {
        font-family = cfg.face;
        font-size = "${toString cfg.size}pt";
      };
      imv.overlayFont = {inherit (cfg) face size;};
      emacs.emacs-rc.font = {inherit (cfg) face size;};
    };

    wayland.windowManager.sway.config.fonts = {
      inherit (cfg) size;
      style = "Book";
      names = [cfg.face];
    };
  };
}
