{
  config,
  lib,
  ...
}: let
  cfg = config.programs.emacs.emacs-rc;
in {
  options.programs.emacs.emacs-rc = {
    enable = lib.mkEnableOption "emacs-rc";

    browse = lib.mkOption {
      type = lib.types.path;
    };

    colorThemeLink = lib.mkOption {
      description = ''
        The color theme's link to the variant the desktop shows, which emacs
        follows.  `null` stays dark.
      '';
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
  };

  config = lib.mkIf cfg.enable {
    home.file.".emacs.d/init.el".text = ''
      (setq emacs-rc-browse-path "${cfg.browse}")
      ${lib.optionalString (cfg.colorThemeLink != null) ''(setq emacs-rc-color-theme-link "${cfg.colorThemeLink}")''}
      (require 'emacs-rc)
    '';
  };
}
