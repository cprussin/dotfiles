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
  };

  config = lib.mkIf cfg.enable {
    home.file.".emacs.d/init.el".text = ''
      (setq emacs-rc-browse-path "${cfg.browse}")
      (require 'emacs-rc)
    '';
  };
}
