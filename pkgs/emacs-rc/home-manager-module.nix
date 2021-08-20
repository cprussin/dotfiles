{ config, lib, pkgs, ... }:
let
  cfg = config.programs.emacs.emacs-rc;

  emojiSets = pkgs.linkFarm "emoji-sets" [{
    name = "emojione";
    path = pkgs.emojione-png;
  }];
in
{
  options.programs.emacs.emacs-rc = {
    enable = lib.mkEnableOption "emacs-rc";

    git = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.git}/bin/git";
    };

    rg = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.ripgrep}/bin/rg";
    };

    browse = lib.mkOption {
      type = lib.types.path;
    };

    shell = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.stdenv.shell}";
    };

    ispell = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.ispell}/bin/ispell";
    };

    editorconfig = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.editorconfig-core-c}/bin/editorconfig";
    };

    emojiSets = lib.mkOption {
      type = lib.types.path;
      default = "${emojiSets}";
    };

    font = lib.mkOption {
      type = lib.types.submodule {
        options = {
          face = lib.mkOption {
            type = lib.types.str;
          };

          size = lib.mkOption {
            type = lib.types.float;
          };
        };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.file = {
      ".emacs.d/init.el".text = ''
        (setq emacs-rc-git-path "${cfg.git}"
              emacs-rc-rg-path "${cfg.rg}"
              emacs-rc-browse-path "${cfg.browse}"
              emacs-rc-shell-path "${cfg.shell}"
              emacs-rc-ispell-path "${cfg.ispell}"
              emacs-rc-editorconfig-path "${cfg.editorconfig}"
              emacs-rc-emoji-sets-path "${cfg.emojiSets}"
              emacs-rc-font-face "${cfg.font.face}"
              emacs-rc-font-size ${toString cfg.font.size})

        (require 'emacs-rc)
      '';
    };
  };
}
