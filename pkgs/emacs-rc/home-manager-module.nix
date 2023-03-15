{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.programs.emacs.emacs-rc;

  mkEmojiSets = emoji-pkgs:
    pkgs.linkFarm "emjoji-sets" (map
      (path: {
        inherit path;
        name = "${path.pname}-v${path.version}";
      })
      emoji-pkgs);
in {
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

    jpegtran = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.libjpeg}/bin/jpegtran";
    };

    pngnq = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.pngnq}/bin/pngnq";
    };

    exiftool = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.exiftool}/bin/exiftool";
    };

    pngcrush = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.pngcrush}/bin/pngcrush";
    };

    convert = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.imagemagick}/bin/convert";
    };

    optipng = lib.mkOption {
      type = lib.types.path;
      default = "${pkgs.optipng}/bin/optipng";
    };

    emojiSets = lib.mkOption {
      type = lib.types.path;
      default = "${mkEmojiSets [pkgs.emojione-png]}";
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
              emacs-rc-jpegtran-path "${cfg.jpegtran}"
              emacs-rc-pngnq-path "${cfg.pngnq}"
              emacs-rc-exiftool-path "${cfg.exiftool}"
              emacs-rc-pngcrush-path "${cfg.pngcrush}"
              emacs-rc-convert-path "${cfg.convert}"
              emacs-rc-optipng-path "${cfg.optipng}"
              emacs-rc-emoji-sets-path "${cfg.emojiSets}")

        (require 'emacs-rc)
      '';
    };
  };
}
