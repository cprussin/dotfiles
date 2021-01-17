{ config, pkgs, ... }:
let
  font = config.primary-user.home-manager.font;
  emoji-sets = pkgs.linkFarm "emoji-sets" [{
    name = "emojione";
    path = pkgs.emojione-png;
  }];
in
{
  primary-user.home-manager = {
    programs.emacs.enable = true;
    services.emacs.enable = true;
    home.file = {
      ".emacs.d/modules" = {
        source = ./modules;
        recursive = true;
      };

      ".emacs.d/init.el".text = ''
        (setq nix-config #s(hash-table
                            test equal
                            data (
                                  "paths" #s(hash-table
                                             test equal
                                             data (
                                                   "rg" "${pkgs.ripgrep}/bin/rg"
                                                   "browse" "${pkgs.launcher}/bin/browse"
                                                   "git" "${pkgs.git}/bin/git"
                                                   "msmtp" "${pkgs.msmtp}/bin/msmtp"
                                                   "shell" "${pkgs.stdenv.shell}"
                                                   "ispell" "${pkgs.ispell}/bin/ispell"
                                                   "emoji-sets" "${emoji-sets}"
                                             ))
                                  "font" #s(hash-table
                                            test equal
                                            data (
                                                  "face" "${font.face}"
                                                  "size" "${toString font.size}"
                                            ))
                                  )))

        (load (concat user-emacs-directory "modules/init"))
      '';
    };
  };
}
