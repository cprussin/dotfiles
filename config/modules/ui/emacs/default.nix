{ config, pkgs, ... }:

{
  primary-user.home-manager = {
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
                                                   "ag" "${pkgs.silver-searcher}/bin/ag"
                                                   "browse" "${pkgs.launcher}/bin/browse"
                                                   "git" "${pkgs.git}/bin/git"
                                                   "msmtp" "${pkgs.msmtp}/bin/msmtp"
                                                   "shell" "${pkgs.stdenv.shell}"
                                             ))
                                  "primaryFont" #s(hash-table
                                                   test equal
                                                   data (
                                                         "face" "${config.fontTheme.primaryFont.face}"
                                                         "size" "${toString config.fontTheme.primaryFont.size}"
                                                   ))
                                  )))

        (load (concat user-emacs-directory "modules/init"))
      '';
    };

    systemd.user.services.emacs-daemon = {
      Unit = {
        Description = "Emacs text editor";
        Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
      };

      Install = {
        WantedBy = [ "default.target" ];
      };

      Service = {
        Type = "forking";
        ExecStart = "${pkgs.stdenv.shell} -l -c 'exec ${pkgs.emacs}/bin/emacs --daemon'";
        ExecStop = "${pkgs.emacs}/bin/emacsclient --eval '(kill-emacs)'";
        Restart = "on-failure";
        SyslogIdentifier = "emacs-daemon";
      };
    };
  };
}
