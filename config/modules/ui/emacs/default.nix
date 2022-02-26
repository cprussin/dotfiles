{pkgs, ...}: {
  primary-user.home-manager = {
    programs.emacs = {
      enable = true;
      emacs-rc = {
        enable = true;
        browse = "${pkgs.launcher}/bin/browse";
      };
    };
    services.emacs.enable = true;
  };
}
