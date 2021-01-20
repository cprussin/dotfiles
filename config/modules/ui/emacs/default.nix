{ config, pkgs, ... }:
let
  font = config.primary-user.home-manager.font;
in
{
  primary-user.home-manager = {
    programs.emacs = {
      enable = true;
      emacs-rc = {
        enable = true;
        browse = "${pkgs.launcher}/bin/browse";
        font = { inherit (font) face size; };
      };
    };
    services.emacs.enable = true;
  };
}
