{
  pkgs,
  lib,
  ...
}: {
  environment.pathsToLink = ["/share/xdg-desktop-portal" "/share/applications"];

  primary-user.home-manager = {
    home.packages = lib.mkForce [pkgs.xdg-desktop-portal];
    xdg.portal.enable = true;
  };
}
