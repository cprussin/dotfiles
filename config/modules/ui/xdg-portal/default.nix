{pkgs, ...}: {
  environment.pathsToLink = ["/share/xdg-desktop-portal" "/share/applications"];

  primary-user.home-manager.xdg.portal = {
    enable = true;
    extraPortals = [pkgs.xdg-desktop-portal-wlr];
    configPackages = [pkgs.sway];
  };
}
