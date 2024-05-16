{ pkgs, config, lib, ... }: let
  portalsDir = "${config.primary-user.home-manager.home.profileDirectory}/share/xdg-desktop-portal/portals";
in {
  environment.pathsToLink = [ "/share/xdg-desktop-portal" "/share/applications" ];

  # TODO replace with xdg.portal
  # (https://github.com/nix-community/home-manager/blob/master/modules/misc/xdg-portal.nix)
  # once it lands in home-manager release
  # primary-user.home-manager.xdg.portal = {
  #   enable = true;
  #   extraPortals = [ pkgs.xdg-desktop-portal-wlr ];
  #   configPackages = [ pkgs.sway ];
  #   config.sway.;
  # };

  primary-user.home-manager = {
    home = {
      packages = lib.mkForce [pkgs.xdg-desktop-portal pkgs.xdg-desktop-portal-wlr pkgs.xdg-desktop-portal-gtk pkgs.sway];
      sessionVariables.XDG_DESKTOP_PORTAL_DIR = portalsDir;
    };
    systemd.user.sessionVariables.XDG_DESKTOP_PORTAL_DIR = portalsDir;
    xdg.configFile."xdg-desktop-portal/sway-portals.conf".text = lib.generators.toINI { } {
      preferred.default = "wlr;gtk";
    };
  };
}
