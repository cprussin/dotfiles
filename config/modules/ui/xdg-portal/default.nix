{
  pkgs,
  lib,
  ...
}: {
  # Neither is a NixOS default: /share/xdg-desktop-portal carries the .portal
  # files the frontend reads to discover backends, /share/applications the
  # desktop entries.  With `useUserPackages` the per-user profile is filtered
  # by this list, so a backend package that isn't linked here is invisible.
  # (/share/dbus-1, which carries the service files the frontend activates a
  # backend by name through, is already linked by `services.dbus`; /share/
  # systemd, which carries the units those service files name, is a default.)
  environment.pathsToLink = [
    "/share/applications"
    "/share/xdg-desktop-portal"
  ];

  primary-user.home-manager = {
    # home-manager installs the frontend and everything in `extraPortals`
    # through `home.packages`, at the normal priority this repo's `lib.mkForce`
    # on that list discards -- so `extraPortals` alone installs nothing, and
    # each backend has to be named here too.  Setting it is still required
    # (home-manager asserts on an empty list) and is what the generated
    # portals.conf is checked against.
    home.packages = lib.mkForce [
      pkgs.xdg-desktop-portal
      pkgs.xdg-desktop-portal-gtk
    ];

    # gtk is the backend that implements FileChooser, AppChooser, Settings,
    # Print and most of the rest.  wlr, added by the sway module, implements
    # only Screenshot and ScreenCast, so without this the interfaces every GTK
    # app asks for on startup have no implementation at all -- including the
    # AppChooser that the frontend's own OpenURI falls back to when there is no
    # default handler for a URL.
    xdg.portal = {
      enable = true;
      extraPortals = [pkgs.xdg-desktop-portal-gtk];
    };
  };
}
