{ pkgs, lib, config, ... }:

{
  primary-user.home-manager = {
    # FIXME: If the cursor theme package isn't in the environment, then the GTK
    # configuration won't be able to find it, since paths appear hardcoded in
    # GTK.  There's likely a way to pass the path to GTK apps instead.
    home.packages = lib.mkForce [
      config.primary-user.home-manager.cursorTheme.package
    ];

    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
    };
    cursorTheme = {
      package = pkgs.numix-cursor-theme;
      name = "Numix-Cursor";
    };
  };
}
