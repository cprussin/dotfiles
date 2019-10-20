{ pkgs, lib, config, ... }:

{
  home-manager.users.${config.primaryUserName} = { ... }: {
    # FIXME: If numix-cursor-theme isn't in the environment, then the GTK
    # configuration won't be able to find it, since paths appear hardcoded in
    # GTK.  There's likely a way to pass the path to GTK apps instead.
    home.packages = lib.mkForce [
      pkgs.numix-cursor-theme
    ];

    xsession.pointerCursor = {
      package = pkgs.numix-cursor-theme;
      name = "Numix-Cursor";
    };
  };
}
