{ lib, config, ... }:

{
  home-manager.users.${config.primaryUserName} = {
    # FIXME: If the cursor theme package isn't in the environment, then the GTK
    # configuration won't be able to find it, since paths appear hardcoded in
    # GTK.  There's likely a way to pass the path to GTK apps instead.
    home.packages = lib.mkForce [ config.cursorTheme.package ];

    xsession.pointerCursor = config.cursorTheme;
  };
}
