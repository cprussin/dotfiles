{ lib, config, ... }:

{
  options.cursorTheme = lib.mkOption {
    description = "";
    type = lib.types.submodule {
      options = {
        package = lib.mkOption {
          description = "The package that provides the cursors";
          type = lib.types.package;
        };
        name = lib.mkOption {
          description = "The name of the cursor set from the package";
          type = lib.types.str;
        };
      };
    };
  };

  config.home-manager.users.${config.primary-user.name} = {
    # FIXME: If the cursor theme package isn't in the environment, then the GTK
    # configuration won't be able to find it, since paths appear hardcoded in
    # GTK.  There's likely a way to pass the path to GTK apps instead.
    home.packages = lib.mkForce [ config.cursorTheme.package ];
    xsession.pointerCursor = config.cursorTheme;
  };
}
