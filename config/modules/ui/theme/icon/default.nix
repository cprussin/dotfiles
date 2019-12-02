{ lib, config, ... }:

{
  options.iconTheme = lib.mkOption {
    description = "The icon theme for the UI.";
    type = lib.types.submodule {
      options = {
        package = lib.mkOption {
          description = "The package that provides the icons";
          type = lib.types.package;
        };
        name = lib.mkOption {
          description = "The name of the icon set from the package";
          type = lib.types.str;
        };
        size = lib.mkOption {
          description = "The icon size to use";
          type = lib.types.str;
        };
      };
    };
  };

  config.home-manager.users.${config.primary-user.name}.home.packages = lib.mkForce [
    config.iconTheme.package
  ];
}
