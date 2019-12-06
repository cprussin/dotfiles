{ lib, config, ... }:

let
  cfg = config.iconTheme;
in

{
  options.iconTheme = {
    package = lib.mkOption {
      description = "The package that provides the icons";
      default = null;
      type = lib.types.nullOr lib.types.package;
    };
    name = lib.mkOption {
      description = "The name of the icon set from the package";
      type = lib.types.str;
    };
  };

  config = lib.mkIf (cfg.package != null) {
    home.packages = [ config.iconTheme.package ];
    programs.mako.icon-path = [
      "${config.iconTheme.package}/share/icons/${config.iconTheme.name}"
    ];
  };
}
