{
  lib,
  config,
  ...
}: let
  cfg = config.cursorTheme;
in {
  options.cursorTheme = {
    package = lib.mkOption {
      description = "The package that provides the cursors";
      default = null;
      type = lib.types.nullOr lib.types.package;
    };
    name = lib.mkOption {
      description = "The name of the cursor set from the package";
      type = lib.types.str;
    };
    lightName = lib.mkOption {
      description = "The cursor set from the package for light mode, where a desktop draws one per mode; null uses `name`";
      default = null;
      type = lib.types.nullOr lib.types.str;
    };
  };

  config = lib.mkIf (cfg.package != null) {
    home = {
      packages = [cfg.package];
      pointerCursor = {
        inherit (cfg) name package;
        gtk.enable = true;
      };
    };
  };
}
