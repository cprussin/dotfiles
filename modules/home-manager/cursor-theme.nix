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
  };

  config.home = lib.mkIf (cfg.package != null) {
    packages = [cfg.package];
    pointerCursor = cfg;
  };
}
