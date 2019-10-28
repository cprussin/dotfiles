{ lib, ... }:

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
}
