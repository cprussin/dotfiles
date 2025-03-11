{
  lib,
  config,
  ...
}: let
  cfg = config.default-terminal;
in {
  options.default-terminal = {
    enable = lib.mkEnableOption "Default terminal";

    enableApplication = lib.mkOption {
      type = lib.types.bool;
      description = ''
        Install the application as well as the terminfo file (set to false to
        only install the terminfo file).
      '';
      default = true;
    };

    bin = lib.mkOption {
      type = lib.types.path;
      description = ''
        The path to the executable for the terminal that should be run whenever
        anything needs to open a terminal.
      '';
    };

    pkg = lib.mkOption {
      type = lib.types.package;
      description = "The package containing the default terminal";
    };

    termname = lib.mkOption {
      type = lib.types.str;
      description = "The name of the terminal";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      [cfg.pkg.terminfo]
      ++ (
        if cfg.enableApplication
        then [cfg.pkg]
        else []
      );
    wayland.windowManager.sway.config.terminal = cfg.bin;
  };
}
