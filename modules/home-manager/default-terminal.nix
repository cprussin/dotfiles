{ lib, config, ... }:

let
  cfg = config.default-terminal;
in

{
  options.default-terminal = {
    enable = lib.mkEnableOption "Default terminal";

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

    terminfo = lib.mkOption {
      type = lib.types.package;
      description = "The package containing the default terminal's terminfo";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.pkg cfg.terminfo ];
  };
}
