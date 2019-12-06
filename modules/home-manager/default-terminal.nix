{ lib, ... }:

{
  options.default-terminal = lib.mkOption {
    type = lib.types.path;
    description = ''
      The path to the executable for the terminal that should be run whenever
      anything needs to open a terminal.
    '';
  };
}
