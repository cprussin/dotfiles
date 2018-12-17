{ lib, ... }:

{
  options.terminal = lib.mkOption {
    type = lib.types.str;
    description = ''
      The string containing the path for the default terminal application.
    '';
  };
}
