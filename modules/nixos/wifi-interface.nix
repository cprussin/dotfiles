{ lib, ... }:

{
  options.wifi-interface = lib.mkOption {
    description = "The wifi interface.";
    type = lib.types.str;
  };
}
