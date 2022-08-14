{
  callPackage,
  lib,
}: args: let
  common = callPackage ./common.nix {};
  commonTuya = callPackage ./common_tuya.nix {};
  base = lib.recursiveUpdate (common args) (commonTuya args);
in
  lib.recursiveUpdate base {
    uart.baud_rate = 9600;
    light = [
      {
        inherit (args) name;
        id = "dimmer";
        platform = "tuya";
        dimmer_datapoint = 2;
        min_value_datapoint = 3;
        switch_datapoint = 1;
        min_value = args.min_value or 10;
        max_value = 1000;
      }
    ];
  }
