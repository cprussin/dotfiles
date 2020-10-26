{ callPackage, lib }: args:

let
  common = callPackage ./common.nix {};
  common_tuya = callPackage ./common_tuya.nix {};
  base = lib.recursiveUpdate (common args) (common_tuya args);
in

lib.recursiveUpdate base {
  uart.baud_rate = 115200;
  light = [
    {
      platform = "tuya";
      name = "${args.name} Light";
      dimmer_datapoint = 10;
      switch_datapoint = 9;
      min_value_datapoint = 105;
      min_value = args.min_value or 10;
      max_value = 1000;
    }
  ];
  fan = [
    {
      inherit (args) name;
      platform = "tuya";
      switch_datapoint = 1;
      speed_datapoint = 3;
    }
  ];
}
