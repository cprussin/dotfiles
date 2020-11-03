{ callPackage, lib }: args:
let
  common = callPackage ./common.nix { };
  base = common args;
in
lib.recursiveUpdate base {
  binary_sensor = [
    {
      platform = "gpio";
      pin = {
        number = "GPIO4";
        mode = "INPUT_PULLUP";
        inverted = true;
      };
      name = "${args.name} Button";
      on_press = [{ "switch.toggle" = "relay"; }];
    }
  ];

  switch = [
    {
      inherit (args) name;
      platform = "gpio";
      pin = "GPIO14";
      id = "relay";
    }
  ];

  status_led.pin = {
    number = "GPIO12";
    inverted = true;
  };
}
