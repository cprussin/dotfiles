{ callPackage, lib }: args:

let
  common = callPackage ./common.nix {};
  base = common args;
in

lib.recursiveUpdate base {
  binary_sensor = [
    {
      platform = "gpio";
      pin = {
        number = "GPIO13";
        inverted = true;
      };
      name = "${args.name} Button";
      on_release = [ { "switch.toggle" = "relay"; } ];
    }
  ];

  switch = [
    {
      inherit (args) name;
      platform = "gpio";
      pin = "GPIO12";
      id = "relay";
      on_turn_on = [ { "switch.turn_off" = "nightlight"; } ];
      on_turn_off = [ { "switch.turn_on" = "nightlight"; } ];
    }
    {
      platform = "gpio";
      pin = "GPIO4";
      id = "nightlight";
      inverted = true;
    }
  ];

  status_led.pin = {
    number = "GPIO5";
    inverted = true;
  };
}
