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
        number = "GPIO0";
        mode = "INPUT_PULLUP";
        inverted = true;
      };
      name = "${args.plug1.name} Button";
      on_press = [{ "switch.toggle" = "relay1"; }];
    }
    {
      platform = "gpio";
      pin = {
        number = "GPIO4";
        mode = "INPUT_PULLUP";
        inverted = true;
      };
      name = "${args.plug2.name} Button";
      on_press = [{ "switch.toggle" = "relay2"; }];
    }
  ];

  switch = [
    {
      inherit (args.plug1) name;
      platform = "gpio";
      pin = "GPIO14";
      id = "relay1";
      on_turn_on = [{ "switch.turn_on" = "led1"; }];
      on_turn_off = [{ "switch.turn_off" = "led1"; }];
    }
    {
      platform = "gpio";
      pin = "GPIO13";
      id = "led1";
      inverted = true;
    }
    {
      inherit (args.plug2) name;
      platform = "gpio";
      pin = "GPIO12";
      id = "relay2";
      on_turn_on = [{ "switch.turn_on" = "led2"; }];
      on_turn_off = [{ "switch.turn_off" = "led2"; }];
    }
    {
      platform = "gpio";
      pin = "GPIO2";
      id = "led2";
      inverted = true;
    }
  ];

  status_led.pin = {
    number = "GPIO1";
    inverted = true;
  };
}
