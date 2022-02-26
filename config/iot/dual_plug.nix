{
  callPackage,
  lib,
}: args: let
  common = callPackage ./common.nix {};
  base = common args;
  plug1Name = args.plug1.name or "Plug 1";
  plug2Name = args.plug2.name or "Plug 2";
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
        name = "${plug1Name} Button";
        on_press = [{"switch.toggle" = "relay1";}];
      }
      {
        platform = "gpio";
        pin = {
          number = "GPIO4";
          mode = "INPUT_PULLUP";
          inverted = true;
        };
        name = "${plug2Name} Button";
        on_press = [{"switch.toggle" = "relay2";}];
      }
    ];

    switch = [
      {
        name = plug1Name;
        platform = "gpio";
        pin = "GPIO14";
        id = "relay1";
        on_turn_on = [{"switch.turn_on" = "led1";}];
        on_turn_off = [{"switch.turn_off" = "led1";}];
      }
      {
        platform = "gpio";
        pin = "GPIO13";
        id = "led1";
        inverted = true;
      }
      {
        name = plug2Name;
        platform = "gpio";
        pin = "GPIO12";
        id = "relay2";
        on_turn_on = [{"switch.turn_on" = "led2";}];
        on_turn_off = [{"switch.turn_off" = "led2";}];
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
