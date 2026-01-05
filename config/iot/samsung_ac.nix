{
  callPackage,
  lib,
}: args: let
  common = callPackage ./common.nix {};
  base = common args;
  thermostat = address: name: {
    inherit address;
    map_auto_to_heat_cool = true;
    climate = {
      name = "${name} Climate";
      visual.min_temperature = 18.333333333;
    };
    indoor_eva_in_temperature.name = "${name} EVA In temperature";
    indoor_eva_out_temperature.name = "${name} EVA Out temperature";
  };
in
  lib.recursiveUpdate base {
    esp32 = {
      board = "m5stack-atom";
      framework.type = "esp-idf";
    };

    uart = {
      tx_pin = "GPIO19";
      rx_pin = "GPIO22";
      baud_rate = 9600;
      parity = "EVEN";
    };

    external_components = {
      source = {
        type = "git";
        url = "https://github.com/cprussin/esphome_samsung_hvac_bus";
        ref = "handle-fahrenheit-temperature";
      };
      components = ["samsung_ac"];
    };

    samsung_ac = {
      non_nasa_keepalive = true;
      capabilities.fan_modes = false;
      devices = [
        (thermostat "00" "Bodhi's Room")
        (thermostat "01" "Guest Room Room")
        (thermostat "02" "Master Bedroom Room")
        {
          address = "c8";
          error_code.name = "Error Code";
          outdoor_instantaneous_power.name = "Outdoor Instantaneous Power";
          outdoor_cumulative_energy.name = "Outdoor Cumulative Energy";
          outdoor_current.name = "Outdoor Current";
          outdoor_voltage.name = "Outdoor Voltage";
          outdoor_temperature.name = "Outdoor temperature";
        }
      ];
    };
  }
