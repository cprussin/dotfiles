{ callPackage }:

let
  passwords = callPackage ../../lib/passwords.nix { };
in

{ id, name, ... }:

{
  esphome = {
    name = id;
    platform = "ESP8266";
    board = "esp01_1m";
  };

  wifi = {
    ssid = "Centar";
    passwordCommand = passwords.getPassword "Wifi/Centar";
    domain = ".lan";
    ap = {
      ssid = name;
      passwordCommand = passwords.getPasswordField "Infrastructure/IoT/${name}" "AP";
    };
  };

  logger.baud_rate = 0;
  captive_portal = { };
  api.passwordCommand = passwords.getPasswordField "Infrastructure/IoT/${name}" "API";
  ota.passwordCommand = passwords.getPasswordField "Infrastructure/IoT/${name}" "OTA";

  sensor = [
    {
      platform = "wifi_signal";
      name = "${name} WiFi Signal";
    }
    {
      platform = "uptime";
      name = "${name} Uptime";
    }
  ];
}
