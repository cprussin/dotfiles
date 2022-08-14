{callPackage}: let
  passwords = callPackage ../../lib/passwords.nix {};
  network = callPackage ../../lib/network.nix {};
in
  {
    id,
    name,
    ...
  }: {
    esphome = {
      name = id;
      comment = name;
      platform = "ESP8266";
      board = "esp01_1m";
    };

    wifi = {
      ssid = "Centar";
      passwordCommand = passwords.getPassword "Connor/Wifi/Centar";
      domain = ".lan";
      manual_ip = {
        static_ip = network.iot."${id}";
        gateway = network.net-hardware.router;
        subnet = network.subnet;
      };
      ap = {
        ssid = name;
        passwordCommand = passwords.getPasswordField "Connor/Infrastructure/IoT/${name}" "AP";
      };
    };

    logger.baud_rate = 0;
    captive_portal = {};
    api.passwordCommand = passwords.getPasswordField "Connor/Infrastructure/IoT/${name}" "API";
    ota.passwordCommand = passwords.getPasswordField "Connor/Infrastructure/IoT/${name}" "OTA";

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

    button = [
      {
        platform = "restart";
        name = "Restart ${name}";
      }
    ];
  }
