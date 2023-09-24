{callPackage}: let
  passwords = callPackage ../../lib/passwords.nix {};
  network = callPackage ../../lib/network.nix {};

  subnets = {
    "8" = "255.0.0.0";
    "9" = "255.128.0.0";
    "10" = "255.192.0.0";
    "11" = "255.224.0.0";
    "12" = "255.240.0.0";
    "13" = "255.248.0.0";
    "14" = "255.252.0.0";
    "15" = "255.254.0.0";
    "16" = "255.255.0.0";
    "17" = "255.255.128.0";
    "18" = "255.255.192.0";
    "19" = "255.255.224.0";
    "20" = "255.255.240.0";
    "21" = "255.255.248.0";
    "22" = "255.255.252.0";
    "23" = "255.255.254.0";
    "24" = "255.255.255.0";
    "25" = "255.255.255.128";
    "26" = "255.255.255.192";
    "27" = "255.255.255.224";
    "28" = "255.255.255.240";
    "29" = "255.255.255.248";
    "30" = "255.255.255.252";
  };
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
      password.keyCommand = passwords.getPassword "Connor/Wifi/Centar";
      domain = ".lan";
      manual_ip = {
        subnet = subnets."${toString network.home.prefixLength}";
        static_ip = network.home."${id}".address;
        gateway = network.home.router.address;
      };
      ap = {
        ssid = name;
        password.keyCommand = passwords.getPasswordField "Connor/Infrastructure/IoT/${name}" "AP";
      };
    };

    logger.baud_rate = 0;
    captive_portal = {};
    api.encryption.key.keyCommand = passwords.getPasswordField "Connor/Infrastructure/IoT/${name}" "Encryption Key";
    ota.password.keyCommand = passwords.getPasswordField "Connor/Infrastructure/IoT/${name}" "OTA";

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
