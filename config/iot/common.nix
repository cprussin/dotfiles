{}:

{ id, name, secrets, ... }:

{
  esphome = {
    name = id;
    platform = "ESP8266";
    board = "esp01_1m";
  };

  wifi = {
    ssid = "Centar";
    password = secrets.wifi;
    domain = ".lan";
    ap = {
      ssid = name;
      password = secrets.ap;
    };
  };

  logger.baud_rate = 0;
  captive_portal = { };
  api.password = secrets.api;
  ota.password = secrets.ota;

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
