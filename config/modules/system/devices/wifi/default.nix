_: {
  interfaces.wifi = "wlan0";
  networking = {
    wireless.iwd = {
      enable = true;
      settings = {
        General.EnableNetworkConfiguration = true;
        Network.NameResolvingService = "resolvconf";
      };
    };
    dhcpcd.denyInterfaces = ["wlan*"];
  };
}
