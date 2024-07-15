_: {
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
