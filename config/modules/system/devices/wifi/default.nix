_: {
  interfaces.wifi = "wlan0";
  networking = {
    wireless.iwd = {
      enable = true;
      settings = {
        General = {
          EnableNetworkConfiguration = true;
          # iwd only looks for a better AP once the signal drops below these.
          # Its defaults (-70 / -76 dBm) keep a laptop on a far AP when a near
          # one is in range, so start looking sooner.
          RoamThreshold = -65;
          RoamThreshold5G = -70;
        };
        Network = {
          NameResolvingService = "resolvconf";
          # iwd's routes default to metric 300, ahead of dhcpcd's wired routes
          # (1000 + ifindex), so a docked laptop sent everything over wifi.
          # Sit behind any wired link instead.
          RoutePriorityOffset = 3000;
        };
      };
    };
    dhcpcd.denyInterfaces = ["wlan*"];
  };
}
