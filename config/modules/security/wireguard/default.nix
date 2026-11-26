{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../../lib/network.nix {};
  resolvectl = "${config.systemd.package}/bin/resolvectl";
in {
  services.resolved = {
    enable = true;
    # avahi answers mDNS.
    settings.Resolve.MulticastDNS = false;
  };

  systemd.services.wireguard-prussinnet = {
    wants = ["systemd-resolved.service"];
    after = ["systemd-resolved.service"];
  };

  networking = {
    wireguard = {
      enable = true;
      interfaces.prussinnet = {
        privateKeyFile = config.deployment.keys.wireguard-private-key.path;
        ips = [network.wireguard6."${config.networking.hostName}".cidr network.wireguard4."${config.networking.hostName}".cidr];
        # Split DNS: only internal.prussin.net goes to crux, over the tunnel.
        # Everything else uses the network's own DNS, so a captive portal
        # that holds the tunnel down doesn't stall every lookup.
        postSetup = ''
          ${resolvectl} dns prussinnet ${network.wireguard6.crux.address} ${network.wireguard4.crux.address}
          ${resolvectl} domain prussinnet internal.prussin.net
          ${resolvectl} default-route prussinnet false
        '';
        peers = [
          {
            name = "crux";
            endpoint = "crux.prussin.net:45676";
            allowedIPs = [network.wireguard6.id.cidr network.wireguard4.id.cidr];
            publicKey = "1ci1NontydmFf2Dmh/LVs528zd7r6SRHoeeby33q3hA=";
            presharedKeyFile = config.deployment.keys.wireguard-preshared-key.path;
          }
        ];
      };
    };
    dhcpcd.denyInterfaces = ["prussinnet"];
  };

  deployment.keys = {
    wireguard-private-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/${config.networking.hostName}/private-key";
      destDir = "/secrets";
    };
    wireguard-preshared-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/${config.networking.hostName}/psk";
      destDir = "/secrets";
    };
  };
}
