{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../../lib/network.nix {};
in {
  networking.wireguard = {
    enable = true;
    interfaces.prussinnet = {
      privateKeyFile = config.deployment.keys.wireguard-private-key.path;
      ips = [network.wireguard."${config.networking.hostName}".cidr];
      postSetup = ''
        echo -en 'nameserver ${network.wireguard.crux.address}\nsearch internal.prussin.net' |\
          ${pkgs.openresolv}/bin/resolvconf -a prussinnet
      '';
      postShutdown = "${pkgs.openresolv}/bin/resolvconf -d prussinnet";
      peers = [
        {
          name = "crux";
          endpoint = "crux.prussin.net:45676";
          allowedIPs = [network.wireguard.id.cidr];
          publicKey = "1ci1NontydmFf2Dmh/LVs528zd7r6SRHoeeby33q3hA=";
          presharedKeyFile = config.deployment.keys.wireguard-preshared-key.path;
        }
      ];
    };
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
