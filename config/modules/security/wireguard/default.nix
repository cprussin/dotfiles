{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../../lib/network.nix {};
in {
  networking = {
    wireguard = {
      enable = true;
      interfaces.prussinnet = {
        privateKeyFile = config.deployment.keys.wireguard-private-key.path;
        ips = [network.wireguard.nodes."${config.networking.hostName}".cidr];
        postSetup = ''
          echo -en 'nameserver ${network.wireguard.nodes.crux.address}\nsearch internal.prussin.net' |\
            ${pkgs.openresolv}/bin/resolvconf -a prussinnet
        '';
        postShutdown = "${pkgs.openresolv}/bin/resolvconf -d prussinnet";
        peers = [
          {
            endpoint = "crux.prussin.net:45676";
            allowedIPs = [network.wireguard.id.cidr];
            publicKey = "1ci1NontydmFf2Dmh/LVs528zd7r6SRHoeeby33q3hA=";
            presharedKeyFile = config.deployment.keys.wireguard-preshared-key.path;
          }
        ];
      };
    };

    # TODO this is a hack around dhcpcd reporting it's up before actually having
    # a lease, and some services -- like wireguard -- using that to determine
    # it's safe to startup, and then not restarting on failure.  See
    # https://github.com/NixOS/nixpkgs/issues/63869.  This is not a good
    # permanent fix and should be removed when some sufficient fix lands
    # upstream.
    dhcpcd = {
      wait = "ipv4";
      extraConfig = "noipv4ll";
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
