{
  pkgs,
  lib,
  config,
  ...
}: let
  network = pkgs.callPackage ../../../lib/network.nix {};

  prussinnet-internal-hosts = pkgs.writeText "internal.prussin.net.hosts" (
    lib.concatStringsSep "\n" (
      map
      (wgNet: ''
        ${wgNet.crux.address} library.internal.prussin.net home-assistant.internal.prussin.net matrix.internal.prussin.net passwords.internal.prussin.net circinus.internal.prussin.net eyes.internal.prussin.net
        ${
          lib.concatStringsSep "\n" (
            lib.mapAttrsToList
            (host: node: "${node.address} ${host}.internal.prussin.net")
            (builtins.removeAttrs wgNet ["id" "prefixLength"])
          )
        }
      '')
      [
        network.wireguard6
        network.wireguard4
      ]
    )
  );
in {
  networking = {
    firewall.interfaces.prussinnet = {
      allowedTCPPorts = [53];
      allowedUDPPorts = [53];
    };
    nameservers = [network.wireguard6.crux.address network.wireguard4.crux.address];
  };
  services.coredns = {
    enable = true;
    config = ''
      (logging) {
        prometheus
        log
        errors
      }

      internal.prussin.net {
        bind prussinnet
        hosts ${prussinnet-internal-hosts} internal.prussin.net {
          reload 0
        }
        import logging
      }

      . {
        bind prussinnet
        forward . tls://2606:4700:4700::1111 tls://2606:4700:4700::1001 {
          tls_servername tls.cloudflare-dns.com
        }
        cache
        import logging
      }
    '';
  };
}
