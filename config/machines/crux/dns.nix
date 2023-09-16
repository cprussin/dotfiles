{
  pkgs,
  lib,
  config,
  ...
}: let
  network = pkgs.callPackage ../../../lib/network.nix {};

  prussinnet-internal-hosts = pkgs.writeText "internal.prussin.net.hosts" ''
    ${network.wireguard.crux.address} library.internal.prussin.net home-assistant.internal.prussin.net matrix.internal.prussin.net
    ${
      lib.concatStringsSep "\n" (
        lib.mapAttrsToList
        (host: node: "${node.address} ${host}.internal.prussin.net")
        (builtins.removeAttrs network.wireguard ["id" "prefixLength"])
      )
    }
  '';
in {
  networking = {
    firewall.interfaces."${config.interfaces.eth}" = {
      allowedTCPPorts = [53];
      allowedUDPPorts = [53];
    };
    firewall.interfaces.prussinnet = {
      allowedTCPPorts = [53];
      allowedUDPPorts = [53];
    };
    nameservers = [network.wireguard.crux.address];
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
