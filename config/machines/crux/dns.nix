{
  pkgs,
  lib,
  config,
  ...
}: let
  network = pkgs.callPackage ../../../lib/network.nix {};

  prussinnet-internal-hosts = pkgs.writeText "internal.prussin.net.hosts" ''
    ${network.wireguard.nodes.crux.address} library.internal.prussin.net home-assistant.internal.prussin.net matrix.internal.prussin.net
    ${
      lib.concatStringsSep "\n" (
        lib.mapAttrsToList
        (host: node: "${node.address} ${host}.internal.prussin.net")
        network.wireguard.nodes
      )
    }
  '';

  prussinnet-override-hosts = pkgs.writeText "prussin.net.hosts" ''
    ${network.home.static.crux.address} crux.prussin.net
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
    nameservers = [network.wireguard.nodes.crux.address];
  };
  services.coredns = {
    enable = true;
    config = ''
      (logging) {
        prometheus
        log
        errors
      }

      (recursive) {
        hosts ${prussinnet-override-hosts} prussin.net {
          reload 0
          fallthrough
        }
        forward . tls://1.1.1.1 tls://1.0.0.1 {
          tls_servername tls.cloudflare-dns.com
        }
        cache
        import logging
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
        import recursive
      }

      . {
        bind ${config.interfaces.eth}
        import recursive
      }
    '';
  };
}
