{
  pkgs,
  lib,
  config,
  ...
}: let
  network = pkgs.callPackage ../../../lib/network.nix {};
  prussinnet-internal-zonefile = pkgs.writeText "db.internal.prussin.net" ''
    $ORIGIN internal.prussin.net.
    @	3600 IN	SOA sns.dns.icann.org. noc.dns.icann.org. (
            2017042745 ; serial
            7200       ; refresh (2 hours)
            3600       ; retry (1 hour)
            1209600    ; expire (2 weeks)
            3600       ; minimum (1 hour)
    )

    3600 IN NS a.iana-servers.net.
    3600 IN NS b.iana-servers.net.

    library            IN A     ${network.wireguard.nodes.crux.address}
    home-assistant     IN A     ${network.wireguard.nodes.crux.address}

    ${
      lib.concatStringsSep "\n" (
        lib.mapAttrsToList
        (host: node: "${host} IN A ${node.address}")
        network.wireguard.nodes
      )
    }
  '';
in {
  networking = {
    firewall = {
      allowedTCPPorts = [53];
      allowedUDPPorts = [53];
    };
    nameservers = [network.wireguard.nodes.crux.address];
  };
  services.coredns = {
    enable = true;
    config = ''
      internal.prussin.net {
        bind prussinnet
        file ${prussinnet-internal-zonefile}
        log
        errors
      }

      (recursive) {
        cache
        hosts prussin.net {
          ${network.home.static.crux.address} crux.prussin.net
          fallthrough
        }
        forward . 1.1.1.1 1.0.0.1
        log
        errors
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
