{
  pkgs,
  lib,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};

  wireguardListenPort = 45676;

  peer-public-keys = {
    gemini = "hmvFMJuINO7N6EO04Pnf32FJFaW5N0zDctot3Qpo8gg=";
    pegasus = "H/hoEAN8RieoOSuxPG3y35fPdhxjLS+iNJcD2CoGS1g=";
    andromeda = "8LO9GBh9oQMnx2MCBjButhXy7ikim7ziqnBPzEUNJxY=";
    steam-deck = "AVscY2MnbeE7H3QkBWmJFhFJ6tMQR/77259nNZoaFwM=";
    printotron = "9T+j7wSMDyCliZ3Li4VCo+3VAY3S30OTQI3lhWnMiBA=";
    shauna-computer = "/kEbRsxX74Vm3mm54Brcuz27+s3F/yE3le/iEhS6AlM=";
    shauna-phone = "tifJWMO8/P4B5QDNpieB/CvLtCyvtuoXi5GnAQYywRU=";
    reece-computer = "BXzJ+WBs4QITPbliB84U7qkQsxVM6PaCJAyXy2pb0jA=";
    mom-vm = "ZycHSW5YU/++Csw73iLbZHo75QM4y38Q3kT3xADsFA0=";
  };

  psk-keyfile = peer: "${peer}-preshared-key";
in {
  deployment.keys =
    {
      wireguard-private-key = {
        keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/crux/private-key";
        destDir = "/secrets";
      };
    }
    // (lib.mapAttrs' (
        peer: _:
          lib.nameValuePair (psk-keyfile peer) {
            keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/${peer}/psk";
            destDir = "/secrets";
          }
      )
      peer-public-keys);

  networking = {
    firewall.interfaces."${config.interfaces.eth}".allowedUDPPorts = [wireguardListenPort];

    wireguard = {
      enable = true;
      interfaces.prussinnet = {
        listenPort = wireguardListenPort;
        privateKeyFile = config.deployment.keys.wireguard-private-key.path;
        ips = [network.wireguard.crux.cidr];

        peers =
          lib.mapAttrsToList (peer: publicKey: {
            inherit publicKey;
            name = peer;
            allowedIPs = ["${network.wireguard."${peer}".address}/128"];
            presharedKeyFile = config.deployment.keys."${psk-keyfile peer}".path;
          })
          peer-public-keys;
      };
    };
  };

  boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = "1";

  systemd.services =
    {
      wireguard-prussinnet = {
        after = ["wireguard-private-key-key.service"];
        requires = ["wireguard-private-key-key.service"];
      };
    }
    // (
      lib.mapAttrs'
      (peer: _:
        lib.nameValuePair "wireguard-prussinnet-peer-${peer}" {
          after = ["${psk-keyfile peer}-key.service"];
          requires = ["${psk-keyfile peer}-key.service"];
        })
      peer-public-keys
    );
}
