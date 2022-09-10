{
  pkgs,
  config,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
  wireguardListenPort = 45676;
  gemini-public-key = "hmvFMJuINO7N6EO04Pnf32FJFaW5N0zDctot3Qpo8gg=";
  pegasus-public-key = "/ta8gnmtXXSwvz5ueU+I7jD6OKCZ2hSlyZ0DnzK1P20=";
  shauna-phone-public-key = "tifJWMO8/P4B5QDNpieB/CvLtCyvtuoXi5GnAQYywRU=";
in {
  deployment.keys = {
    wireguard-private-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/crux/private-key";
      destDir = "/secrets";
    };
    gemini-preshared-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/gemini/psk";
      destDir = "/secrets";
    };
    pegasus-preshared-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/pegasus/psk";
      destDir = "/secrets";
    };
    shauna-phone-preshared-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/shauna-phone/psk";
      destDir = "/secrets";
    };
  };

  networking = {
    firewall.allowedUDPPorts = [wireguardListenPort];

    wireguard = {
      enable = true;
      interfaces.prussinnet = {
        listenPort = wireguardListenPort;
        privateKeyFile = config.deployment.keys.wireguard-private-key.path;
        ips = [network.wireguard.nodes.crux.cidr];
        peers = [
          {
            allowedIPs = ["${network.wireguard.nodes.gemini.address}/32"];
            publicKey = gemini-public-key;
            presharedKeyFile = config.deployment.keys.gemini-preshared-key.path;
          }
          {
            allowedIPs = ["${network.wireguard.nodes.pegasus.address}/32"];
            publicKey = pegasus-public-key;
            presharedKeyFile = config.deployment.keys.pegasus-preshared-key.path;
          }
          {
            allowedIPs = ["${network.wireguard.nodes.shauna-phone.address}/32"];
            publicKey = shauna-phone-public-key;
            presharedKeyFile = config.deployment.keys.shauna-phone-preshared-key.path;
          }
        ];
      };
    };
  };

  systemd.services.wireguard-prussinnet = {
    after = [
      "wireguard-private-key-key.service"
      "gemini-preshared-key-key.service"
      "pegasus-preshared-key-key.service"
      "shauna-phone-preshared-key-key.service"
    ];
    requires = [
      "wireguard-private-key-key.service"
      "gemini-preshared-key-key.service"
      "pegasus-preshared-key-key.service"
      "shauna-phone-preshared-key-key.service"
    ];
  };
}
