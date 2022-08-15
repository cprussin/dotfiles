{
  config,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  imports = [
    ./hardware.nix
    ../../profiles/laptop
  ];

  primary-user.name = "cprussin";
  networking = {
    hostName = "gemini";
    hostId = "72b0fa88";
  };
  environment.etc."machine-id".text = "1e07c7df74514d51872dac89ab314e7c\n";
  services.getty.greetingLine = builtins.readFile ./greeting;
  primary-user.home-manager.wayland.windowManager.sway.config.output.eDP-1.scale = "1.15";

  networking.wireguard = {
    enable = true;
    interfaces.prussinnet = {
      privateKeyFile = config.deployment.keys.wireguard-private-key.path;
      ips = ["${network.wireguard.gemini}/24"];
      postSetup = "echo -n 'nameserver ${network.nixos-containers.coredns}' | ${pkgs.openresolv}/bin/resolvconf -a prussinnet";
      postShutdown = "${pkgs.openresolv}/bin/resolvconf -d prussinnet";
      peers = [
        {
          endpoint = "720-natoma-drive.prussin.net:45676";
          allowedIPs = ["${network.wireguard.id}/24" "${network.nixos-containers.id}/24"];
          publicKey = "1ci1NontydmFf2Dmh/LVs528zd7r6SRHoeeby33q3hA=";
          presharedKeyFile = config.deployment.keys.wireguard-preshared-key.path;
        }
      ];
    };
  };

  deployment.keys = {
    wireguard-private-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/gemini/private-key";
      destDir = "/secrets";
    };
    wireguard-preshared-key = {
      keyCommand = passwords.getPassword "Connor/Infrastructure/wireguard/gemini/psk";
      destDir = "/secrets";
    };
  };

  security.pki.certificateFiles = [./prussin-net-ca.pem];
}
