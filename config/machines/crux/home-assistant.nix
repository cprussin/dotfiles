{
  pkgs,
  config,
  lib,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
  homeAssistantPort = "8123";
in {
  deployment.keys = {
    "home-assistant.internal.prussin.net.crt" = {
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/home-assistant.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
      group = config.users.users.nginx.group;
    };
    "home-assistant.internal.prussin.net.key" = {
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/home-assistant.internal.prussin.net/key";
      user = config.users.users.nginx.name;
      group = config.users.users.nginx.group;
    };
  };

  networking.firewall.allowedTCPPorts = [80 443];
  users.users.nginx.extraGroups = ["keys"];
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    virtualHosts."home-assistant.internal.prussin.net" = {
      listenAddresses = [network.wireguard.nodes.crux.address];
      sslCertificate = "/run/keys/home-assistant.internal.prussin.net.crt";
      sslCertificateKey = "/run/keys/home-assistant.internal.prussin.net.key";
      forceSSL = true;
      extraConfig = "proxy_buffering off;";
      locations."/" = {
        proxyPass = "http://localhost:8123";
        proxyWebsockets = true;
      };
    };
  };

  virtualisation.oci-containers.containers.home-assistant = {
    image = "ghcr.io/home-assistant/home-assistant:stable";
    environment.TZ = "America/Los_Angeles";
    ports = ["127.0.0.1:${homeAssistantPort}:${homeAssistantPort}"];
    extraOptions =
      [
        "--device=/dev/ttyACM0:/dev/ttyACM0"
      ]
      ++ (
        lib.mapAttrsToList
        (host: node: "--add-host=${host}:${node.address}")
        network.home.iot
      );
    volumes = ["/var/lib/hass:/config"];
  };

  systemd.services = {
    "container@home-assistant-proxy" = {
      after = [
        "home-assistant.internal.prussin.net.crt-key.service"
        "home-assistant.internal.prussin.net.key-key.service"
      ];
      requires = [
        "home-assistant.internal.prussin.net.crt-key.service"
        "home-assistant.internal.prussin.net.key-key.service"
      ];
    };
    podman-home-assistant = {
      requires = ["import-tank.service"];
      after = ["import-tank.service"];
    };
  };
}