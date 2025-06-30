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
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/home-assistant.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };

    "home-assistant.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/home-assistant.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
  };

  networking.firewall.interfaces = {
    prussinnet.allowedTCPPorts = [80 443];
    podman0.allowedTCPPorts = [3000];
  };

  users = {
    groups.zwave-js = {};
    users = {
      nginx.extraGroups = ["keys"];
      zwave-js = {
        isSystemUser = true;
        group = "zwave-js";
      };
    };
  };

  services = {
    zwave-js = {
      enable = true;
      serialPort = "/dev/ttyACM1";
      secretsConfigFile = "/secrets/zwave-js-keys.json";
    };
    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."home-assistant.internal.prussin.net" = {
        listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
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
        (builtins.removeAttrs network.home ["id" "prefixLength" "router"])
      );
    volumes = ["/var/lib/hass:/config"];
  };

  systemd.services = {
    nginx = {
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
