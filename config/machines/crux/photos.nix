{ pkgs, config, ... }: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  deployment.keys = {
    immich-secrets = {
      inherit (config.services.immich) user group;
      keyCommand = passwords.getImmichSecrets "Connor/Infrastructure/immich/database";
    };

    "photos.internal.prussin.net.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/photos.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };

    "photos.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/photos.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
  };

  networking.firewall.interfaces = {
    prussinnet.allowedTCPPorts = [80 443];
  };

  services = {
    immich = {
      enable = true;
      secretsFile = config.deployment.keys."immich-secrets".path;
      mediaLocation = "/srv/Library/Photos";
    };

    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."photos.internal.prussin.net" = {
        listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
        sslCertificate = config.deployment.keys."photos.internal.prussin.net.crt".path;
        sslCertificateKey = config.deployment.keys."photos.internal.prussin.net.key".path;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:${toString config.services.immich.port}";
      };
    };
  };

  systemd.services = {
    immich-server = {
      requires = ["import-tank.service"];
      after = ["import-tank.service"];
    };
    nginx = {
      after = [
        "photos.internal.prussin.net.crt-key.service"
        "photos.internal.prussin.net.key-key.service"
      ];
      requires = [
        "photos.internal.prussin.net.crt-key.service"
        "photos.internal.prussin.net.key-key.service"
      ];
    };
  };
}
