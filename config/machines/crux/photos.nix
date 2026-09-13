{
  pkgs,
  lib,
  config,
  ...
}: let
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

  # immich comes from nixpkgs-unstable rather than 26.05, which stopped at a
  # 2.x that upstream abandoned -- see modules/system/nix.  That input floats,
  # so an `[flake] update sources` can carry immich across a major version
  # with nobody deciding to, and the next deploy would restart the server into
  # a forward-only database migration that upstream gives no way back out of.
  # Refuse to evaluate instead: the bump is fine, it just has to be somebody's
  # decision, made with a dump in hand.
  assertions = [
    {
      assertion = lib.versions.major config.services.immich.package.version == "3";
      message = ''
        immich is no longer 3.x (nixpkgs-unstable now has ${config.services.immich.package.version}).
        Dump the database before deploying it -- `pg_dumpall` as the postgres
        user on crux -- and check upstream's release notes for whatever this
        major changes.  Then raise the version in machines/crux/photos.nix.
      '';
    }
  ];

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
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.immich.port}";
          proxyWebsockets = true;
          recommendedProxySettings = true;
          extraConfig = ''
            client_max_body_size 50000M;
            proxy_read_timeout   600s;
            proxy_send_timeout   600s;
            send_timeout         600s;
          '';
        };
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
