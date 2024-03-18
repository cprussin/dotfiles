{
  config,
  pkgs,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
in {
  deployment.keys = {
    vaultwarden-secrets = {
      inherit (config.users.users.vaultwarden) group;
      # Replace `null` with "Connor/Infrastructure/vaultwarden/admin_token" to re-enable admin access
      keyCommand = passwords.getVaultwardenSecrets "Connor/Infrastructure/vaultwarden/database" "Connor/Infrastructure/vaultwarden/push" null;
      user = config.users.users.vaultwarden.name;
    };

    "passwords.internal.prussin.net.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/passwords.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };

    "passwords.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/passwords.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
  };

  services = {
    vaultwarden = {
      enable = true;
      dbBackend = "postgresql";
      config = {
        DOMAIN = "https://passwords.internal.prussin.net";
        SIGNUPS_ALLOWED = false;
        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8222;
        PUSH_ENABLED = true;
      };
      environmentFile = config.deployment.keys."vaultwarden-secrets".path;
    };

    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."passwords.internal.prussin.net" = {
        listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
        sslCertificate = config.deployment.keys."passwords.internal.prussin.net.crt".path;
        sslCertificateKey = config.deployment.keys."passwords.internal.prussin.net.key".path;
        forceSSL = true;
        http2 = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString config.services.vaultwarden.config.ROCKET_PORT}";
          proxyWebsockets = true;
        };
      };
    };
  };

  systemd.services.vaultwarden = {
    after = ["postgresql.service"];
    requires = ["postgresql.service"];
  };
}
