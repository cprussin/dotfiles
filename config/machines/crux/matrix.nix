{
  pkgs,
  config,
  lib,
  ...
}: let
  passwords = pkgs.callPackage ../../../lib/passwords.nix {};
  network = pkgs.callPackage ../../../lib/network.nix {};
  max_upload_size = "200M";
  federation_port_external = 8448;
  federation_port_internal = 8008;
  client_port_internal = 8009;

  mautrix_settings = port: {
    appservice = {
      inherit port;
      address = "http://localhost:${toString port}";
    };
    homeserver = {
      address = "http://localhost:${toString client_port_internal}";
      domain = "prussin.net";
    };
    bridge = {
      permissions = {
        "prussin.net" = "full";
        "@connor:prussin.net" = "admin";
      };
      login_shared_secret_map."prussin.net" = "$SHARED_SECRET";
      encryption = {
        allow = true;
        default = true;
      };
    };
  };
in {
  deployment.keys = {
    "matrix.internal.prussin.net.crt" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/matrix.internal.prussin.net/cert";
      user = config.users.users.nginx.name;
    };
    "matrix.internal.prussin.net.key" = {
      inherit (config.users.users.nginx) group;
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/ssl/matrix.internal.prussin.net/key";
      user = config.users.users.nginx.name;
    };
    "matrix-synapse-database-config.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMatrixSynapseDatabaseConfigFile "Connor/Infrastructure/matrix/matrix-synapse-database";
    };
    "matrix-synapse-shared-secret-config.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMatrixSynapseSharedSecretConfigFile "Connor/Infrastructure/matrix/matrix-synapse-shared-secret";
    };
    matrix-synapse-signing-key = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getFullPassword "Connor/Infrastructure/matrix/signing-keys/prussin.net";
    };
  };

  security.acme = {
    defaults.email = "admin@prussin.net";
    acceptTerms = true;
  };

  services = {
    postgresql.enable = true;
    nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."matrix.prussin.net" = {
        forceSSL = true;
        enableACME = true;
        http2 = true;
        listen = [
          # Acme only
          {
            addr = "0.0.0.0";
            port = 80;
          }
          {
            addr = "[::]";
            port = 80;
          }
          # Matrix Federation Server
          {
            addr = "0.0.0.0";
            port = federation_port_external;
            ssl = true;
          }
          {
            addr = "[::]";
            port = federation_port_external;
            ssl = true;
          }
        ];
        locations."/" = {
          proxyPass = "http://localhost:${toString federation_port_internal}";
          extraConfig = "client_max_body_size ${max_upload_size};";
        };
      };
      virtualHosts."matrix.internal.prussin.net" = {
        listenAddresses = ["[${network.wireguard6.crux.address}]" "${network.wireguard4.crux.address}"];
        sslCertificate = "/run/keys/matrix.internal.prussin.net.crt";
        sslCertificateKey = "/run/keys/matrix.internal.prussin.net.key";
        forceSSL = true;
        http2 = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString client_port_internal}";
          extraConfig = "client_max_body_size ${max_upload_size};";
        };
      };
    };

    matrix-synapse = {
      enable = true;

      settings = {
        inherit max_upload_size;
        server_name = "prussin.net";
        signing_key_path = config.deployment.keys.matrix-synapse-signing-key.path;
        suppress_key_server_warning = true;

        listeners = [
          {
            port = federation_port_internal;
            bind_addresses = ["127.0.0.1"];
            tls = false;
            x_forwarded = true;
            resources = [
              {
                names = ["federation"];
                compress = false;
              }
            ];
          }
          {
            port = client_port_internal;
            bind_addresses = ["127.0.0.1"];
            tls = false;
            x_forwarded = true;
            resources = [
              {
                names = ["client"];
                compress = true;
              }
            ];
          }
        ];
      };

      plugins = [
        config.services.matrix-synapse.package.plugins.matrix-synapse-shared-secret-auth
      ];

      extraConfigFiles = [
        config.deployment.keys."matrix-synapse-database-config.yaml".path
        config.deployment.keys."matrix-synapse-shared-secret-config.yaml".path
      ];
    };
  };

  systemd.services = {
    # For acme to work correctly
    nginx = {
      requires = [
        "import-tank.service"
        "matrix.internal.prussin.net.crt-key.service"
        "matrix.internal.prussin.net.key-key.service"
      ];
      after = [
        "import-tank.service"
        "matrix.internal.prussin.net.crt-key.service"
        "matrix.internal.prussin.net.key-key.service"
      ];
    };
    postgresql = {
      requires = ["import-tank.service"];
      after = ["import-tank.service"];
    };
    matrix-synapse = {
      after = [
        "matrix-synapse-signing-key-key.service"
        "matrix-synapse-database-config.yaml-key.service"
        "matrix-synapse-shared-secret-config.yaml-key.service"
      ];
      requires = [
        "matrix-synapse-signing-key-key.service"
        "matrix-synapse-database-config.yaml-key.service"
        "matrix-synapse-shared-secret-config.yaml-key.service"
      ];
      serviceConfig.ExecStartPre = lib.mkForce [];
    };
  };

  users.users.matrix-synapse.extraGroups = ["keys"];

  # Port 80 is open only for ACME challenges
  networking.firewall.interfaces."${config.interfaces.eth}".allowedTCPPorts = [80 federation_port_external];
  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [80 443];
}
