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
  mautrix-telegram-port = 29317;
  mautrix-signal-port = 29328;

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
    mautrix-telegram-environment-file = {
      user = "mautrix-telegram";
      group = "mautrix-telegram";
      keyCommand = passwords.getMautrixTelegramEnvironmentFile "Connor/Infrastructure/matrix/bridges/telegram" "Connor/Infrastructure/matrix/matrix-synapse-shared-secret";
    };
    "mautrix-telegram-registration-file.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixRegistrationFile "telegram" mautrix-telegram-port;
    };
    mautrix-signal-environment-file = {
      user = "mautrix-signal";
      group = "mautrix-signal";
      keyCommand = passwords.getMautrixSignalEnvironmentFile "Connor/Infrastructure/matrix/bridges/signal" "Connor/Infrastructure/matrix/matrix-synapse-shared-secret";
    };
    "mautrix-signal-registration-file.yaml" = {
      user = "matrix-synapse";
      group = "matrix-synapse";
      keyCommand = passwords.getMautrixRegistrationFile "signal" mautrix-signal-port;
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
        listenAddresses = [network.wireguard.nodes.crux.address];
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

        app_service_config_files = [
          config.deployment.keys."mautrix-telegram-registration-file.yaml".path
          config.deployment.keys."mautrix-signal-registration-file.yaml".path
        ];
      };

      plugins = [
        (config.services.matrix-synapse.package.plugins.matrix-synapse-shared-secret-auth.overrideAttrs (_: {
          version = "2.0.1";
          src = pkgs.fetchFromGitHub {
            owner = "devture";
            repo = "matrix-synapse-shared-secret-auth";
            rev = "2.0.1";
            sha256 = "0cbpj6npbnda23qrp7z5l33c95sh5mh21m9sc32xxiqaikj29ali";
          };
          buildInputs = [config.services.matrix-synapse.package];
        }))
      ];

      extraConfigFiles = [
        config.deployment.keys."matrix-synapse-database-config.yaml".path
        config.deployment.keys."matrix-synapse-shared-secret-config.yaml".path
      ];
    };

    mautrix-telegram = {
      enable = true;
      environmentFile = config.deployment.keys.mautrix-telegram-environment-file.path;
      settings = lib.recursiveUpdate (mautrix_settings mautrix-telegram-port) {
        bridge = {
          sync_create_limit = 0;
          sync_direct_chats = true;
          backfill.initial_limit = -1;
        };
      };
    };

    mautrix-signal = {
      enable = true;
      environmentFile = config.deployment.keys.mautrix-signal-environment-file.path;
      settings = mautrix_settings mautrix-signal-port;
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
        "mautrix-telegram-registration-file.yaml-key.service"
        "mautrix-signal-registration-file.yaml-key.service"
      ];
      requires = [
        "matrix-synapse-signing-key-key.service"
        "matrix-synapse-database-config.yaml-key.service"
        "matrix-synapse-shared-secret-config.yaml-key.service"
        "mautrix-telegram-registration-file.yaml-key.service"
        "mautrix-signal-registration-file.yaml-key.service"
      ];
      serviceConfig.ExecStartPre = lib.mkForce [];
    };
    mautrix-telegram = {
      after = ["postgresql.service" "mautrix-telegram-environment-file-key.service"];
      requires = ["postgresql.service" "mautrix-telegram-environment-file-key.service"];
      serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "mautrix-telegram";
        Group = "mautrix-telegram";
      };
    };
    mautrix-signal = {
      after = ["postgresql.service" "mautrix-signal-environment-file-key.service"];
      requires = ["postgresql.service" "mautrix-signal-environment-file-key.service"];
      serviceConfig = {
        DynamicUser = lib.mkForce false;
        User = "mautrix-signal";
        Group = "mautrix-signal";
      };
    };
  };

  # see https://github.com/NixOS/nixpkgs/blob/nixos-21.11/nixos/modules/misc/ids.nix
  ids = {
    uids = {
      mautrix-telegram = 350;
      mautrix-signal = 351;
    };
    gids = {
      mautrix-telegram = 350;
      mautrix-signal = 351;
    };
  };

  users = {
    groups = {
      mautrix-telegram.gid = config.ids.gids.mautrix-telegram;
      mautrix-signal.gid = config.ids.gids.mautrix-signal;
    };
    users = {
      matrix-synapse.extraGroups = ["keys"];
      mautrix-telegram = {
        group = "mautrix-telegram";
        home = "/var/lib/mautrix-telegram";
        createHome = true;
        shell = "${pkgs.bash}/bin/bash";
        uid = config.ids.uids.mautrix-telegram;
        extraGroups = ["keys"];
      };
      mautrix-signal = {
        group = "mautrix-signal";
        home = "/var/lib/mautrix-signal";
        createHome = true;
        shell = "${pkgs.bash}/bin/bash";
        uid = config.ids.uids.mautrix-signal;
        extraGroups = ["keys" "signald"];
      };
    };
  };

  # Port 80 is open only for ACME challenges
  networking.firewall.interfaces."${config.interfaces.eth}".allowedTCPPorts = [80 federation_port_external];
  networking.firewall.interfaces.prussinnet.allowedTCPPorts = [80 443];
}
